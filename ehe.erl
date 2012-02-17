-module(ehe).
-compile(export_all).
-import(lists, [reverse/1, reverse/2]).

expand_file(File, Bs) ->
    {ok, Bin} = file:read_file(File),
    expand_string(binary_to_list(Bin), Bs).

expand_binary(Str, Bs) ->
    expand_string(binary_to_list(Str), Bs).

expand_string(Str, Bs) ->
    B1 = make_bindings(Bs, erl_eval:new_bindings()),
    expand(Str, 0, B1, []).

make_bindings([{K,V}|T], B) -> make_bindings(T, erl_eval:add_binding(K,V,B));
make_bindings([], B)        -> B.

expand("<?e" ++ T, Ln, B0, L) ->
    {Str, Ln1, T1} = collect_form(T, Ln, []),
    {Value, B1} = string2value(Str, Ln, B0),
    expand(T1, Ln1, B1, reverse(Value, L));
expand([$\n|T], Ln, B, L) ->
    expand(T, Ln+1, B, [$\n|L]);
expand([H|T], Ln, B, L) ->
    expand(T, Ln, B, [H|L]);
expand([], _, B, L) ->
    {reverse(L), B}.

collect_form("?>" ++ T, Ln, L) -> {reverse(L), Ln, T};
collect_form([$\n|T], Ln, L)   -> collect_form(T, Ln+1, [$\n|L]);
collect_form([H|T], Ln, L)     -> collect_form(T, Ln, [H|L]);
collect_form([], Ln, L)        -> {reverse(L), Ln, []}.

string2value(Str, Ln, Bindings0) ->
    case erl_scan:string(Str ++ ".", Ln) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_exprs(Tokens) of
		{ok, Exprs} -> 
		    {value, Value, Bindings1} = eval(Exprs, Bindings0),
		    {Value, Bindings1};
		Other ->
		    io:format("cannot parse:~p Reason=~p~n",[Tokens,Other])
	    end;
	Other ->
	    io:format("cannot tokenise:~p Reason=~p~n",[Str,Other])
    end.

%% eval([{call,_,{remote,_,{atom,_,ehe},{atom,_,bindings}},[]}|T], B0) ->
%%     {B0, B0},
    

eval(Exprs, B0) ->
    %% io:format("ehe:~p~n",[Exprs]),
    case (catch erl_eval:exprs(Exprs, B0, {eval, fun local/3})) of
	{'EXIT', Why} ->
	    Error = pre({error,evaluating,Exprs,content,B0,was,Why}),
	    {value, Error, B0};
	{redirect, X} ->
	    throw({redirect,X});
	Other ->
	    Other
    end.

local(bindings, [], B0) ->
    {value, B0, B0};
local(pre, [Expr], B0) ->
    {value, Val, B1} = eval([Expr], B0),
    {value, pre(Val), B1};
local(show_bindings,[],B0) ->
    {value, pre(B0), B0};
local(redirect, [Expr], B0) ->
    {value, Val, B1} = eval([Expr], B0),
    io:format("Redirecting to:~p~n",[Val]),
    throw({redirect, Val});
local(Name, Args, B0) ->
    io:format("local handler called:~p~n",[{Name,Args,B0}]),
    {true, B0}.
    
pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].

require_arg(Key, [{Key,Val}]) -> Val;
require_arg(Key, [_|T])       -> require_arg(Key, T);
require_arg(Key, [])          -> io:format("Missign argument:~p~n",[Key]),
				 exit(ebadInput).

    

