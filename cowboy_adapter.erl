-module(cowboy_adapter).
-compile(export_all).

start_link([{port,Port},{handler,F}]) ->
    ok = application:start(cowboy),
    Dispatch = [{'_', [{'_', ?MODULE, F}]}],  %% server is the name of this module
    NumberOfAcceptors = 100,
    cowboy:start_listener(my_named_thing,
			  NumberOfAcceptors,
			  cowboy_tcp_transport, [{port, Port}],
			  cowboy_http_protocol, [{dispatch, Dispatch}]),
    ok.


init({tcp, http}, Req, Opts) -> {ok, Req, Opts}.

terminate(_, _) ->  ok.

handle(Req, F) ->
    case (catch F({?MODULE, Req, F})) of
	{'EXIT', Why} ->
	    io:format("EXIT:~p~n",[Why]);
	X ->
	    X
    end.

json_to_erl([{Str,[]}], _) ->
    mochijson2:decode(Str).

send_data(Type, Data, {_,Req, F}) ->
    %% warning Type ignored ... fix me ...
    {ok, Req1} = cowboy_http_req:reply(200, [], Data, Req),
    {ok, Req1, F}.

send_file(File, {_, Req, F}) ->
    %% should check the mime type here ... do later
    case filename:extension(File) of
	".ehe" ->
	    {ok, Bin} = file:read_file(File),
	    {Data, Bs} = ehe:expand_binary(Bin, [{'Req', {?MODULE,Req, F}}]),
	    {ok, Req1} = cowboy_http_req:reply(200, [], Data, Req),
	    {ok, Req1, F};
	Ext ->
	    {ok, Bin} = file:read_file(File),
	    {ok, Req1} = cowboy_http_req:reply(200, [], Bin, Req),
	    {ok, Req1, F}
    end.

magic(_) ->
    "Hello from the cowboy adapter".

	
get(path, {_, Req, _}) ->
    {Path, _} = cowboy_http_req:path(Req),
    [binary_to_list(I) || I <- Path];
get(args, {_, Req, _}) ->
    {Args, _} = cowboy_http_req:qs_vals(Req),
    Args1 = [{cvt(K),cvt(V)} || {K,V} <- Args],
    Args1.

cvt(X) when is_binary(X) ->  binary_to_list(X);
cvt(true) ->  [].

reply_json(Obj, {_,Req, F}) ->
    Json = mochijson2:encode(Obj),
    {ok, Req1} = cowboy_http_req:reply(200, [], Json, Req),
    {ok, Req1, F}.
	    
header(X) ->
    {"Content-Type", mime_type(X)}.

mime_type(gif)     ->  "image/gif";
mime_type(jpg)     -> "image/jpeg";
mime_type(png)     -> "image/png";
mime_type(css)     -> "text/css";
mime_type(special) -> "text/plain; charset=x-user-defined";
mime_type(json)    -> "application/json";
mime_type(swf)     -> "application/x-shockwave-flash";
mime_type(html)    -> "text/html";
mime_type(xul)     -> "application/vnd.mozilla.xul+xml";
mime_type(js)      -> "application/x-javascript";
mime_type(svg)     -> "image/svg+xml".

pre(X, _) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T]) -> [H|quote(T)];
quote([]) -> [].

