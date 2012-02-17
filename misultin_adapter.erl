-module(misultin_adapter).
-compile(export_all).

start_link([{port,Port},{handler,F}]) ->
    Z = misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req, F) end}]),
    ok.

handle_http(Req, F) ->
    case (catch F({?MODULE, Req})) of
	{'EXIT', Why} ->
	    io:format("EXIT:~p~n",[Why]);
	X ->
	    X
    end.

json_to_erl([{Str,[]}], _) ->
    mochijson2:decode(Str).

send_data(Type, Data, {_,Req}) ->
    Req:ok([header(Type)], Data).

send_file(File, {_, Req}) ->
    case filename:extension(File) of
	".ehe" ->
	    {ok, Bin} = file:read_file(File),
	    {Data, Bs} = ehe:expand_binary(Bin, [{'Req', {?MODULE,Req}}]),
	    Req:ok([header(html)], Data);
	Ext ->
	    {ok, Bin} = file:read_file(File),
	    Req:ok([header(classify_extension(Ext))], Bin)
    end.

magic(_) ->
    "Hello from the misultin adapter".

get(path, {_,Req}) ->
    Resource = Req:resource([lowercase, urldecode]);
get(args, {_,Req}) ->
    Req:parse_qs().

reply_json(Obj, {_,Req}) ->
    Json = mochijson2:encode(Obj),
    Req:ok([header(json)], Json).
	    
header(X) ->
    {"Content-Type", mime_type(X)}.

mime_type(gif)  ->  "image/gif";
mime_type(jpg) -> "image/jpeg";
mime_type(png) -> "image/png";
mime_type(css)  -> "text/css";
mime_type(special)  -> "text/plain; charset=x-user-defined";
mime_type(json)  -> "application/json";
mime_type(swf)  -> "application/x-shockwave-flash";
mime_type(html) -> "text/html";
mime_type(xul) -> "application/vnd.mozilla.xul+xml";
mime_type(js)   -> "application/x-javascript";
mime_type(svg)   -> "image/svg+xml".

pre(X, _) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T]) -> [H|quote(T)];
quote([]) -> [].

classify_extension(".GIF") -> gif;
classify_extension(".jpg") -> jpeg;
classify_extension(".js")  -> js;
classify_extension(".css") -> css;
classify_extension(_)      -> html.
