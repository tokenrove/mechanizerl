
-module(esi_callbacks).
-export([echo/3]).

echo(SessionId, Env, Input) ->
    Qs = httpd:parse_query(case proplists:get_value(request_method, Env) of
                               "GET" -> proplists:get_value(query_string, Env, []);
                               "POST" -> Input
                           end),
    mod_esi:deliver(SessionId, "Content-Type: text/html\r\n\r\n"),
    mod_esi:deliver(SessionId, "<html><body><table><tr><th>Name</th><th>Value</th></tr>"),
    lists:foreach(fun ({K,V}) -> mod_esi:deliver(SessionId, io_lib:format("<tr><td>~s</td><td>~s</td></tr>", [K,V])) end,
                  Qs),
    mod_esi:deliver(SessionId, "</table></body></html>").

login(SessionId, Env, Input) ->
    Qs = httpd:parse_query(Input),
    case {proplists:get_value(username, Env), proplists:get_vaue(password, Env)} of
        {"valid","correct"} ->
            mod_esi:deliver(SessionId, "Set-Cookie: ...\r\n\r\nSuccess.");
        _ -> mod_esi:deliver(SessionId, "Set-Cookie: ...\r\n\r\nFailed.")
    end.

auth_required(SessionId, Env, Input) ->
    case lists:keyfind("cookie", 1, Env) of
        {"cookie","authorized=true"} -> mod_esi:deliver(SessionId, "Okay.");
        _ -> mod_esi:deliver(SessionId, "Failed.")
    end.
