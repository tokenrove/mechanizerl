%% @doc Tests mechanizerl against a local HTTP server.
-module(local_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("mechanizerl/include/mechanizerl.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).

groups() ->
    [{groups,
      [{trivial, [shuffle], [basic_behavior]},
       {html, [shuffle], []},
       {links, [shuffle], []},
       {forms, [shuffle], [basic_forms]},
       {rest, [shuffle], []},
       {cookies, [shuffle], []}]
     }].

all() ->
    [{group,groups}].

init_per_suite(Config) ->
    mechanizerl:start(),
    DataDir = ?config(data_dir, Config),
    {ok, Pid} = inets:start(httpd, [{port,0},
                                    {server_name,"localhost"},
                                    {mime_type,"application/octet-stream"},
                                    {server_root,DataDir},
                                    {document_root,filename:join([DataDir, "www"])},
                                    {modules,[mod_alias,mod_esi,mod_head,mod_get]},
                                    {erl_script_alias, {"/erl", [esi_callbacks]}},
                                    {directory_index,["index.html"]}]),
    [{port, Port}] = httpd:info(Pid, [port]),
    [{httpd_port, Port}, {httpd_pid, Pid} | Config].

end_per_suite(Config) ->
    inets:stop(httpd, ?config(httpd_pid, Config)),
    ok.

local_url(Config) ->
    "http://localhost:"++integer_to_list(?config(httpd_port, Config))++"/".

verify_simple_index_page(M) ->
    200 = M:status(),
    true = length(M:links()) > 3,
    Stylesheet = M:find_link({url,".*\\.css"}),
    "/css/style.css" = Stylesheet#link.url,
    ErlangLink = M:find_link({text,"erl"}),
    "http://www.erlang.org/" = ErlangLink#link.url,
    true = M:base() =:= M:uri(),
    {match,_} = re:run(M:title(), "Mechanizerl test root"),
    {match,_} = re:run(M:text(), "This\\s+is\\s+a\\s+test\\s+of\\s+mechanizerl\\."),
    [] = M:forms(),
    undefined = M:current_form(),
    ok.

verify_simple_next_page(M) ->
    200 = M:status(),
    {match,_} = re:run(M:base(), "/next.html$"),
    true = M:base() =:= M:uri(),
    {match,_} = re:run(M:title(), "Next page"),
    {match,_} = re:run(M:text(), "Not much here"),
    [] = M:links(),
    [] = M:forms(),
    ok.

basic_behavior(Config) ->
    M = mechanizerl:new(),
    BaseUrl = local_url(Config),
    undefined = M:back(),
    ok = M:get(BaseUrl),
    BaseUrl = M:base(),
    ok = verify_simple_index_page(M),
    ok = M:follow_link({text, "Next"}),
    ok = verify_simple_next_page(M),
    ok = M:back(),
    ok = verify_simple_index_page(M),
    M:get("next.html"),
    ok = verify_simple_next_page(M).    

fill_simple_form(M) ->
    ok = M:set_fields([{implicit_type, "42"},
                       {email, "foo@bar.com"},
                       {checkboxen, ["2","3"]},
                       {radio, "the"},
                       {single_select, "implicit value"},
                       {multiple_select, ["implicit value","another_explicit"]}]),
    ok = M:submit(),
    Results = [R#xmlText.value || R <- xmerl_xpath:string("//table/tr/td/text()", M:body_xml())],
    io:format(standard_error, "DEBUG: ~p~n", [Results]),
    ["implicit_type","42",
     "email","foo@bar.com",
     "checkboxen","2",
     "hidden","secret",
     "checkboxen","3",
     "radio","the",
     "single_select","implicit value",
     "multiple_select","implicit value",
     "multiple_select","another_explicit"
    ] = Results,
    ok.

basic_forms(Config) ->
    M = mechanizerl:new(),
    BaseUrl = local_url(Config),
    ok = M:get(BaseUrl),
    ok = M:follow_link({text, "Basic forms"}),
    undefined = M:form_with_fields([email,no_such_field]),
    Form = M:form_number(1),
    Form = M:form_with_fields([email,hidden,radio]),
    Form = M:form_name("simple-with-get"),
    Action = Form#form.action,
    ok = fill_simple_form(M),
    {match,_} = re:run(M:base(), Action++"$"),
    {match,_} = re:run(M:uri(), Action++"\\?.+$"),
    M:back(),
    Form2 = M:form_name("simple-with-post"),
    Form2 = M:form_id("simple"),
    Action2 = Form2#form.action,
    ok = fill_simple_form(M),
    {match,_} = re:run(M:base(), Action2++"$"),
    true = M:base() =:= M:uri(),
    M:back(),
    M:form_name("pre-filled"),
    ok = M:submit(),
    ["implicit_type","implicit",
     "email","an.email@example.moc",
     "checkboxen","2",
     "hidden","secret",
     "checkboxen","4",
     "radio","or",
     "single_select","explicit",
     "multiple_select","explicit",
     "multiple_select","implicit value"
    ] = [R#xmlText.value || R <- xmerl_xpath:string("//table/tr/td/text()", M:body_xml())].
