%% We should do our own tracking of cookies since httpc's cookies are
%% associated with a profile, and so we generate a new atom for each
%% profile, which seems ugly and heavy to me.
%%
%% @private
-module(mechanize_worker).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2]).
-behaviour(gen_server).

-include("mechanizerl.hrl").
-include("mechanize_internal_types.hrl").
-include_lib("xmerl/include/xmerl.hrl").

start_link(Profile) ->
    gen_server:start_link(?MODULE, Profile, []).

init(Profile) ->
    case inets:start(httpc, [{profile,Profile}]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    httpc:set_options([{cookies, enabled}], Profile),
    {ok, #state{profile=Profile}}.

terminate(_Reason, #state{profile=Profile}) ->
    inets:stop(httpc, Profile),
    ok.

base_url(Url) ->
    {ok, {Scheme, UserInfo, Host, Port, Path, _}} = http_uri:parse(Url),
    atom_to_list(Scheme) ++ "://" ++ UserInfo ++ Host ++ ":" ++ integer_to_list(Port) ++ Path.
rebase_url(Url, Path) ->
    {ok, {Scheme, UserInfo, Host, Port, _, _}} = http_uri:parse(Url),
    atom_to_list(Scheme) ++ "://" ++ UserInfo ++ Host ++ ":" ++ integer_to_list(Port) ++ Path.    
expand_relative_url(Url, #state{history=[]}) -> Url;
expand_relative_url(Path, #state{history=[{#request{url=Url},_}|_]}) ->
    case Path of
        "http://"++_ -> Path;
        "https://"++_ -> Path;
        [$/ | _] -> rebase_url(Url, Path);
        _ -> base_url(Url) ++ [$/ | Path]
    end.

make_request(Request, State) ->
    make_request(Request, State, 0).
make_request(_, _, 7) ->
    {error, {max_redirect_count_reached}};
make_request(Request=#request{url=Url,content_type=Content_Type}, State=#state{}, RedirectCount) ->
    ReqTuple = case Content_Type of
                   undefined -> {Url, Request#request.headers};
                   _ -> {Url, Request#request.headers, Content_Type, Request#request.body}
               end,
    {ok, {{_Version, Status, _Reason}, Headers, Body}} = httpc:request(Request#request.method, ReqTuple, [], [], State#state.profile),
    ProcessedHeaders = [{list_to_atom(string:to_lower(H)),V} || {H,V} <- Headers],
    case proplists:get_value(location, ProcessedHeaders) of
        Location when undefined =/= Location andalso Status >= 300 andalso Status < 400 ->
            make_request(#request{url=expand_relative_url(Location,State),headers=[],method=get}, State, RedirectCount+1);
        _ -> {Request,#response{status=Status, headers=ProcessedHeaders, body=Body}}
    end.

parse_content_type(Headers) ->
    case proplists:get_value('content-type', Headers) of
        undefined -> undefined;
        CT ->
            case string:tokens(CT, "/;,") of
                [Major,Minor | _] -> {Major,Minor}
            end
    end.

parse_html(Body, State=#state{history=[{#request{url=Url},_}|_]}) ->
    {HTML=#xmlElement{name=html,content=Content}, []} = xmerl_scan:string(Body),
    Forms = parse_forms(HTML, Url),
    State#state{xml=HTML,
                text=extract_text(Content),
                links=parse_links(HTML),
                forms=Forms,
                current_form=case Forms of
                                 [] -> undefined;
                                 _ -> 1
                             end,
                title=read_title(HTML)}.

read_title(HTML) ->
    extract_text(xmerl_xpath:string("/html/head/title/text()", HTML)).

extract_url(Element) ->
    case xmerl_xpath:string("./@href|./@src|./@action", Element) of
        [#xmlAttribute{value=V}] -> V;
        [] -> undefined
    end.

parse_links(HTML) ->
    Hrefs = xmerl_xpath:string("//*[@href]|//*[@src]", HTML),
    [#link{type=L#xmlElement.name, url=extract_url(L), text=extract_text(L)} || L <- Hrefs].

parse_forms(HTML, Url) ->
    Forms = xmerl_xpath:string("//form", HTML),
    [parse_form(Form, Url) || Form <- Forms].

proplist_from_attributes([]) -> [];
proplist_from_attributes([#xmlAttribute{name=K,value=V} | Attrs]) ->
    [{K,V} | proplist_from_attributes(Attrs)].

http_verb_as_atom(V) -> list_to_atom(string:to_lower(V)).

parse_form(Form=#xmlElement{name=form, attributes=Attrs}, Url) ->
    Props = proplist_from_attributes(Attrs),
    #form{action=proplists:get_value(action, Props, Url),
          method=http_verb_as_atom(proplists:get_value(method, Props, "GET")),
          name=proplists:get_value(name, Props),
          id=proplists:get_value(id, Props),
          content_type=proplists:get_value('content-type', Props, "application/x-www-form-urlencoded"),
          fields=parse_fields(Form)}.
parse_fields(Form) ->
    [parse_field(F) || F <- xmerl_xpath:string("//input|//select|//textarea|//button", Form)].
parse_field(E=#xmlElement{name=select, attributes=Attrs, content=Content}) ->
    Props = proplist_from_attributes(Attrs),
    {AllOptions,SelectedOptions} = parse_options(xmerl_xpath:string("./option|./optgroup/option", E), [], []),
    field_from_props(select, Props, Content, SelectedOptions, AllOptions);
parse_field(#xmlElement{name=Name, attributes=Attrs, content=Content}) ->
    Props = proplist_from_attributes(Attrs),
    field_from_props(Name, Props, Content).

parse_options([], Values, Selected) -> {Values, Selected};
parse_options([E|Es], Values, Selected) ->
    {V,S} = parse_option(E),
    parse_options(Es, [V|Values], case S of true -> [V|Selected]; false -> Selected end).
parse_option(#xmlElement{name=option, attributes=Attrs, content=Content}) ->
    Props = proplist_from_attributes(Attrs),
    Value = proplists:get_value(value, Props, extract_text(Content)),
    {Value,undefined =/= proplists:get_value(selected, Props)}.

field_from_props(Tag, Props, Content) ->
    Type = case {Tag, proplists:get_value(type, Props)} of
               {select,_} -> select;
               {input,undefined} -> text;
               {button,undefined} -> submit;
               {_,S} when is_list(S) -> list_to_atom(S);
               {A,undefined} -> A
           end,
    Value = case Type of
                radio -> case proplists:get_value(checked, Props, false) of
                             false -> undefined;
                             _ -> proplists:get_value(value, Props)
                         end;
                checkbox -> case proplists:get_value(checked, Props, false) of
                             false -> undefined;
                             _ -> proplists:get_value(value, Props)
                         end;
                _ -> proplists:get_value(value, Props)
            end,
    field_from_props(Type, Props, Content, Value).
field_from_props(select, Props, Content, Value, Options) ->
    F = field_from_props(select, Props, Content, Value),
    F#field{options=Options}.
field_from_props(Type, Props, Content, Value) ->
    #field{name=case proplists:get_value(name, Props) of
                    undefined -> undefined;
                    S -> list_to_atom(S)
                end,
           type=Type,
           attributes=Props,
           value=Value,
           content=Content}.

extract_text(Content) ->
    lists:flatten(lists:reverse(extract_text(Content, []))).
extract_text([], Text) -> Text;
extract_text([Element|Elements], Text) ->
    extract_text(Elements, extract_text(Element, Text));
extract_text(#xmlElement{content=Content}, Text) ->
    extract_text(Content, Text);
extract_text(#xmlText{value=Value}, Text) -> [Value|Text];
extract_text(#xmlPI{}, Text) -> Text;
extract_text(#xmlComment{}, Text) -> Text;
extract_text(#xmlDecl{}, Text) -> Text.

update_for_response({Request, Response=#response{headers=Headers}}, #state{profile=Profile,history=History}) ->
    %% Note that we blow away everything but history and profile here,
    %% intentionally; keep that in mind if the state record is
    %% extended.
    ContentType = parse_content_type(Headers),
    State = #state{history=[{Request, Response} | History],
                   profile=Profile,
                   content_type=ContentType},
    case {ContentType,length(Response#response.body)} of
        {{"text", "html"}, X} when X > 0 -> parse_html(Response#response.body, State);
        _ -> State
    end.

return_ok_on_success(#response{status=Status}, State) ->
    {if Status >= 200 andalso Status < 400 -> ok; true -> {error, Status} end, State}.

handle_http_method_call(Request, State) ->
    {ReqOut,Response} = make_request(Request, State),
    {Reply, NewState} = return_ok_on_success(Response, update_for_response({ReqOut, Response}, State)),
    {reply, Reply, NewState}.
handle_http_method_call(Method, Url, State) ->
    handle_http_method_call(#request{method=Method,
                                     url=expand_relative_url(Url, State)},
                            State).
handle_http_method_call(Method, Url, Content_Type, Body, State) ->    
    handle_http_method_call(#request{method=Method,
                                     url=expand_relative_url(Url, State),
                                     content_type=Content_Type,
                                     body=Body},
                           State).

%% @todo Really, we should use dictionaries for forms and fields to
%% avoid all these wasteful maps.
get_form(I, Forms) ->
    try lists:nth(I, Forms) of
        Form=#form{} -> Form
    catch
        error:_ -> undefined
    end.

-spec(get_field(atom(), [#field{}]) -> undefined|#field{}|[#field{}]).
get_field(Name, Fields) ->
    case get_fields(Name, Fields) of
        [] -> undefined;
        [F=#field{}] -> F;
        Fs -> Fs
    end.
-spec(get_fields(atom(), [#field{}]) -> [#field{}]).
get_fields(Name, Fields) ->
    [F || F <- Fields, #field{name=Name} =:= F].

set_field(Values, Field=#field{type=checkbox, attributes=Attr}) ->
    Value = proplists:get_value(value, Attr),
    case lists:member(Value, Values) of
        true -> Field#field{value=Value};
        false -> Field#field{value=undefined}
    end;
set_field(Values, Field=#field{type=radio, attributes=Attr}) ->
    Value = proplists:get_value(value, Attr),
    case lists:member(Value, Values) of
        true -> Field#field{value=Value};
        false -> Field#field{value=undefined}
    end;
set_field(Values, Field=#field{type=select, options=Opts}) ->
    Field#field{value=[V || V <- Values, lists:member(V,Opts)]};
set_field(Values, Field=#field{}) ->
    Field#field{value=Values}.
set_field(Name, Values, Form=#form{fields=Fields}) ->
    Form#form{fields=[case F#field.name of
                          Name -> set_field(Values, F);
                          _ -> F
                      end || F <- Fields]};
set_field(Name, Values, State=#state{forms=Forms, current_form=I}) ->
    case get_form(I, Forms) of
        Form=#form{} ->
            case set_field(Name, Values, Form) of
                undefined -> undefined;
                NewForm ->
                    State#state{forms=[case J of
                                           I -> NewForm;
                                           _ -> F
                                       end || {J,F} <- mechanize_util:enumerate(Forms)]}
            end;
        undefined -> undefined
    end.

successful_controls(#form{fields=Fields}) ->
    lists:flatten([case F#field.type of
                       select -> [{F#field.name, V} || V <- F#field.value];
                       _ -> {F#field.name,F#field.value}
                   end || F <- Fields, F#field.name =/= undefined, F#field.value =/= undefined,
                          not lists:member(F#field.type, [button,submit,reset,image])]).

submit_form(Form=#form{action=Action, method=Method}, SubmissionField, State) ->
    Successful = successful_controls(Form),
    Fields = case SubmissionField of
                 #field{name=Name,value=Value} when Name =/= undefined -> [{Name,Value} | Successful];
                 _ -> Successful
             end,
    %% @todo Should handle various content-types.
    Data = encode_form(Fields),
    case Method of
        get -> handle_http_method_call(get, Action ++ "?" ++ Data, State);
        post -> handle_http_method_call(post, Action, "application/x-www-form-encoded", Data, State)
    end.

encode_form(Fields) ->
    lists:flatten(string:join([http_uri:encode(atom_to_list(K))++"="++http_uri:encode(V)
                               || {K,V} <- Fields], "&")).

find_field_of_type(_, []) ->
    undefined;
find_field_of_type(Type, [F=#field{type=Type}|_]) -> F;
find_field_of_type(Type, [#field{}|Fields]) -> find_field_of_type(Type, Fields).



%%% PAGE-FETCHING METHODS
handle_call({get,Url}, _From, State) -> handle_http_method_call(get, Url, State);
handle_call({head,Url}, _From, State) -> handle_http_method_call(head, Url, State);
handle_call({delete,Url}, _From, State) -> handle_http_method_call(delete, Url, State);
handle_call({put,Url,Content_Type,Data}, _From, State) ->
    handle_http_method_call(put, Url, Content_Type, Data, State);
handle_call({post,Url,Content_Type,Data}, _From, State) ->
    handle_http_method_call(post, Url, Content_Type, Data, State);
handle_call(reload, _From, State=#state{history=[{Req,_}|Rest]}) ->
    handle_http_method_call(Req, State#state{history=Rest});
handle_call(back, _From, State=#state{history=[{_,_},{Req,Res}|Rest]}) ->
    {reply, ok, update_for_response({Req,Res},State#state{history=Rest})};

%%% RESPONSE METHODS

handle_call(base_url, _From, State=#state{history=[{#request{url=Url},_}|_]}) ->
    {reply, base_url(Url), State};
handle_call(request, _From, State=#state{history=[{Req=#request{},_}|_]}) ->
    {reply, Req, State};
handle_call(response, _From, State=#state{history=[{_,Res=#response{}}|_]}) ->
    {reply, Res, State};
handle_call(content_type, _From, State=#state{content_type=ContentType}) ->
    {reply, ContentType, State};

handle_call(forms, _From, State=#state{forms=Forms}) ->
    {reply, Forms, State};
handle_call(links, _From, State=#state{links=Links}) ->
    {reply, Links, State};

handle_call(title, _From, State=#state{title=Title}) ->
    {reply, Title, State};

handle_call(as_xml, _From, State=#state{xml=Xml}) ->
    {reply, Xml, State};
handle_call(as_text, _From, State=#state{text=Text}) ->
    {reply, Text, State};

%%% FORM METHODS

handle_call(current_form, _From, State=#state{current_form=undefined}) ->
    {reply, undefined, State};
handle_call(current_form, _From, State=#state{forms=Forms,current_form=I}) ->
    {reply, lists:nth(I, Forms), State};
handle_call({set_current_form,I}, _From, State=#state{forms=Forms}) ->
    case get_form(I, Forms) of
        #form{} -> {reply, ok, State#state{current_form=I}};
        undefined -> {reply, undefined, State}
    end;
handle_call({field,Name}, _From, State=#state{forms=Forms,current_form=I}) ->
    case {form,get_form(I, Forms)} of
        {form,#form{fields=Fields}} ->
            case {field,get_field(Name, Fields)} of
                {field,#field{value=Value}} ->
                    {reply, Value, State};
                {field,undefined} ->
                    {reply, undefined, State}
            end;
        {form,undefined} -> {reply, undefined, State}
    end;
    
handle_call({field,Name,Values=[_]}, _From, State=#state{}) ->
    case set_field(Name, Values, State) of
        NewState=#state{} ->
            {reply, ok, NewState};
        undefined ->
            {reply, undefined, State}
    end;
handle_call({field,Name,Values=[Value|_]}, _From, State=#state{}) when is_list(Value) ->
    case set_field(Name, Values, State) of
        NewState=#state{} ->
            {reply, ok, NewState};
        undefined ->
            {reply, undefined, State}
    end;
handle_call({field,Name,Value}, _From, State=#state{}) ->
    case set_field(Name, [Value], State) of
        NewState=#state{} ->
            {reply, ok, NewState};
        undefined ->
            {reply, undefined, State}
    end;
handle_call({click,Name}, _From, State=#state{forms=Forms,current_form=I}) ->
    case {form,get_form(I, Forms)} of
        {form,Form=#form{fields=Fields}} ->
            case {field,get_field(Name, Fields)} of
                {field,Field=#field{type=Type}} ->
                    case Type of
                        submit -> {Reply,NewState} = submit_form(Form, Field, State),
                                  {reply,Reply,NewState};
                        _ -> {reply,{error,not_implemented},State}
                    end;
                {field,undefined} -> {reply,undefined,State}
            end;
        {form,undefined} -> {reply,undefined,State}
    end;
handle_call(submit, _From, State=#state{forms=Forms,current_form=I}) ->
    case {form,get_form(I, Forms)} of
        {form,Form=#form{fields=Fields}} ->
            submit_form(Form, find_field_of_type(submit, Fields), State);
        {form,undefined} -> {reply,undefined,State}
    end;

%% Anything left is probably a fall-through from a method that
%% expected history.
handle_call(_, _, State=#state{history=[]}) -> {reply, undefined, State}.

handle_cast(stop, State) -> {stop, normal, State}.
