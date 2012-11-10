%% @doc A tool for testing web applications.  Follows the
%% WWW::Mechanize API where convenient.  Most methods return undefined
%% if no request/response is available.
%%
%% @todo A good parse transform would reduce the code debt here.

-module(mechanizerl).
-author('Julian Squires <julian@cipht.net>').
-export([start/0, start/2, stop/0, stop/1]).
%% Constructor
-export([new/0]).
%% Page-fetching methods
-export([get/2,head/2,put/4,post/4,delete/2,reload/1,back/1]).
%% Status methods
-export([uri/1,response/1,status/1,content_type/1,base/1,forms/1,
         current_form/1,links/1,is_html/1,is_json/1,is_xml/1, title/1]).
%% Content-handling methods
-export([body/1,body_xml/1,text/1]).
%% Link methods
-export([follow_link/2,find_link/2,find_all_links/2,find_all_links/1]).
%% Form methods
-export([form_number/2,form_name/2,form_id/2,form_with_fields/2]).
%% Field methods
-export([field/3,set_fields/2,set_visible/2,value/2,click/2,submit/1,submit_form/3]).
-compile({no_auto_import,[get/1,put/2]}).
-behaviour(application).

-include("mechanizerl.hrl").
-include("mechanize_internal_types.hrl").

%%%% Public API

-type handle() :: {?MODULE, pid()}.
-type response_code() :: ok | {error,integer()}.

%% @doc Creates a new mechanizerl instance.  Use the return value as a
%% prefix to call other functions, as in:
%% ```
%% 1> M = mechanizerl:new().
%% {mechanizerl, <0.64.0>}
%% 2> M:get("http://localhost/").
%% ok
%% '''
-spec new() -> handle().
new() ->
    {ok, Pid} = mechanize_sup:start_child(),
    {?MODULE, Pid}.

%%% PAGE-FETCHING METHODS

%% @doc GETs a URL.  Returns ok if the result was 2XX or 3XX, or
%% {error,integer()} otherwise.
-spec get(url()|#link{}, handle()) -> response_code().
get(#link{url=URL}, {?MODULE,Pid}) -> gen_server:call(Pid, {get, URL});
get(URL, {?MODULE,Pid}) -> gen_server:call(Pid, {get, URL}).
%% @doc HEADs a URL.
-spec head(url()|#link{}, handle()) -> response_code().
head(#link{url=URL}, {?MODULE,Pid}) -> gen_server:call(Pid, {head, URL});
head(URL, {?MODULE,Pid}) -> gen_server:call(Pid, {head, URL}).
%% @doc Sends a PUT request to URL.
-spec put(url()|#link{}, string(), iodata(), handle()) -> response_code().
put(#link{url=URL}, Content_Type, Data, {?MODULE,Pid}) -> gen_server:call(Pid, {put, URL, Content_Type, Data});
put(URL, Content_Type, Data, {?MODULE,Pid}) -> gen_server:call(Pid, {put, URL, Content_Type, Data}).
%% @doc POSTs to URL.
-spec post(url()|#link{}, string(), iodata(), handle()) -> response_code().
post(#link{url=URL}, Content_Type, Data, {?MODULE,Pid}) -> gen_server:call(Pid, {post, URL, Content_Type, Data});
post(URL, Content_Type, Data, {?MODULE,Pid}) -> gen_server:call(Pid, {post, URL, Content_Type, Data}).
%% @doc DELETEs URL.
-spec delete(url()|#link{}, handle()) -> response_code().
delete(#link{url=URL}, {?MODULE,Pid}) -> gen_server:call(Pid, {get, URL});
delete(URL, {?MODULE,Pid}) -> gen_server:call(Pid, {get, URL}).
%% @doc Acts like a browser's reload button.  Returns undefined if
%% there is no current request.
-spec reload(handle()) -> undefined | response_code().
reload({?MODULE,Pid}) -> gen_server:call(Pid, reload).
%% @doc Acts like a browser's back button.  Returns undefined if there
%% is no further history.
-spec back(handle()) -> undefined | response_code().
back({?MODULE,Pid}) -> gen_server:call(Pid, back).

%%% STATUS METHODS

%% @doc Returns the current URI.
-spec uri(handle()) -> undefined | url().
uri({?MODULE,Pid}) ->
    case gen_server:call(Pid, request) of
        #request{url=Url} -> Url;
        _ -> undefined
    end.

%% @doc Returns the tuple {Status, Headers, Body} of the current
%% response.
-spec response(handle()) -> undefined | {integer(), [tuple()], string()}.
response({?MODULE,Pid}) ->
    case gen_server:call(Pid, response) of
        #response{status=Status,headers=Headers,body=Body} -> {Status,Headers,Body};
        _ -> undefined
    end.

%% @doc Returns the status code of the response.
-spec status(handle()) -> undefined | integer().
status(Handle) ->
    case response(Handle) of
        {Status,_,_} -> Status;
        undefined -> undefined
    end.

%% @doc Returns the content type of the response as a {Type,Subtype}
%% tuple.
-spec content_type(handle()) -> undefined | {string(),string()}.
content_type({?MODULE,Pid}) -> gen_server:call(Pid, content_type).

%% @doc Returns the base URI for the current response.
-spec base(handle()) -> undefined | url().
base({?MODULE,Pid}) -> gen_server:call(Pid, base_url).

%% @doc Returns a list of forms found in the response, if the response
%% was HTML.
-spec forms(handle()) -> undefined | [#form{}].
forms({?MODULE,Pid}) -> gen_server:call(Pid, forms).

%% @doc Returns the current form; defaults to the first form in the
%% response.
-spec current_form(handle()) -> undefined | #form{}.
current_form({?MODULE,Pid}) -> gen_server:call(Pid, current_form).

%% @doc Returns the links found in the response, if the response was HTML.
-spec links(handle()) -> undefined | [#link{}].
links({?MODULE,Pid}) -> gen_server:call(Pid, links).

%% @doc Returns whether the current response is HTML, based on content type.
-spec is_html(handle()) -> undefined | boolean().
is_html(Handle) ->
    case content_type(Handle) of
        undefined -> undefined;
        {"text", "html"} -> true;
        _ -> false
    end.

%% @doc Returns whether the current response is JSON, based on content type.
-spec is_json(handle()) -> undefined | boolean().
is_json(Handle) ->
    case content_type(Handle) of
        undefined -> undefined;
        {"application", "json"} -> true;
        _ -> false
    end.

%% @doc Returns whether the current response is XML, based on content type.
-spec is_xml(handle()) -> undefined | boolean().
is_xml(Handle) ->
    case content_type(Handle) of
        undefined -> undefined;
        {"application", "xml"} -> true;
        {"text", "xml"} -> true;
        _ -> false
    end.

%% @doc Returns the contents of the HTML title tag of the current response.
-spec title(handle()) -> undefined | string().
title({?MODULE,Pid}) -> gen_server:call(Pid, title).

%%% CONTENT-HANDLING METHODS

%% @doc Returns the raw body of the last response.
-spec body(handle()) -> string().
body(Handle) ->
    case response(Handle) of
        undefined -> undefined;
        {_, _, Body} -> Body
    end.

%% @doc Returns the body of the response as an xmerl #xmlElement.
-spec body_xml(handle()) -> any().
body_xml({?MODULE,Pid}) -> gen_server:call(Pid, as_xml).

%% @doc Returns the text extracted from the response, if the response
%% was HTML.
-spec text(handle()) -> undefined | string().
text({?MODULE,Pid}) -> gen_server:call(Pid, as_text).

%%% LINK METHODS

%% @doc Follow a link on the current page, as per get/2.  See find_link/2 for a description of the argument.
-spec follow_link(integer()       |
                  {text,iodata()} |
                  {url,iodata()},
                  handle()) -> undefined | response_code().
follow_link(Argument, Handle) ->
    case find_link(Argument, Handle) of
        undefined -> undefined;
        Link -> get(Link, Handle)
    end.

%% @doc Finds a link on the current page.  If an integer is passed,
%% finds the nth link, indexed from 1; if {text,iodata()} is passed,
%% the link whose text content matches the regexp is found, per the re
%% module; a tuple of the form {url,iodata()} chooses the link whose
%% URL matches the supplied regexp.
%%
%% @see re
-spec find_link(integer()       |
                {text,iodata()} |
                {url,iodata()},
                handle()) -> undefined | #link{}.
find_link(N, Handle) when is_integer(N) ->
    lists:nth(N, links(Handle));
find_link({text,RegexString}, Handle) ->
    {ok,RE} = re:compile(RegexString),
    find_by_re(RE, [{L,L#link.text} || L <- links(Handle)]);
find_link({url,RegexString}, Handle) ->
    {ok,RE} = re:compile(RegexString),
    find_by_re(RE, [{L,L#link.url} || L <- links(Handle)]).

find_by_re(_RE, []) -> undefined;
find_by_re(RE, [{K,V}|Rest]) ->
    case re:run(V, RE) of
        {match,_} -> K;
        nomatch -> find_by_re(RE, Rest)
    end.

%% @doc Per find_link/2, but returns a list of matching links.  Does
%% not support the integer argument.
-spec find_all_links({text,iodata()}  |
                     {url,iodata()},
                     handle()) -> undefined | [#link{}].
find_all_links({text,RegexString}, Handle) ->
    {ok,RE} = re:compile(RegexString),    
    [L || L <- links(Handle), nomatch =/= re:run(L#link.text, RE)];
find_all_links({url,RegexString}, Handle) ->
    {ok,RE} = re:compile(RegexString),    
    [L || L <- links(Handle), nomatch =/= re:run(L#link.url, RE)].

%% @doc Returns all the links on the current page.
-spec find_all_links(handle()) -> undefined | [#link{}].
find_all_links({?MODULE,Pid}) -> gen_server:call(Pid, {find_all_links}).

%% XXX find_all_inputs?
%% XXX find_all_submits?

%%% FORM METHODS

%% @doc Selects the nth form on the page as the current form.  Forms are indexed from 1.
-spec form_number(integer(), handle()) -> undefined | #form{}.
form_number(N, Handle={?MODULE,Pid}) ->
    gen_server:call(Pid, {set_current_form, N}),
    current_form(Handle).

%% @doc Selects the first form whose name equals Name as the current form.
-spec form_name(string(), handle()) -> undefined | #form{}.
form_name(Name, Handle) ->
    case lists:keyfind(Name, 2, [{I,Form#form.name} || {I,Form} <- mechanize_util:enumerate(forms(Handle))]) of
        {I,_} -> form_number(I, Handle);
        false -> undefined
    end.

%% @doc Selects the form whose ID equals ID as the current form.
-spec form_id(string(), handle()) -> undefined | #form{}.
form_id(ID, Handle) ->
    case lists:keyfind(ID, 2, [{I,Form#form.id} || {I,Form} <- mechanize_util:enumerate(forms(Handle))]) of
        {I,_} -> form_number(I, Handle);
        false -> undefined
    end.

%% @doc Selects the first form with all of the named fields specified
%% in the argument.
-spec form_with_fields([string()], handle()) -> undefined | #form{}.
form_with_fields(Fields, Handle) ->
    case find_by_fields(Fields, mechanize_util:enumerate(forms(Handle))) of
        undefined -> undefined;
        I -> form_number(I, Handle)
    end.

find_by_fields(_, []) -> undefined;
find_by_fields(Fields, [{I,Form}|Forms]) ->
    case has_named_fields(Fields, Form) of
        true -> I;
        false -> find_by_fields(Fields, Forms)
    end.

has_named_fields(Desired, #form{fields=Actual}) ->
    Names = [F#field.name || F <- Actual],
    lists:all(fun (D) -> lists:member(D, Names) end, Desired).

%%% FIELD METHODS

%% @doc Sets the named field in the current form to the value or
%% values specified.  If the name matches a group of checkboxes, those
%% with matching values will be ticked, and the others will be
%% unticked.
-spec field(atom(), string()|[string()], handle()) -> undefined | ok.
field(Name, Value, {?MODULE,Pid}) -> gen_server:call(Pid, {field, Name, Value}).

%% @doc Sets fields given as a proplist.
-spec set_fields([proplists:property()], handle()) -> undefined | ok | {error,atom()}.
set_fields(Props, Handle) ->
    case lists:all(fun ({Name,Value}) -> ok =:= field(Name, Value, Handle) end, Props) of
        true -> ok;
        false -> undefined
    end.

%% @doc Sets non-hidden fields to the supplied values, in order of occurrence.
-spec set_visible([string()|[string()]], handle()) -> undefined | ok | {error,atom()}.
set_visible(Values, Handle) -> 
    case current_form(Handle) of
        undefined -> undefined;
        Form ->
            Fields = [F#field.name || F <- Form#form.fields, not lists:member(F#field.type, [hidden,submit,reset,image,button])],
            set_fields(lists:zip(Fields, Values), Handle)
    end.

%% @doc Returns the value of a named field.
-spec value(atom(), handle()) -> undefined | string() | [string()].
value(Name, {?MODULE,Pid}) -> gen_server:call(Pid, {field, Name}).

%% @doc Clicks the named button in the current form.
-spec click(string(), handle()) -> undefined | response_code().
click(Name, {?MODULE,Pid}) -> gen_server:call(Pid, {click, Name}).

%% @doc Submits the current form.
-spec submit(handle()) -> undefined | response_code().
submit({?MODULE, Pid}) -> gen_server:call(Pid, submit).

%% @doc Submits a named form (per form_name/2) with values from a
%% proplist (per set_fields/2), per submit/1.
-spec submit_form(string(), [proplists:property()], handle()) -> undefined | response_code().
submit_form(Name, Values, Handle) ->
    case {form,form_name(Name, Handle)} of
        {form,undefined} -> undefined;
        {form,_} ->
            case {fields,set_fields(Values, Handle)} of
                {fields,undefined} -> undefined;
                {fields,ok} -> submit(Handle)
            end
    end.

%%%% Application behavior

%% @private
start(_Type, _StartArgs) ->
    mechanize_sup:start_link().
%% @private
stop(_State) -> ok.

start() -> lists:foreach(fun application:start/1, [xmerl, inets, mechanizerl]).
stop() -> application:stop(mechanizerl).
