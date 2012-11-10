
-type header() :: {Name::atom(), Value::string()}.
-type body() :: iodata().

-record(request,
        { url :: url(),
          content_type = undefined,
          method :: http_verb(),
          headers = [] :: [header()],
          body :: body()}).

-record(response,
        { status :: integer(),
          headers = [] :: [header()],
          body :: body()}).

-record(state,
        { %% always set
          profile :: atom(),
          %% present once we've made a request
          content_type :: undefined|{string(),string()},
          history = [] :: [{#request{},#response{}}],
          %% only present if the current response was HTML
          links = [] :: [#link{}],
          forms = [] :: [#form{}],
          current_form :: undefined|integer(),
          title :: undefined|string(),
          xml,
          text = [] :: string()
        }).
