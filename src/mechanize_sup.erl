%% @private
-module(mechanize_sup).
-export([init/1, start_link/0, start_child/0]).
-behaviour(supervisor).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Worker = {mechanize_worker,{mechanize_worker,start_link,[]},temporary,brutal_kill,worker,[mechanize_worker]},
    {ok, {{simple_one_for_one, 0, 1}, [Worker]}}.

%% Ugly and probably doesn't scale well; see note at the top of mechanize_worker.erl
gensym() ->
    N = proplists:get_value(specs, supervisor:count_children(?MODULE)),
    list_to_atom("profile_"++integer_to_list(N)).

start_child() ->
    supervisor:start_child(?MODULE, [gensym()]).

