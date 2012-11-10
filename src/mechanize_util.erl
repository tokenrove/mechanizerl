-module(mechanize_util).
-export([enumerate/1]).

enumerate(List) -> enumerate(List, 1).
enumerate([], _) -> [];
enumerate([X|Xs], I) -> [{I,X} | enumerate(Xs, 1+I)].
