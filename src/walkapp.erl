-module(walkapp).

-export([walk/1]).

walk(AppName) ->
    Pid = application_controller:get_master(AppName),
    walk_internal([Pid], sets:new()).

walk_internal([], Seen) ->
    error_logger:info_msg("done!"),
    sets:to_list(Seen);

walk_internal([Pid|ToWalk], Seen) ->
    Info = erlang:process_info(Pid),
    error_logger:info_msg("Looking at ~p (~p)~n", [Pid, proplists:get_value(registered_name, Info)]),
    Avoid = erlang:whereis(application_controller),
    Links = proplists:get_value(links, Info),
    NewLinks = [E || E <- (Links -- [Avoid]),
                     sets:is_element(E, Seen) /= true],
    walk_internal(ToWalk ++ NewLinks, sets:add_element(Pid, Seen)).

