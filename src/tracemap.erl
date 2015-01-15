%%%-------------------------------------------------------------------
%%% @author David N. Welton <davidw@dedasys.com>
%%% @copyright (C) 2015, David N. Welton
%%% @doc
%%%
%%% @end
%%% Created : 15 Jan 2015 by David N. Welton <davidw@dedasys.com>
%%%-------------------------------------------------------------------
-module(tracemap).

%% API
-export([map/1]).

%%%===================================================================
%%% API
%%%===================================================================

map(AppName) ->
    Pids = app_pids(AppName),
    trace(Pids).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec walk(AppName::atom()) -> list(pid()).

app_pids(AppName) ->
    Pid = application_controller:get_master(AppName),
    walk_internal([Pid], sets:new()).

app_pids_internal([], Seen) ->
    sets:to_list(Seen);

app_pids_internal([Pid|ToWalk], Seen) ->
    Info = erlang:process_info(Pid),
    Avoid = erlang:whereis(application_controller),
    Links = proplists:get_value(links, Info),
    NewLinks = [E || E <- (Links -- [Avoid]),
                     sets:is_element(E, Seen) /= true],
    app_pids_internal(ToWalk ++ NewLinks, sets:add_element(Pid, Seen)).

trace(Pids) when is_list(Pids) ->
    dbg:start(),
    dbg:tracer(process,
               {fun msgtrace:tracer/2, []}),
    [dbg:p(Pid, [m, r]) || Pid <- Pids].

stop() ->
    dbg:stop().

tracer(Other, _Acc) ->
    error_logger:info_msg("trace: ~p", [Other]).
