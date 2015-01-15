-module(msgtrace).

-include_lib("stdlib/include/ms_transform.hrl").

-export([trace/0, trace/1, tracer/2]).

trace() ->
    trace(all).

trace(Pid) when is_pid(Pid) ->
    dbg:start(),
    dbg:tracer(process,
               {fun msgtrace:tracer/2, []}),
    dbg:p(Pid, [m, r]);

trace(Pids) when is_list(Pids) ->
    dbg:start(),
    dbg:tracer(process,
               {fun msgtrace:tracer/2, []}),
    [dbg:p(Pid, [m, r]) || Pid <- Pids].

tracer(Other, _Acc) ->
    error_logger:info_msg("trace: ~p", [Other]).
