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
-export([app_pids/1, map/1]).

%%%===================================================================
%%% API
%%%===================================================================

map(AppName) ->
    Pids = app_pids(AppName),
    lists:map(
      fun(P) ->
              Name =
                  case erlang:process_info(P, registered_name) of
                      {registered_name, N} -> N;
                      _ -> undefined
                  end,
              io:format("Pid:	~p Name:	~p~n", [P, Name])
      end, Pids),
    trace(Pids).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec app_pids(AppName::atom()) -> list(pid()).

app_pids(AppName) ->
    Pid = application_controller:get_master(AppName),
    {group_leader, Leader} = erlang:process_info(Pid, group_leader),
    app_pids_internal([Pid], sets:new(), Leader).

app_pids_internal([], Seen, _Leader) ->
    sets:to_list(Seen);

app_pids_internal([Pid|ToWalk], Seen, Leader) when is_port(Pid) ->
    app_pids_internal(ToWalk, Seen, Leader);
app_pids_internal([Pid|ToWalk], Seen, Leader) when is_pid(Pid) ->
    Info = erlang:process_info(Pid),
    case proplists:get_value(group_leader, Info) of
        Leader ->
            %% If it has the same group leader, it's good, let's
            %% investigate it.
            Links = proplists:get_value(links, Info),
            NewLinks = [E || E <- Links,
                             sets:is_element(E, Seen) /= true],
            app_pids_internal(ToWalk ++ NewLinks, sets:add_element(Pid, Seen), Leader);
        _ ->
            %% Ignore it
            app_pids_internal(ToWalk, Seen, Leader)
    end.

trace(Pids) when is_list(Pids) ->
    dbg:start(),
    {ok, DumpFile} = file:open("/tmp/trace.txt", [write]),
    dbg:tracer(process,
               {fun tracer/2, DumpFile}),
    [dbg:p(Pid, [m, r]) || Pid <- Pids].

stop() ->
    dbg:stop().

tracer(Data, DumpFile) ->
    ToWrite =
        case Data of
            {trace, Pid, send, Msg, Dest} ->
                {Pid, send, Msg, Dest};
            {trace, Pid, 'receive', Msg} ->
                {Pid, 'receive', Msg}
        end,
    file:write(DumpFile, io_lib:format("~p.~n", [ToWrite]))
    DumpFile.
