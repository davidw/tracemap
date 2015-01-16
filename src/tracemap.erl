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
-export([app_pids/1, map/1, graphviz/1, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec map(AppName::atom()) -> list().

%% Trace the pids for a given application.
map(AppName) ->
    Pids = app_pids(AppName),
    trace(Pids),
    [name_from_pid(P) || P <- Pids].

%% Show all pids under an application.
-spec app_pids(AppName::atom()) -> list(pid()).

app_pids(AppName) ->
    Pid = application_controller:get_master(AppName),
    {group_leader, Leader} = erlang:process_info(Pid, group_leader),
    app_pids_internal([Pid], sets:new(), Leader).

%% Creates a graphviz file from the relationships.
-spec graphviz(Filename::string()) -> ok.

graphviz(Filename) ->
    {ok, F} = file:open(Filename, [write]),
    file:write(F, "digraph G {\n"),
    file:write(F, "	overlap = false;\n"),
    lists:map(
      fun({send, Pid, Dest, _Msg}) ->
              file:write(F, io_lib:format("	~p -> ~p~n", [Pid, Dest]));
         ({'receive', _Pid, _Msg}) ->
              undefined
      end, ets:tab2list(tracedump)),
    file:write(F, "}\n").

stop() ->
    catch ets:delete(tracedump),
    dbg:stop().


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================


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
    catch ets:delete(tracedump),
    ets:new(tracedump, [public, duplicate_bag, named_table]),
    dbg:tracer(process,
               {fun tracer/2, []}),
    [dbg:p(Pid, [m, r]) || Pid <- Pids].

tracer(Data, []) ->
    ToWrite =
        case Data of
            {trace, Pid, send, Msg, Dest} ->
                {send, name_from_pid(Pid), name_from_pid(Dest), Msg};
            {trace, Pid, 'receive', Msg} ->
                {'receive', name_from_pid(Pid), Msg}
        end,
    ets:insert(tracedump, ToWrite),
    [].

name_from_pid(Process) when is_atom(Process) ->
    Process;
name_from_pid(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, N} -> N;
        _ -> Pid
    end.
