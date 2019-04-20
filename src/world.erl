%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                         %%
%% File:        world.erl                                                  %%
%%                                                                         %%
%% Description: This is the world server process. It representats the      %%
%%              game world. It holds a list of all processes on the map    %%
%%              stored in an ETS Table. Via erlang:monitor() the list      %%
%%              of processes on the map is automatically updated if a      %%
%%              process is terminated.                                     %%
%%                                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HIGH PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LOW PRIORITY TASKS %%&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: implement a radar ping on all processes
%% TODO: implement a save mechanism during pause mode
%% TODO: unit test to add two different PIDs and calculate the distance
%% TODO: unit test to retrieve all PIDs at a certain position with around 5 
%%       PIDs registered
%% TODO: unit test to pause all registred processes
%% TODO: unit test to resume all registred processes
%% TODO: unit test the all/1 function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Module Definition        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(world).
-behaviour(gen_server).
-export([handle_cast/2, handle_info/2, start/1, start_link/1,
         init/1, handle_call/3, code_change/3, terminate/2,
         add/2, rm/2, pos/2, distance/2, help/0, stop/1, pause/2,
         resume/2, processes/1, save/2  ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("records.hrl").

-compile([debug_info]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Public Server Interface  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help( ) ->
  io:format("world:add(PID, {Lat,Long,Name,Type,PID2}): Add a PID at "
            "positioin Lat|Long~n"),
  io:format("world:rm(PID, {Lat,Long,Name,Type,PID2}): Remove a PID at "
            "position Lat|Long~n"),
  io:format("world:pos(PID, {Lat,Long}): Retrieve all PIDs at the "
            "position Lat|Long ~n"),
  io:format("world:distance(PID, {PIDA,PIDB}): calculate the distance "
            "between A and B~n"),
  io:format("world:processes(world): show all processes on the world~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Add a PID at position Latitude, Longitude
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add( PID, {Lat,Long,Name,Type,PID2} ) 
when is_number(Lat), is_number(Long)  ->
  gen_server:call(PID, {register,Lat,Long,Name,Type,PID2}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Pause this process and all processes of the world
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause( PID, _AdminToken ) when is_list(_AdminToken) ->
  gen_server:call(PID, {pause, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Create a XML Savegame. All registered processes must be in the
%%      paused state, before beeing able to save them.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save( PID, _AdminToken ) when is_list(_AdminToken) ->
  gen_server:call(PID, {save, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc resume this process and all processes of the world
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resume( PID, _AdminToken ) when is_list(_AdminToken) ->
  gen_server:call(PID, {resume, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc remove a PID at position Latitude, Longitude
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm( PID, {Lat,Long,Name,Type,PID2} ) 
when is_number(Lat), is_number(Long), is_list(Name) ->
  gen_server:call(PID, {unregister,Lat,Long,Name,Type,PID2}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc get all PIDs at position Latitude,Longitude
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pos( PID, {Lat,Long} ) when is_number(Lat), is_number(Long) ->
  gen_server:call(PID, {retrieve,Lat,Long} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc get all PIDs at position Latitude,Longitude
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

processes( PID ) ->
  gen_server:call(PID, {all} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc get the Distance between two PIDs
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

distance( PID, {PIDA,PIDB} ) ->
  gen_server:call(PID, {distance,PIDA,PIDB}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc stop the server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop( PID ) ->
  gen_server:cast(PID, {stop} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper Functions    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Calculate the distance between two processes that are registered
%%      at the world process. Return {fail} or {ok, DistanceKm}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate_distance(PIDA, PIDB, State) when is_record(State, worldstate) ->

  case ets:match(State#worldstate.map, {'$1','$2', '$3', '$4',PIDA}) of
    [[LatA,LongA]] -> case ets:match(State#worldstate.map, 
                                 {'$1','$2', '$3', '$4', PIDB}) of
                        [[LatB, LongB]] ->
                          CoA = #coords{ latitude = LatA, 
                                             longitude = LongA },
                          CoB = #coords{ latitude = LatB, 
                                             longitude = LongB },
                          DistanceKm = calc:distance(CoA,CoB),
                          {ok, DistanceKm};
                        [] -> {fail}
                      end;
    [] -> {fail}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc check if the state is paused or not. Return true or false.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_paused(_State) when is_record(_State, worldstate) ->
  case _State#worldstate.paused of
    true -> true;
    _  -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Server Initialization %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start world server with map
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Map) -> 
  State = #worldstate{ map = Map },
  io:format("start world ~n~n"),
  gen_server:start({local, ?MODULE}, ?MODULE, [State],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start the world process.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Map) ->
  State = #worldstate{ map = Map },
  io:format("start_link: world ~n~n"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [State],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function initializes the auth server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([State]) ->
  rand:seed(exs1024,{erlang:phash2([node()]),
            erlang:monotonic_time(),
            erlang:unique_integer()}),
  io:format("world init~n~n"),
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Server Message Handling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc register PID at Latitude,Longitude
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({register,_Lat,_Long,_Name,_Type,_PID},_From,_State) 
when is_number(_Lat), is_number(_Long)  -> 
 
  % check if the process is paused. if yes, return {paused}. Else insert the
  % new process to the ETS Table and establish an erlang:monitor between the
  % two processes. As soon as the registered process dies, a {'DOWN', ...}
  % message will take care of the deregistration of the process from the
  % ETS Table.

  case is_paused(_State) of
    true  -> {reply, {paused}, _State};
    false -> ets:insert(_State#worldstate.map , {_Lat,_Long,_Name,_Type,_PID}),
             erlang:monitor(process, _PID),
             {reply, {ok, {_Lat,_Long, _Name,_Type,_PID}}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc send a pause message to all processes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({pause, _AdminToken},_From,_State) 
when is_record(_State, worldstate), is_list(_AdminToken) -> 

  % only admins have the persmissions to pause, so check if it is an
  % admin. if yes, then search the ETS Table for ALL entries and send
  % a pause message to each town or player process. 

  case auth:verify_admin(whereis(auth), _AdminToken) of
    {verify_admin_ok, _} -> 
      NewState = _State#worldstate{ paused = true },
      ProcessList = ets:match(_State#worldstate.map, 
                              {'$1','$2', '$3', '$4','$5'}),
      Responses = lists:map( fun(X) ->
                              [_,_,_,Type,PID] = X,
                              case Type of
                                town -> town:pause(PID, _AdminToken);
                                player -> player:pause(PID, _AdminToken)
                              end
                            end,
                            ProcessList ),

      % if one process could not be stopped, then return {fail} else
      % return {ok}

      case lists:member(no_permission, Responses) of
        true -> {reply, {fail}, NewState};
        _ ->  {reply, {ok}, NewState}
      end;

    % If the admin 
    % privileges are not present, return {verify_admin_failed}
    {verify_admin_failed} -> 
      {reply, {verify_admin_failed}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc send a resume message to all processes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({resume, _AdminToken},_From,_State) 
when is_record(_State, worldstate) -> 

  % verfiy that the _AdminToken belongs to an Admin
  case auth:verify_admin(whereis(auth), _AdminToken) of
    {verify_admin_ok, _} -> 

      % if verfication suceeded, set the paused state
      % of this server to false.
		  
      NewState = _State#worldstate{ paused = false },
      
      % create a process list of all registered processes

      ProcessList = ets:match(_State#worldstate.map, 
                              {'$1','$2', '$3', '$4','$5'}),
      
      % send a resume message to each process by calling the interface
      % function of the process town:resume or player:resume. Save the
      % responses of the servers in a Responses Variable.
      
      Responses = lists:map( fun(X) ->
                       [_,_,_,Type,PID] = X,
                         case Type of
                           town -> town:resume(PID, _AdminToken);
                           player -> player:resume(PID, _AdminToken)
                         end
                     end,
                     ProcessList ),

      % check if there was a message "no_permission" in the responses
      % and return fail in this case, otherwise it seems everything was
      % fine, so return {ok}.

      case lists:member(no_permission, Responses) of
        true -> {reply, {fail}, NewState};
        _ ->  {reply, {ok}, NewState}
      end;
    {verify_admin_failed} -> 
      {reply, {verify_admin_failed}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Create an XML savegame for all processes. To do so it is necessary
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({save, _AdminToken},_From,_State) 
when is_record(_State, worldstate) -> 

  % check if the process is paused. if not, then go return "not_paused"
  % otherwise go on with the procedure.

  case is_paused(_State) of
    false -> {reply, {not_paused}, _State};
    true  ->  ProcessList = ets:match(_State#worldstate.map, 
                              {'$1','$2', '$3', '$4','$5'}),

	      % check if all processes are paused, only if this is true
	      % then we can go on and call the save routines of all the
	      % single processes

              Responses = lists:map( fun(X) ->
                                        [_,_,_,Type,PID] = X,
                                          case Type of
                                            town -> town:paused(PID);
                                            player -> player:paused(PID)
                                          end
				     end,
                                          ProcessList ),

              % check if in the list of responses there is a "false" response
	      % If "false", some processes are not paused and "not_paused" will
	      % be returned. Else go on and save the state of the processes

              case lists:member({false}, Responses) of
                true  -> {reply, {not_paused}, _State};

                % iterate the process list and call process:save for all processes
                % then get the XML result of the save operation and return it as
                % a reply.

                false -> Result = lists:map( fun(X) ->
                                      [_,_,_,Type,PID] = X,
				        case Type of
                                          town -> {ok, XML} = town:save(PID,
						         	_AdminToken),
						  {XML};
				          player -> {ok, XML} = player:save(PID,
						                _AdminToken),
						    {XML}
                                        end
				    end,
				    ProcessList ),
			 {reply, {Result}, _State}
             end
  end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc unregister PID at Latitude,Longitude
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({unregister,_Lat,_Long,_Name,_Type,_PID},_From,_State) -> 

  case is_paused(_State) of
	  
    % check if the process is currently paused, if yes, ignore the request
    % and answer with {paused}
	  
    true -> {reply, {paused}, _State};

    % if the process is not paused, delete the entry that needs to be
    % deleted. Reply {ok} if the outcome of the operation is correct, else
    % {fail}

    false -> R = ets:match_delete(_State#worldstate.map, 
                                  {_Lat,_Long,_Name,_Type, _PID}),
             case R of
               true -> {reply, {ok}, _State};
               false -> {reply, {fail}, _State}
             end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc calculate Distance between two PIDs
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({distance,PIDA,PIDB},_From,_State) -> 
   case calculate_distance(PIDA, PIDB,_State) of
      {fail} -> {reply, {fail}, _State};
      {ok, NumberKm} -> {reply, {NumberKm}, _State}
   end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc retrieve all PIDs at Latitude,Longitude
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({retrieve,_Lat,_Long},_From,_State) -> 
  R = ets:match(_State#worldstate.map, {_Lat,_Long, '$1', '$2', '$3'}),
  {reply, R, _State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc retrieve all PIDs 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({all},_From,_State) -> 
  R = ets:match(_State#worldstate.map, {'$1','$2', '$3', '$4', '$5'}),
  {reply, R, _State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc handle DOWN messages. Whenever a process dies, that is monitored,
%%      it will be removed from the ets.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({'DOWN', _Ref, _Process, _PID, _Reason}, _OldState) 
when is_record(_OldState, worldstate) -> 
  ets:match_delete(_OldState#worldstate.map, {'$1','$2','$3','$4', _PID}),
  {noreply, _OldState};
handle_info(_,_State)       -> {noreply, _State}.


handle_cast({stop}, State)  -> {stop, normal, State}.
code_change(_,_,_)          -> {ok}.
terminate(_,_)              -> {ok}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       Unit Tests         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc handle DOWN messages. Whenever a process dies, that is monitored,
%%      it will be removed from the ets.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

add_pid_and_retrieve_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,_}  = auth:login(auth,"Daniel","blabla"),
  world:add( world, {1,2,"hash",town,whereis(auth)} ),
  List = world:pos( world, {1,2} ),
  world:stop(world),
  auth:stop(auth),
  lists:member({1,2,"hash",town,whereis(auth)}, List) =:= true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test if all registered processes are paused after the world server
%%      has send "pause" messages to each of the registered processes. This
%%      is necessary to pause all processes and afterwards create a consistent
%%      save file.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help_test() ->
  world:help().

pause_all_registered_processes_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,K}  = auth:login(auth,"Daniel","blabla"),

    StateNuremberg = 
    #townstate{ name          = "nuremberg",
                coordinate    = #coords
                { 
                  latitude  = 49.461,
                  longitude = 11.062
                },
                population    = 1300000,
                connections   = [],
                birthrate     = 20.0,
                infectionrate = 5.0,
                lethality     = 30.0,
                travelrate    = 400,
                airport       = open,
                roads         = open,
                players       = []
              },

  StateMunich = 
  #townstate{ name            = "munich",
              coordinate      = #coords
              { 
                latitude  = 48.144,
                longitude = 11.558
              },
              population      = 1300000,
              birthrate       = 20.0,
              infectionrate   = 5.0,
              lethality       = 30.0,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              players         = []
            },

  {ok,_} = town:start(StateMunich),
  {ok,_} = town:start(StateNuremberg),
  world:pause(world, K),
  Result = town:paused(nuremberg),
  Result2 = town:paused(munich),
  
  world:stop(world),
  auth:stop(auth),
  town:stop(nuremberg),
  town:stop(munich),
  {Result, Result2} =:= {{paused},{paused}}.

try_save_all_processes_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,K}  = auth:login(auth,"Daniel","blabla"),

    StateNuremberg = 
    #townstate{ name          = "nuremberg",
                coordinate    = #coords
                { 
                  latitude  = 49.461,
                  longitude = 11.062
                },
                population    = 1300000,
                connections   = [],
                birthrate     = 20.0,
                infectionrate = 5.0,
                lethality     = 30.0,
                travelrate    = 400,
                airport       = open,
                roads         = open,
                players       = []
              },

  StateMunich = 
  #townstate{ name            = "munich",
              coordinate      = #coords
              { 
                latitude  = 48.144,
                longitude = 11.558
              },
              population      = 1300000,
              birthrate       = 20.0,
              infectionrate   = 5.0,
              lethality       = 30.0,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              players         = []
            },
  PlayerState =
  #playerstate{ name = "daniel", 
                coordinate = #coords{ latitude = 49.461, longitude = 11.062 },
                location = undefined,
                activity = undefined,
                paused = false
  },
  {ok,_} = player:start(PlayerState),
  {ok,_} = town:start(StateMunich),
  {ok,_} = town:start(StateNuremberg),
  world:pause(world, K),
  world:save(world, K),
  
  town:stop(munich),
  town:stop(nuremberg),
  player:stop(daniel),
  auth:stop(auth),
  world:stop(world).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
