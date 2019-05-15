%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                         %%
%% File:        town.erl                                                   %%
%%                                                                         %%
%% Description: This is one of the most important processes of the game    %%
%%              The town can contain players which can join a town.        %%
%%              It keeps track of the population and the infected          %%
%%              population. Heals can be performed by the players if they  %%
%%              have previously joined the town. Also the town has         %%
%%              associated connections to other towns which can be used    %%
%%              to travel by ship, by air or by road. Also the population  %%
%%              uses this travel to be transfered to other towns, thereby  %%
%%              spreadic diseases to other towns. Policies can be changed  %%
%%              to close connections to reduce traffic and other things    %%
%%              which need to be implemented.                              %%
%%                                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HIGH PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: implement automatic travel of population 
%% TODO: implement a way for the player to travel between towns
%% TODO: implement facilities like hospital

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LOW PRIORITY TASKS %%&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: write a functions that checks the town state for equality 
%%       while disregarding the timely stuff.
%% TODO: implement help() functionality for single functions.
%% TODO: impelement expection handling in some way if a wrong message is
%%       received without crashing the process.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Module Definition        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(town).
-behaviour(gen_server).
-export([start/1, start_link/1, terminate/2, help/0, stop/1,
         init/1, handle_info/2, handle_cast/2, handle_call/3,
         code_change/3, join/3, leave/3, state/2, latitude/1,
         longitude/1, coordinates/1, connect/4, pause/2, resume/2,
         disconnect/4, help/1, immigrate/2, save/2, population/1,
         infected/1, heal/2, paused/1]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchonous message {heal, _Token} 
%%      to the process _PID. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

heal( _PID, _Token) ->
  gen_server:call(_PID, {heal, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchnoous message {pause, 
%%      _AdminToken} to the process _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause( _PID, _AdminToken) ->
  gen_server:call(_PID, {pause, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchnoous message {is_paused, 
%%      _AdminToken} to the process _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

paused( _PID ) ->
  gen_server:call(_PID, {is_paused}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message 
%%      {resume, _AdminToken} to Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resume( _PID, _AdminToken) ->
  gen_server:call(_PID, {resume, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface functions send the synchronous message 
%%      {_PID, _AdminToken} to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save( _PID, _AdminToken) ->
  gen_server:call(_PID, {save, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {joinplayer,
%%      _PlayerID, _Token} to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

join( _PID, _PlayerID, _Token) ->
  gen_server:call(_PID, {joinplayer, _PlayerID, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {leaveplayer,
%%      _PlayerID, _Token} to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leave( _PID, _PlayerID, _Token) ->
  gen_server:call(_PID, {leaveplayer, _PlayerID, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {state,
%%      _AdminToken} to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

state(_PID, _AdminToken) ->
  gen_server:call(_PID, {state, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {latitude}
%%      to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

latitude(_PID) ->
  gen_server:call(_PID, {latitude}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {longitude}
%%      to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

longitude(_PID) ->
  gen_server:call(_PID, {longitude}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {population}
%%      to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

population(_PID) ->
  gen_server:call(_PID, {population}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {infected}
%%      to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

infected(_PID) ->
  gen_server:call(_PID, {infected}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {coordinate}
%%      to the server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coordinates(_PID) ->
  gen_server:call(_PID, {coordinates}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {immigrate,
%%      _Immigrants} to the server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

immigrate(_PID, _Immigrants) when is_record(_Immigrants, immigrants) ->
  gen_server:call(_PID, {immigrate, _Immigrants}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {connect,
%%      _Location, air|road|ship, _AdminToken} to the Server located at
%%      _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(_PID, Location, air, _AdminToken) 
when is_record(Location, location) ->
  gen_server:call(_PID, {connect, Location, air, _AdminToken});
connect(_PID, Location, road, _AdminToken) 
when is_record(Location, location) ->
  gen_server:call(_PID, {connect, Location, road, _AdminToken});
connect(_PID, Location, ship, _AdminToken) 
when is_record(Location, location) ->
  gen_server:call(_PID, {connect, Location, ship, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the synchronous message {disconnect,
%%      _Location, air|road|ship, _AdminToken} to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disconnect(_PID, _Location, air, _AdminToken) 
when is_record(_Location, location) ->
  gen_server:call(_PID, {disconnect, _Location, air, _AdminToken});
disconnect(_PID, _Location, road, _AdminToken) 
when is_record(_Location, location) ->
  gen_server:call(_PID, {disconnect, _Location, road, _AdminToken});
disconnect(_PID, _Location, ship, _AdminToken) 
when is_record(_Location, location) ->
  gen_server:call(_PID, {disconnect, _Location, ship, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends the asynchronous message {stop} to 
%%      the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_PID) ->
  gen_server:cast(_PID, {stop}).

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
%% @doc This public interface functions prints a summary of the public
%%      interface functions.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help() ->
  io:format(" town:longitude( _PID )          ~n"),
  io:format(" town:latitude( _PID )           ~n"),
  io:format(" town:coordinates( _PID )        ~n"),
  io:format(" town:state( _PID, _AdminToken ) ~n"),
  io:format(" town:pause( _PID, _AdminToken ) ~n"),
  io:format(" town:resume( _PID, _AdminToken ) ~n"),
  io:format(" town:save( _PID, _AdminToken )  ~n"),
  io:format(" town:join( _PID, _PlayerID, _Token) ~n"),
  io:format(" town:leave( _PID, _PlayerID, _Token ) ~n"),
  io:format(" town:immigrate( _PID, _Immigrants ) ~n"),
  io:format(" town:connect( _PID, _Location, air|road|ship, _AdminToken ) ~n"),
  io:format(" town:disconnect( _PID, _Location, air|road|ship, "
            "_AdminToken)~n").

help(longitude) ->
  io:format(" town:longitude( _PID )          ~n"),
  io:format("              ~n"),
  io:format("      returns the longitude of the town as {12.12}  ~n"),
  io:format("              ~n")
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function checks if the townstate _State is paused or not. It 
%%      returns eiter true or false.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_paused(_State) when is_record(_State,townstate) ->
  case _State#townstate.paused of
    true  -> true;
    false -> false;
    _     -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function checks if the player owning the _Token already 
%%      perfomed his heal today. It returns either ok or fail.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_healed(_Token,_State) when is_record(_State,townstate), is_list(_Token) ->
  Heals = _State#townstate.healsofplayers,
  case Heals of
    undefined -> false;
    undef     -> false;
    []        -> false;
    _         -> R = lists:map( 
		 fun(X) ->
                   case {_Token} =:= X of
                     true  -> ok;
                     false -> fail
                   end
                 end,
                   Heals),
                 lists:member(ok, R)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function checks if _Player is registered in _State. Return "true" 
%%      or "false".  
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_player_registered(_Player, _State) 
when is_record(_State, townstate), is_record(_Player, playerid)
->
  RegisteredPlayers = _State#townstate.players,
  R = lists:flatten(lists:map( fun(X) ->
                                 case _Player#playerid.name =:= 
                                      X#playerid.name of
                                   true -> player_registered;
                                   _    -> []
                                 end
                               end
                               , RegisteredPlayers ) ),
  lists:member(player_registered,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function verfies if _Connection exists in _State. Return "true" 
%%      or "false".
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connection_exists(_Connection, _State) 
when is_record(_State, townstate), is_record(_Connection,aircon);
     is_record(_State, townstate), is_record(_Connection,roadcon);
     is_record(_State, townstate), is_record(_Connection,shipcon) ->

  Connections     = _State#townstate.connections,
  
  R = lists:flatten(lists:map( fun(X) ->
                                 case _Connection =:= X of
                                   true -> connection_exists;
                                   _    -> []
                                 end
                               end
                               , Connections ) ),
  lists:member(connection_exists,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function verfies that a _Token has user permissions. It returns
%%      fail or ok.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ask_auth_server_for_user_permission(_Token,_State)
  when is_record(_State, townstate) ->

  AuthServerPID = whereis(auth),
  Token         =  _Token,

  case auth:verify(AuthServerPID, Token) of
    {verify_failed} -> fail;
    {verify_ok, _} -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function Checks back with the authentification server named
%%      "auth", if the _Token has admin privileges or not. It either returns
%%      ok or fail.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ask_auth_server_for_admin_permission(_Token,_State)
  when is_record(_State, townstate) ->
  AuthServerPID = whereis(auth),
  Token         =  _Token,

  case auth:verify_admin(AuthServerPID, Token) of
    {verify_admin_failed} -> fail;
    {verify_admin_ok, _} -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function updates the state of the town. It calculates the
%%      Time passed since the last start of day and if a day has passed,
%%      calls the function iterate_day(...)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update( _State ) when is_record(_State, townstate) ->
  StartOfDaySec  = _State#townstate.start_of_day_sec,
  TimeNative     = erlang:monotonic_time(),
  TimeSec        = erlang:convert_time_unit(TimeNative, native, seconds),
  Delta          = TimeSec - StartOfDaySec,
  LengthOfDaySec = ?LENGTH_OF_DAY_HOUR * ?LENGTH_OF_HOUR_SEC * 
                   ?LENGTH_OF_SEC_SEC,
  NewState = case Delta > LengthOfDaySec of
               true  -> iterate_day(_State, trunc(Delta/LengthOfDaySec));
               false -> _State
             end,
  NewState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels a swtich over between two consequetive days.
%%      It calculates the newly infected persons, the people who die from
%%      infections. Also it can handle several days that are iterated at
%%      once, with it's second argument representing the number of days
%%      which have passed.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iterate_day(_State, 1) when is_record(_State,townstate) ->
  CurrentTimeNative = erlang:monotonic_time(),
  CurrentTimeSec    = erlang:convert_time_unit(CurrentTimeNative, 
                                               native, seconds),

  % each infected person infects other persons
  
  Infections        = trunc(_State#townstate.infectionrate * 
                            _State#townstate.infectedpopulation),
  % some of the infected will die
  
  Lethality         = _State#townstate.lethality,

  % new population is population minus infectedpeople*lethality
  
  Population        = trunc( _State#townstate.population - 
                             _State#townstate.infectedpopulation * Lethality),

  % the new infected persons is the infected persons minus dead people
  % among the infected plus the new infections. Also we must verify that
  % the new infections don't exceed the amount of available population which
  % is still healthy.
  
  Infected          = trunc(_State#townstate.infectedpopulation - 
                            Lethality * _State#townstate.infectedpopulation + 
                            case Infections > ( _State#townstate.population
                                 -_State#townstate.infectedpopulation) of
                              true  -> (_State#townstate.population-
                                        _State#townstate.infectedpopulation);
                              false -> Infections
                            end),

  NewState          = _State#townstate{ infectedpopulation = 
                                          case Infected of
                                            X when X > 0 -> X;
                                            X when X < 0 -> 0;
                                            0 -> 0
                                          end,
                                        start_of_day_sec   = CurrentTimeSec,
                                        population         = 
                                          case Population of
                                            X when X > 0 -> X;
                                            X when X < 0 -> 0;
                                            0 -> 0
                                          end,
					healsofplayers = []
                                      },

  NewState; 
iterate_day(_State, _N) 
when is_record(_State,townstate), is_integer(_N), _N > 1 ->
  CurrentTimeNative = erlang:monotonic_time(),
  CurrentTimeSec    = erlang:convert_time_unit(CurrentTimeNative, 
                                               native, seconds),
  Infections        = trunc(_State#townstate.infectionrate * 
                                 _State#townstate.infectedpopulation),
  Lethality         = _State#townstate.lethality,
  Population        = trunc( _State#townstate.population - 
                      _State#townstate.infectedpopulation * Lethality),
  Infected          = trunc(_State#townstate.infectedpopulation - 
                      Lethality 
                      * _State#townstate.infectedpopulation + Infections),

  NewState          = _State#townstate{ infectedpopulation = Infected,
                                        start_of_day_sec  = CurrentTimeSec,
                                        population        = Population
                                      },
  iterate_day(NewState, _N-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Server Initialization %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function starts the server without creating a link.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_State) when is_record(_State, townstate) ->
  gen_server:start({local, 
    list_to_atom(lists:flatten(io_lib:format("~s",[_State#townstate.name]))) },
                   ?MODULE, _State,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function starts the server by calling the underlying function
%%      gen_server:start_link. A link is created between the processes.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(_State) when is_record(_State, townstate) ->
  gen_server:start_link({local, 
    list_to_atom(lists:flatten(io_lib:format("~s",[_State#townstate.name]))) },
                        ?MODULE,  _State,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  @doc This function reads a _State variable and initializes the town server.
%%       It creates a seed for the random number generator, then it 
%%       saves the start time of the server in Erlang:monotonic_time(). Note
%%       that the erlang monotonic time is a time, that strictly increases
%%       whith each call to it and which is consistent among all connected
%%       Erlang nodes. Afterwards add the town server to the world map, by
%%       calling world:add(world ...), to communicate with the world server.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_State) when is_record(_State, townstate) ->
  rand:seed(exs1024,{erlang:phash2([node()]),
            erlang:monotonic_time(),
            erlang:unique_integer()}),
  StartOfDayNative  = erlang:monotonic_time(),
  StartOfDaySec = erlang:convert_time_unit(StartOfDayNative, native,
                    seconds),
  Coordinate = _State#townstate.coordinate,
  Lat        = Coordinate#coords.latitude,
  Long       = Coordinate#coords.longitude,
  WorldPID   = whereis(world), 
  case is_pid(WorldPID) of
    true  -> Response = world:add( whereis(world), {Lat, Long, 
                   _State#townstate.name, town, self()  }),
             Response = {ok, {Lat,Long, _State#townstate.name, town, self()}},
             NewState = _State#townstate{ start_of_day_sec = StartOfDaySec },
             {ok, NewState};
    false -> {world_pid_not_found}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Server Message Handling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {heal, _Token} message. This is
%%      message is usually send to the town server by a player process. The
%%      town server checks if the _Token is valid and if the _Token belongs
%%      to a player with the character type "medic", also it checks the
%%      level of the character by contacting the authentification server.
%%      Then it decreases the number of infected people in the town by the
%%      number of the playerlevel. Afterwards it adds the player to a list
%%      called "healsofplayers". Since only one heal per day may be perfomed
%%      by each player, this list is reset at the new day. If no more heals
%%      are available the function replies with {no_more_heal}. If the player
%%      is not a medic, the function replies with {wrong_character}. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({heal, _Token}, _From, _OldState) 
when is_record(_OldState, townstate) ->
  case ask_auth_server_for_user_permission(_Token,_OldState) of
    ok -> 
      case is_paused(_OldState) of
        true  -> {reply, {paused}, _OldState};
        false -> Character = auth:character(auth,_Token),
                 Level     = auth:level(auth,_Token),
                 case Character =:= {verify_ok, medic} of
                   true -> 
                     case Level of
                       {verify_ok,LevelNum} -> 
                         case has_healed(_Token,_OldState) of 
                           true -> {reply, {no_more_heal}, _OldState};
		           false -> Infected = 
				      _OldState#townstate.infectedpopulation,
				    %% this is important otherwise the code could crash
				    %% when the healsofplayers is undefined
			            Heals = case _OldState#townstate.healsofplayers of
					      undef -> [];
                                              undefined -> [];
		                              A -> A
				            end,
			            NewHeal = lists:flatten([
	                                 lists:append(Heals,{_Token})]),
                                    NewInfected = Infected - LevelNum,
                                    NewState = _OldState#townstate{ 
                                         infectedpopulation = NewInfected, 
                                         healsofplayers     = NewHeal 
                                    },
                                    {reply, {LevelNum}, NewState}
		         end;
                       _ -> {reply, {level_error}, _OldState}
                     end;
                   _ -> {reply, {wrong_character}, _OldState}
                 end
      end;
    fail -> {reply, {no_permission}, _OldState}
  end
;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {travel, _Origin, _To, _Player,
%%      _Transport, _Token} message. It replies with {paused} if the server
%%      is paused. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({travel, _Origin, _To, _Player, _Transport, _Token}, _From, 
            _OldState) 
when is_record(_OldState, townstate),
     is_record(_Origin,   location ),
     is_record(_To,       location),
     is_record(_Player,   playerid),
     is_list(_Token) ->

  %% TODO: implement 
  case ask_auth_server_for_user_permission(_Token,_OldState) of
    ok -> case is_paused(_OldState) of
            true  -> {reply, {paused}, _OldState};
            false ->  
              case is_player_registered(_Player,_OldState) of
                true -> {ok, {travel_started}, _OldState};
                false -> {reply, {player_not_in_town}, _OldState}
              end
          end;
    fail -> {reply, {no_permission}, _OldState}
  end
;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This functon function handels a wrong message to not make the
%%      process crash. Replies with {wrong_message}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({join, town, _,_}, _From, _OldState) 
when is_record(_OldState, townstate) ->
  {reply, {wrong_message}, _OldState};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This save the game and produce xmlElement output or return 
%%      {state_not_paused} or return {no_permission}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({save, _AdminToken}, _From, _OldState) 
when is_record(_OldState, townstate) ->
  case ask_auth_server_for_admin_permission(_AdminToken,_OldState) of
    ok -> case is_paused(_OldState) of
              true -> {reply, {ok, 
                               xml_parser:build_xml_town_savegame(_OldState)},
                               _OldState};
              false -> {reply, {state_not_paused}, _OldState}
            end;
    fail -> {reply, {no_permission}, _OldState}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc: this is intended as a function to handle incoming passengers from
%%       various sources of transportation.
%%
%%       TODO: add population and infected population in case the border is 
%%       not closed also add 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({immigrate, _Immigrants}, _From, _OldState) 
when is_record(_OldState, townstate) ->
  case is_paused(_OldState) of
    true -> {reply, {paused}, _OldState};
    false -> State = update(_OldState),
             case _Immigrants#immigrants.connection of
               undefined     -> bla;
               {aircon, _}   -> case State#townstate.airport of
                                  open -> bla
                                end
             end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {joinplayer, _UserID, _Token}
%%      message. It checks if the Server is paused and replies {paused}.
%%      Otherwise it checks if the player is registered in the town, and
%%      if it is already registered replies with {player_already_registered}.
%%      Check if the user has privileges and the Token is correct, other
%%      wise return {no_permission}. Then add the player to the list of
%%      registered players and return {ok}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({joinplayer, _UserID, _Token },_From, _OldState) 
when is_record(_OldState, townstate), is_list(_Token) -> 
  case is_paused(_OldState) of
    true -> {reply, {paused}, _OldState};
    false ->
      State = update(_OldState),
      case is_player_registered( #playerid{ name = _UserID }, State) of
        false -> case ask_auth_server_for_user_permission(_Token,State) of
                   ok -> NewPlayer = #playerid{ name = _UserID },
                         RegisteredPlayers = State#townstate.players,
                         NewList  = lists:append(RegisteredPlayers, NewPlayer),
                         NewState = State#townstate{ players = 
                                                 lists:flatten([NewList]) },
                         {reply, {ok}, NewState};
                   fail -> {reply, {no_permission}, State}
                 end;
        true -> {reply, {player_already_registered}, State}
      end
  end;
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {leaveplayer, _UserID, _Token}
%%      message. It replies with {paused} if the State of the Server is
%%      paused. Otherwise it checks if the player is registered at the
%%      town. If not, {player_not_registered} is returned. Then it
%%      checks if the user Token is correct, otherwise it returns 
%%      {no_permission}. Then it deletes the player from the registered
%%      players and returns {ok}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({leaveplayer, _UserID, _Token},_From,_OldState) 
when is_record(_OldState, townstate), is_list(_Token)  -> 
  case is_paused(_OldState) of
    true -> {reply, {paused}, _OldState};
    false ->  State = update(_OldState),
              case is_player_registered( #playerid{ name = _UserID }, State) of
              true -> 
                case ask_auth_server_for_user_permission(_Token,State) of
                  ok   -> 
                    PlayerToDelete    = #playerid{ name = _UserID },
                    RegisteredPlayers = State#townstate.players,
                    NewList = lists:delete( PlayerToDelete, RegisteredPlayers),
                    NewState = State#townstate{ players = NewList },
                    {reply, {ok}, NewState};
                  fail -> 
                    {reply, {no_permission}, State}
                end;
              false -> {reply, {player_not_registered}, State}
            end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {longitude} message. It replies
%%      with {paused} if the server is paused, otherwise it returns
%%      {Longitude}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({longitude},_From,_OldState) when is_record(_OldState, townstate) -> 
  case is_paused(_OldState) of
    true -> {reply, {paused}, _OldState};
    false -> {reply, {_OldState#townstate.coordinate#coords.longitude}, 
              _OldState}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {latitude} message. It replies
%%      with {paused} if the server is paused, otherwise it returns
%%      {Latitude}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({latitude},_From,_State) when is_record(_State, townstate) -> 
  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    false -> {reply, {_State#townstate.coordinate#coords.latitude}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {coordinate} message. It replies
%%      with {paused} if the server is paused. Otherwise it replies with
%%      {#coords} a coordinate record in a erlang tuple.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({coordinates},_From,_State) when is_record(_State, townstate) -> 
  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    false -> {reply, {_State#townstate.coordinate}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {disconnect, _Destination, _ConType,
%%      _AdminToken} message. If the server is paused, it returns {paused}.
%%      Otherwise it updates the Serverstate and removes the Connection. 
%%      Removal of the connectin requires Admin privileges. Therefore the
%%      authentification server is interrogated if Admin privileges are
%%      available.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({disconnect, _Destination, _ConType, _AdminToken},_From,_OldState) 
  when is_record(_Destination, location), _ConType =:= air;
       is_record(_Destination, location), _ConType =:= road;
       is_record(_Destination, location), _ConType =:= ship -> 

  case is_paused(_OldState) of 
    true -> {reply, {paused}, _OldState};
    false -> 
      State = update(_OldState),
      Start = 
        #location{ name      = State#townstate.name,
                   latitude  = State#townstate.coordinate#coords.latitude,
                   longitude = State#townstate.coordinate#coords.longitude },
      {_,_,_,_,Distance} = calc:course( Start, _Destination ) ,
      ToDelete = case _ConType of
                   air  -> #aircon{  from       = Start,
                                     to         = _Destination, 
                                     distanceKm = Distance };
                   road -> #roadcon{ from       = Start,
                                     to         = _Destination,
                                     distanceKm = Distance };
                   ship -> #shipcon{ from       = Start,
                                     to         = _Destination,
                                     distanceKm = Distance }
                  end,
      case ask_auth_server_for_admin_permission(_AdminToken,State) of
        ok -> Connections = State#townstate.connections,
              NewConnections = lists:delete(ToDelete, Connections),
              NewState = State#townstate{ connections = NewConnections },
              {reply, {ok}, NewState};
        _  -> {reply, {no_permission}, State}
      end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {connect, _Destination, _ConType,
%%      _AdminToken} message. It replies creates a one way connection from one 
%%      town to another.  ConType can be aircon,roadcon or shipcon. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({connect, _Destination, _ConType, _AdminToken},_From,_OldState) 
  when is_record(_Destination, location), _ConType =:= air;
       is_record(_Destination, location), _ConType =:= road;
       is_record(_Destination, location), _ConType =:= ship-> 

  case is_paused(_OldState) of 
    true -> {reply, {paused}, _OldState};
    false -> 
      State = update(_OldState),
      Start = 
        #location{ name      = State#townstate.name,
                   latitude  = State#townstate.coordinate#coords.latitude,
                   longitude = State#townstate.coordinate#coords.longitude },
        case whereis( list_to_atom(_Destination#location.name) ) of
          undefined -> {reply, {invalid_destination}, State};
          _ -> {_,_,_,_,Distance} = calc:course( Start, _Destination ) ,
               NewConnection = 
                 case _ConType of
                   air  -> #aircon{  from       = Start,
                                     to         = _Destination, 
                                     distanceKm = Distance };
                   road -> #roadcon{ from       = Start,
                                     to         = _Destination,
                                     distanceKm = Distance };
                   ship -> #shipcon{ from       = Start,
                                     to         = _Destination,
                                     distanceKm = Distance }
                 end,
               case connection_exists(NewConnection, State) of
                 true  -> {reply, {connection_exists}, State};
                 false -> 
                   case ask_auth_server_for_admin_permission(_AdminToken,State) of
                     ok -> 
                       Connections = State#townstate.connections,
                       NewConnections = 
                           lists:flatten(
                             [lists:append( [Connections], [NewConnection])]
                           ),
                       NewState = 
                         State#townstate{ connections = NewConnections },
                       {reply, {ok}, NewState};
                     fail -> {reply, {no_permission}, State}
                   end
               end
        end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {pause, _Token} message. It checks
%%      if the _Token owner has admin privileges by contacting the 
%%      authentification server. If sends back a message {no_permission} to 
%%      the invoker. Otherwise it replies with {paused}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({pause, _Token },_From, _State) 
when is_record(_State, townstate), is_list(_Token) -> 
  case ask_auth_server_for_admin_permission(_Token,_State) of
    ok -> NewState = _State#townstate{ paused = true },
          {reply, {paused}, NewState};
    _ -> {noreply, {no_permission},  _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {pauseed} message. It checks
%%      It replies with {paused} or {running}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({is_paused},_From, _State) 
when is_record(_State, townstate)  -> 
  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    _    -> {reply, {not_paused}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {resume, _Token} message. It
%%      checks if the _Token has Admin Privileges by contacting the 
%%      authentification server. If not, it sends back {no_permission} to 
%%      the invoker. Otherwise it sets the server state to resumed and 
%%      sends back the message {resumed}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({resume, _Token}, _From, _OldState) 
when is_record(_OldState, townstate), is_list(_Token) -> 
  case ask_auth_server_for_admin_permission(_Token,_OldState) of
    ok -> State = update(_OldState#townstate{ paused = false }),
          {reply, {resumed}, State};
    _ -> {noreply, {no_permission}, _OldState}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {state, _AdminToken} message.
%%      It checks if the Token belongs to an Admin. If not, it replies
%%      with {no_permission} to the invoker, otherwise, it sends back
%%      {ok, _State}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({state, _AdminToken},_From,_OldState) 
when is_record(_OldState, townstate), is_list(_AdminToken)  -> 
  case ask_auth_server_for_admin_permission(_AdminToken,_OldState) of
    ok -> State = update(_OldState),
          {reply, {ok,State}, State};
    fail -> {reply, {no_permission}, _OldState}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {population}  message. It replies
%%      with {NumberOfPopulation} to the invoker.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({population},_From,_OldState) 
when is_record(_OldState, townstate)  -> 
  State = update(_OldState),
  {reply, {State#townstate.population}, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming {infected} message. It replies with
%%      with {NumberOfInfectedPopulaiton}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({infected},_From,_OldState) 
when is_record(_OldState, townstate)  -> 
  State = update(_OldState), 
  {reply, {State#townstate.infectedpopulation}, State};
handle_call(_,_,_OldState)  -> {reply,{unknown_message}, _OldState}.
handle_info(_,_)            -> {ok}.
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
%% @doc test to instantiate the server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

begin_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  StateMunich = 
  #townstate{ name            = "blubber",
              coordinate      = #coords
              { 
                latitude  = 48.144,
                longitude = 11.558
              },
              population      = 1300000,
              birthrate       = 20,
              infectionrate   = 5,
              lethality       = 30,
              infectedpopulation = 1000,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              players         = []
            },
  town:start(StateMunich),
  Result = case whereis(blubber) of
    undefined -> fail;
    _ -> ok
  end,
  {Coords} = town:coordinates(blubber),
  Expect = #coords{ latitude = 48.144, 
	            longitude = 11.558 },
  world:stop(world),
  town:stop(blubber),
  ?assert( Coords =:= Expect ),
  ?assert( Result =:= ok).

retrieve_state_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,Token}  = auth:login(auth,"Daniel","blabla"),
  StateMunich = 
  #townstate{ name            = "munich",
              coordinate      = #coords
              { 
                latitude  = 48.144,
                longitude = 11.558
              },
              population      = 1300000,
              birthrate       = 20,
              infectionrate   = 5,
              lethality       = 30,
              infectedpopulation = 1000,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },
  {ok,_}     = town:start(StateMunich),
  {ok,State} = town:state(munich,Token),
  world:stop(world),
  town:stop(munich),
  auth:stop(auth),

  ?assert(State#townstate.name          =:= StateMunich#townstate.name),
  ?assert(State#townstate.coordinate    =:= StateMunich#townstate.coordinate),
  ?assert(State#townstate.birthrate     =:= StateMunich#townstate.birthrate),
  ?assert(State#townstate.infectionrate =:= 
          StateMunich#townstate.infectionrate),
  ?assert(State#townstate.lethality     =:= StateMunich#townstate.lethality),
  ?assert(State#townstate.travelrate    =:= StateMunich#townstate.travelrate),
  ?assert(State#townstate.connections   =:= StateMunich#townstate.connections),
  ?assert(State#townstate.airport       =:= StateMunich#townstate.airport ),
  ?assert(State#townstate.roads         =:= StateMunich#townstate.roads ),
  ?assert(State#townstate.paused        =:= StateMunich#townstate.paused ),
  ?assert(State#townstate.players       =:= StateMunich#townstate.players ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test to add player and verify the state
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_player_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,Token}  = auth:login(auth,"Daniel","blabla"),
  StateMunich = 
  #townstate{ name            = "munich",
              coordinate      = #coords
              { 
                latitude  = 48.144,
                longitude = 11.558
              },
              population      = 1300000,
              infectedpopulation = 1000,
              birthrate       = 20,
              infectionrate   = 5,
              lethality       = 30,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },
  {ok,_}         = town:start(StateMunich),
  town:join(munich, "Horst" ,Token),
  {ok,State2} = town:state(munich,Token),



  world:stop(world),
  town:stop(munich),
  auth:stop(auth),
  ?assert(State2#townstate.name          =:= StateMunich#townstate.name),
  ?assert(State2#townstate.coordinate    =:= StateMunich#townstate.coordinate),
  ?assert(State2#townstate.birthrate     =:= StateMunich#townstate.birthrate),
  ?assert(State2#townstate.infectionrate =:= StateMunich#townstate.infectionrate),
  ?assert(State2#townstate.lethality     =:= StateMunich#townstate.lethality),
  ?assert(State2#townstate.travelrate    =:= StateMunich#townstate.travelrate),
  ?assert(State2#townstate.connections   =:= StateMunich#townstate.connections),
  ?assert(State2#townstate.airport       =:= StateMunich#townstate.airport ),
  ?assert(State2#townstate.roads         =:= StateMunich#townstate.roads ),
  ?assert(State2#townstate.paused        =:= StateMunich#townstate.paused ),
  ?assert(State2#townstate.players       =:= [#playerid{name = "Horst"}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc add and afterwardds remove a player and verify that the player 
%% has left the town
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_and_remove_player_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,Token}  = auth:login(auth,"Daniel","blabla"),

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
              infectedpopulation = 1000,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },
  %StateMunichNew = StateMunich#townstate{ players = 
  %                    [#playerid{ name = "Horst" }] },
  {ok,_}          = town:start(StateMunich),
  {ok,_}          = town:state(munich,Token),
  town:join(munich, "Horst" ,Token),
  {ok,State2}     = town:state(munich,Token),



  town:leave(munich, "Horst", Token),
  {ok,State3} = town:state(munich,Token),

  world:stop(world),
  town:stop(munich),
  auth:stop(auth),

  ?assert(State2#townstate.name          =:= StateMunich#townstate.name),
  ?assert(State2#townstate.coordinate    =:= StateMunich#townstate.coordinate),
  ?assert(State2#townstate.birthrate     =:= StateMunich#townstate.birthrate),
  ?assert(State2#townstate.infectionrate =:= 
                                        StateMunich#townstate.infectionrate),
  ?assert(State2#townstate.lethality     =:= StateMunich#townstate.lethality),
  ?assert(State2#townstate.travelrate    =:= StateMunich#townstate.travelrate),
  ?assert(State2#townstate.connections   =:= StateMunich#townstate.connections),
  ?assert(State2#townstate.airport       =:= StateMunich#townstate.airport ),
  ?assert(State2#townstate.roads         =:= StateMunich#townstate.roads ),
  ?assert(State2#townstate.paused        =:= StateMunich#townstate.paused ),
  ?assert(State2#townstate.players       =:=  [#playerid{name = "Horst"}]),

  ?assert(State3#townstate.name          =:= StateMunich#townstate.name),
  ?assert(State3#townstate.coordinate    =:= StateMunich#townstate.coordinate),
  ?assert(State3#townstate.birthrate     =:= StateMunich#townstate.birthrate),
  ?assert(State3#townstate.infectionrate =:= 
          StateMunich#townstate.infectionrate),
  ?assert(State3#townstate.lethality     =:= StateMunich#townstate.lethality),
  ?assert(State3#townstate.travelrate    =:= StateMunich#townstate.travelrate),
  ?assert(State3#townstate.connections   =:= StateMunich#townstate.connections),
  ?assert(State3#townstate.airport       =:= StateMunich#townstate.airport ),
  ?assert(State3#townstate.roads         =:= StateMunich#townstate.roads ),
  ?assert(State3#townstate.paused        =:= StateMunich#townstate.paused ),
  ?assert(State3#townstate.players       =:=  []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function is a test to add connections.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_connection_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,Token}  = auth:login(auth,"Daniel","blabla"),
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
              infectedpopulation = 1000,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },
    StateNuremberg = 
    #townstate{ name          = "nuremberg",
                coordinate    = #coords
                { 
                  latitude  = 49.461,
                  longitude = 11.062
                },
                population    = 1300000,
                infectedpopulation = 1000,
                connections   = [],
                infectionrate = 5.0,
                lethality     = 30.0,
                travelrate    = 400,
                airport       = open,
                roads         = open,
                players       = []
              },
  {ok,_}         = town:start(StateMunich),
  {ok,_}         = town:start(StateNuremberg),
  {ok,State}     = town:state(munich,Token),

  Loc = #location{ name = "nuremberg", latitude = 49.461, longitude = 11.062},
  Loc2 = #location{ name = "munich", latitude = 48.144, longitude = 11.558},

  {_,_,_,_,Distance} = calc:course(Loc2,Loc),

  town:connect(munich, Loc, road, Token),
  town:connect(munich, Loc, air, Token),
  town:connect(munich, Loc, ship, Token),


  NewState = State#townstate{ connections = [#roadcon{ 
                                                  from = Loc2,
                                                  to = Loc,
                                                  distanceKm = Distance
                                               },
                                             #aircon{ from = Loc2, 
                                                  to = Loc,
                                                  distanceKm = Distance},
                                             #shipcon { from = Loc2,
                                                  to = Loc,
                                                  distanceKm = Distance}
                                            ]},

  NewState2 = State#townstate{ connections = [#roadcon{ 
                                                  from = Loc2,
                                                  to = Loc,
                                                  distanceKm = Distance
                                               },
                                             #shipcon { from = Loc2,
                                                  to = Loc,
                                                  distanceKm = Distance}
                                            ]},
  {ok,NewState} = town:state(munich,Token),


  town:disconnect(munich, Loc, air, Token),

  {ok,Comparison} = town:state(munich,Token),

  world:stop(world),
  town:stop(munich),
  town:stop(nuremberg),
  auth:stop(auth),
  ?assert(NewState2#townstate.connections =:= Comparison#townstate.connections),
  ?assert(Distance =< 220),
  ?assert(Distance >= 100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc update_test
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_test() ->
  StartOfDayNative  = erlang:monotonic_time(),
  StartOfDaySec = erlang:convert_time_unit(StartOfDayNative, native,
                    seconds),
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
              infectedpopulation = 1234,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = [],
	      start_of_day_sec = StartOfDaySec - 
	      ?LENGTH_OF_DAY_HOUR * ?LENGTH_OF_HOUR_SEC * ?LENGTH_OF_SEC_SEC - 20
            },
  NewState = update( StateMunich),
  ?assert( NewState#townstate.population =/= StateMunich#townstate.population ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Test the infected interface
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

infected_test() ->
  Map         = ets:new(world,[public,set]),
  {ok, _}     = world:start(Map),
  {ok, _}     = auth:start("../auth_testfile3"),
  {ok,_}  = auth:login(auth,"Daniel","blabla"),
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
              infectedpopulation = 1234,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },
  {ok,_}         = town:start(StateMunich),
  {1234} = town:infected(munich),
  world:stop(world),
  town:stop(munich),
  auth:stop(auth).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test if the help function works
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help_test() ->
  town:help(),
  town:help(longitude).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test if it is possible to get population
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

population_test() ->
  Map         = ets:new(world,[public,set]),
  {ok, _}     = world:start(Map),
  {ok, _}     = auth:start("../auth_testfile3"),
  {ok,_}  = auth:login(auth,"Daniel","blabla"),
  StateMunich = 
  #townstate{ name            = "munich",
              coordinate      = #coords
              { 
                latitude      = 48.144,
                longitude     = 11.558
              },
              population      = 1300000,
              birthrate       = 20.0,
              infectionrate   = 5.0,
              lethality       = 30.0,
              infectedpopulation = 1234,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },
  {ok,PID}         = town:start(StateMunich),
  {Number} = town:population(PID),
  world:stop(world),
  town:stop(munich),
  auth:stop(auth),
  ?assert( Number =:= 1300000 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test if it is possible to heal
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

heal_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,Token}  = auth:login(auth,"Daniel","blabla"),

  % player must be at the same position as town
  PlayerState = #playerstate{ name = daniel, 
                coordinate = #coords{ latitude = 48.144, longitude = 11.558 },
                location = undefined,
                activity = undefined,
                paused = false},
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
              infectedpopulation = 1234,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },
  {ok,_}         = town:start(StateMunich),
  {ok, PlayerPid}        = player:start(PlayerState),
  {ok}           = player:join( daniel, munich, town, Token ),
  Result = player:heal(PlayerPid, Token),
  world:stop(world),
  town:stop(munich),
  auth:stop(auth),
  player:stop(daniel),
  ?assert( Result =:= {3} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc verify that it is impossible to heal if you have the wrong character
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

heal_wrong_character_test() ->
  Map        = ets:new(world,[public,set]),
  {ok, _}    = world:start(Map),
  {ok, _}    = auth:start("../auth_testfile3"),
  {ok,Token} = auth:login(auth,"Fabian","blabla"),

  % player must be at the same position as town
  PlayerState = #playerstate{ name = daniel, 
                coordinate = #coords{ latitude = 48.144, longitude = 11.558 },
                location   = undefined,
                activity   = undefined,
                paused     = false},
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
              infectedpopulation = 1234,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },
  {ok,_}          = town:start(StateMunich),
  {ok, PlayerPid} = player:start(PlayerState),
  {ok}            = player:join( daniel, munich, town, Token ),
  Result          = player:heal(PlayerPid, Token),
  world:stop(world),
  town:stop(munich),
  auth:stop(auth),
  player:stop(daniel),
  ?assert( Result =:= {wrong_character} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This is a test generator function. It changes the default of 5 seconds
%% test timeout to 40 seconds
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decreasing_population_test() ->
    {timeout, 40, 

  fun() ->

  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,Token}  = auth:login(auth,"Daniel","blabla"),
  StateMunich = 
  #townstate{ name            = "munich",
              coordinate      = #coords
              { 
                latitude  = 48.144,
                longitude = 11.558
              },
              population      = 1000,
              infectedpopulation = 1,
              birthrate       = 0.0,
              infectionrate   = 3.0,
              lethality       = 1.0,
              travelrate      = 0,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },

  {ok,_}         = town:start(StateMunich),

  timer:sleep(
              ?LENGTH_OF_DAY_HOUR * 
	      ?LENGTH_OF_HOUR_SEC * 
              ?LENGTH_OF_SEC_SEC * 
	      2 * 100
	     ),
  {ok,NewState}         = town:state(munich, Token),
  world:stop(world),
  town:stop(munich),
  auth:stop(auth),
  ?assert(NewState#townstate.population < 1000 )
  end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc unkown message test
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unkown_message_test() ->

  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,Token}  = auth:login(auth,"Daniel","blabla"),
  StateMunich = 
  #townstate{ name            = "munich",
              coordinate      = #coords
              { 
                latitude  = 48.144,
                longitude = 11.558
              },
              population      = 1000,
              infectedpopulation = 1,
              birthrate       = 0.0,
              infectionrate   = 3.0,
              lethality       = 1.0,
              travelrate      = 0,
              connections     = [],
              airport         = open,
              roads           = open,
              paused          = false,
              players         = []
            },

  {ok,TPID}         = town:start(StateMunich),
  {Result}       = auth:login(TPID,"Daniel","blabla"),
  world:stop(world),
  town:stop(munich),
  auth:stop(auth),
  ?assert( Result =:= unknown_message ).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
