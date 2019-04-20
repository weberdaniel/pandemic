%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HIGH PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LOW PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: implement the possibility to travel between towns
%% TODO: implement the possibility to interact with facilities
%% TODO: implement a unit test to travel from one town to another 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Module Definition        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(player).
-behaviour(gen_server).
-export([start/1, start_link/1, terminate/2, pause/2, help/0,
         init/1, handle_info/2, handle_cast/2, handle_call/3,
         code_change/3, leave/4, join/4, travel/5, resume/2,
         latitude/2, longitude/2, coordinates/2, location/2,
         stop/1, save/2, heal/2, paused/1, level/2  ]).  

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {travel, _From, _To, 
%%      air, _Token} message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

travel( _PID, _From, _To, air,  _Token) when is_list(_Token) ->
  gen_server:call(_PID, {travel, _From, _To, air, _Token});
travel( _PID, _From, _To, road,  _Token) when is_list(_Token) ->
  gen_server:call(_PID, {travel, _From, _To, road, _Token});
travel( _PID, _From, _To, ship,  _Token) when is_list(_Token) ->
  gen_server:call(_PID, {travel, _From, _To, ship, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface sends a synchronous {_PID, _PIDtoEnter, town, _Token}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

join( _PID, _PIDtoEnter, town, _Token) when is_list(_Token) ->
  gen_server:call(_PID, {join, town, _PIDtoEnter, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {_PID, _Token} message
%%      to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

heal( _PID, _Token ) when is_list(_Token) ->
  gen_server:call(_PID, {heal, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {leave, town, _PIDtoleave,
%%      _Token} message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leave( _PID, _PIDtoLeave, town, _Token) when is_list(_Token) ->
  gen_server:call(_PID, {leave, town, _PIDtoLeave, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {save, _AdminToken}
%%      message to the server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save( _PID, _AdminToken) when is_list(_AdminToken) ->
  gen_server:call(_PID, {save, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {latitude, _Token}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

latitude( _PID, _Token) when is_list(_Token) ->
  gen_server:call(_PID, {latitude, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {longitude, _Token}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

longitude( _PID, _Token) when is_list (_Token) ->
  gen_server:call(_PID, {longitude,  _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {level, _Token}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

level( _PID, _Token) when is_list (_Token) ->
  gen_server:call(_PID, {level,  _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {coordinates, _Token}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

coordinates( _PID, _Token) when is_list (_Token) ->
  gen_server:call(_PID, {coordinates,  _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {location, _Token}
%%      message to the server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

location( _PID, _Token) when is_list(_Token) ->
  gen_server:call(_PID, {location,  _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {pause, _AdminToken}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause( _PID, _AdminToken) when is_list(_AdminToken) ->
  gen_server:call(_PID, {pause ,  _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {paused}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

paused( _PID) ->
  gen_server:call(_PID, {is_paused}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface functions sends a synchronous {resume, _AdminToken}
%%      message to the process located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resume( _PID, _AdminToken) when is_list(_AdminToken) ->
  gen_server:call(_PID, {resume,  _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sens an asynchronous {stop} message to
%%      the Server located at _PID
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop( _PID ) ->
  gen_server:cast(_PID, {stop}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function prints a help text on all available interface
%%      functions.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help( ) ->
  io:format("player:join(PID, PIDtojoin, town, Token)~n"),
  io:format("player:leave(PID, PIDtoleave, town, Token)~n"),
  io:format("player:travel(PID, From, To, air|road|ship, Token)~n"),
  io:format("player:location(PID, Token)~n"),
  io:format("player:pause(PID, AdminToken)~n"),
  io:format("player:resume(PID, AdminToken)~n"),
  io:format("player:latitude(PID, Token)~n"),
  io:format("player:longitude(PID, Token)~n"),
  io:format("player:save(PID, AdminToken)~n"),
  io:format("player:coordinates(PID, Token)~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper Functions    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function checks whether the State of the Player process is
%%      "paused" or not. It either returns the atom "true" or "false".
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_paused( _State ) when is_record(_State, playerstate) ->

  case _State#playerstate.paused of
    true -> true;
    _    -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function asks the authentification server, with the name "auth",
%%      whether the _Token has admin_permissions or not. It returns either
%%      the atom "ok","fail".
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ask_auth_server_for_admin_permission(_Token) when is_list(_Token) ->

  AuthServerPID = whereis(auth),
  Token         =  _Token,

  case AuthServerPID of
    undefined -> fail;
    _ -> case auth:verify_admin(AuthServerPID, Token) of
           {verify_admin_failed} -> fail;
           {verify_admin_ok, _}  -> ok
         end
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function asks the authentification server, named with the atom
%%      "auth", whether the _Token belongs to a user or not. It returns either
%%      the atom "fail" or "ok".
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ask_auth_server_for_user_permission(_Token,_State) 
  when is_record(_State, playerstate), is_list(_Token) ->

  AuthServerPID = whereis(auth),
  Token         =  _Token,

  case AuthServerPID of
    undefined -> fail;
    _ -> case auth:verify(AuthServerPID, Token) of
           {verify_failed} -> fail;
           {verify_ok, _}  -> ok
         end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Server Initialization %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Start the Player Process by handing over a playerstate record. The
%%      server is started via gen_server:start(...) , which means that
%%      no link is established between the calling process and the new player
%%      process. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start( _Playerstate ) when is_record(_Playerstate, playerstate) ->

  gen_server:start({local,
    list_to_atom(lists:flatten(io_lib:format("~s",
                 [_Playerstate#playerstate.name]))) },
                 ?MODULE, [_Playerstate],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Start the Player Process by handing over a playerstate record. The
%%      server is started via gen_server:start_link(...) , which means that
%%      a link is established between the calling process and the new player
%%      process. Links result to the fact, that if one of the processes dies,
%%      then, the other dies as well.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link( _Playerstate ) when is_record(_Playerstate, playerstate) ->

  gen_server:start_link({local,
    list_to_atom(lists:flatten(io_lib:format("~s",
                 [_Playerstate#playerstate.name]))) },
    ?MODULE, [_Playerstate],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Initialization routine of the generic server behaviour. Hand over the
%%      state of a player.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([_Playerstate]) when is_record(_Playerstate, playerstate) ->
  rand:seed(exs1024,{erlang:phash2([node()]),
            erlang:monotonic_time(),
            erlang:unique_integer()}),
  Lat  = _Playerstate#playerstate.coordinate#coords.latitude,
  Long = _Playerstate#playerstate.coordinate#coords.longitude,
  Response   = world:add( whereis(world), {Lat, Long, 
                   _Playerstate#playerstate.name, player, self()  }),
  Response = {ok, {Lat,Long, _Playerstate#playerstate.name, player, self()}},
  {ok, _Playerstate}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Server Message Handling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Receive an incoming synchronous {heal, _Token} message. To perform a 
%%      heal, the function town:heal(...) is used, to interact with the town 
%%      process.  To make the town:heal(...) function work, the character of 
%%      your player must be a medic. This is verified by the town:heal(...) 
%%      function. Otherwise town:heal(...) returns {wrong_character}. Also
%%      the player may not have healed already on this day, because healing
%%      may only performed once per day. So if no more heals are available,
%%      {no_more_heal} is returned by town:heal(...).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({heal, _Token},_From,_State) 
when is_record(_State, playerstate), is_list(_Token) ->
  PlayerLocation = _State#playerstate.location,
  case is_record(PlayerLocation, location) of
    true -> 
      LocationName   = PlayerLocation#location.name,
      LocationPID    = whereis(LocationName),
      case is_process_alive(LocationPID) of
        true ->
          case is_paused(_State) of
            true  -> {reply, {paused}, _State};
            false -> Reply = town:heal(LocationPID, _Token),
	             {reply, Reply, _State}
          end;
	         %TODO: eventuelly reset internal state if this happens.
	         %      obviously location is wrong (??)
        false -> {reply, {not_in_town}, _State}
      end;
    false -> {reply, {not_in_town}, _State}
      end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels a incoming synchonous {join, town, _PID, _Token} 
%%      message. It checks if the server state is paused and replys {paused}. 
%%      Otherwise it obtains the latitude and longitude of the town to join 
%%      and validates that the player character is at the correct position 
%%      to join the town. Then it calls the town:join( ...) function to join 
%%      the town. The player then is registered at the process representing 
%%      the town. There he can for example heal population, but only if he
%%      is registered.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({join,town,_PID,_Token},_From,_State) 
when is_record(_State, playerstate), is_atom(_PID) -> 

  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    _    -> {TownLat}  = town:latitude(_PID),
	    {TownLong} = town:longitude(_PID),

            TownLocation = #location{
                             name      = _PID,
                             latitude  = TownLat,
                             longitude = TownLong
                           },

	    PlayerCoords = _State#playerstate.coordinate,
            case TownLat =:= PlayerCoords#coords.latitude of
	      true -> 
	        case TownLong =:= PlayerCoords#coords.longitude of
	          true -> R = town:join(_PID, 
                                _State#playerstate.name, _Token),
                          NewState = _State#playerstate{ 
                              location = TownLocation
                          },
                          {reply, R, NewState};
		  _ -> {reply, {wrong_coordinate}, _State}
		end;
              _ -> {reply, {wrong_coordinate}, _State}
            end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {leave, town, PID, 
%%      _Token} message. This means that player will leave a town. note this 
%%      does not validate the _Token, because the town server is going to 
%%      validate the token. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({leave,town,_PID,_Token},_From,_State) 
when is_record(_State,playerstate), is_list(_Token) -> 

  % check if the state of the player is paused, if yes, then no operations
  % are possible. Else, the town server will be called to deregister the
  % player. Of course this can also fail if for example, the player is not
  % in the town or if the town is paused.

  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    _ ->    R = town:leave(_PID, _State#playerstate.name, _Token),
            {reply, R, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {pause, _AdminToken} 
%%      message. It sets the paused state of the server to true and returns 
%%      {paused}, if the _AdminToken is correct. The _AdminToken is verfied 
%%      by the authentification server. Otherwise {no_permission} is returned.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({pause,_AdminToken},_From,_State) 
when is_record(_State,playerstate), is_list(_AdminToken) -> 

  case ask_auth_server_for_admin_permission(_AdminToken) of
    ok -> NewState = _State#playerstate{ paused = true },
             {reply, {paused}, NewState};
    fail -> {reply, {no_permission}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {paused} 
%%      message. It returns the paused state of the server. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({is_paused},_From,_State) 
when is_record(_State,playerstate) -> 

  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    _    -> {reply, {not_paused}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {save,_AdminToken} 
%%      message.  It saves the state of the process as an XML text. It Needs 
%%      authentification with an _AdminToken and the _AdminToken is verfied
%%      by talking to the authentification server. The function returns 
%%      {ok,XML} or {not_paused} or {no_permission} 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({save,_AdminToken},_From,_State) 
when is_record(_State,playerstate), is_list(_AdminToken)  -> 

  case ask_auth_server_for_admin_permission(_AdminToken) of
    ok -> case is_paused(_State) of
               true  -> XML = xml_parser:build_xml_player_savegame(_State),
                        {reply, {ok, XML}, _State};
               false -> {reply, {not_paused}, _State}
             end;
    fail -> {reply, {no_permission}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchrnous {resume,_AdminToken} 
%%      message. It resets the pause state to resumed and returns {resumed} or
%%      {no_permission}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({resume,_AdminToken},_From,_State) 
when is_record(_State,playerstate), is_list(_AdminToken) -> 

  case ask_auth_server_for_admin_permission(_AdminToken) of
    ok -> NewState = _State#playerstate{ paused = false },
             {reply, {resumed}, NewState};
    fail -> {reply, {no_permission}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc currently this function does nothing.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({travel,_From, _To, _Transport, _Token},_From,_State) 
when is_record(_State,playerstate),
     is_record(_From, location),
     is_record(_To,   location),
     is_atom(_Transport),
     is_list(_Token) ->

  {reply,{ok},_State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchonous {laitutde, _Token} 
%%      message.  It validates the _Token. If the validation fails, it 
%%      returns the message {no_permission}. If validation succeeds, then 
%%      the latitude of the player is replied.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({latitude,_Token},_From,_State) 
when is_record(_State,playerstate) -> 
  case ask_auth_server_for_user_permission(_Token,_State) of
    ok ->  Coordinate = _State#playerstate.coordinate,
           Lat = Coordinate#coords.latitude,
           {reply, {Lat}, _State};
    fail -> {reply, {no_permission}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {longitude, _Token} 
%%      message. It validates the _Token. If the validation fails, it returns 
%%      the message {no_permission}. If validation succeeds, then the 
%%      longitude of the player is replied.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({longitude,_Token},_From,_State) 
when is_record(_State,playerstate) -> 
  case ask_auth_server_for_user_permission(_Token,_State) of
    ok -> Coordinate = _State#playerstate.coordinate,
          Long = Coordinate#coords.longitude,
          {reply, {Long}, _State};
    fail -> {reply, {no_permission}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {level, _Token} 
%%      message. It validates the _Token. If the validation fails, it returns 
%%      the message {no_permission}. If validation succeeds, then the 
%%      level of the player is replied.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({level,_Token},_From,_State) 
when is_record(_State,playerstate) -> 
  case ask_auth_server_for_user_permission(_Token,_State) of
    ok -> Level = _State#playerstate.level,
          {reply, {Level}, _State};
    fail -> {reply, {no_permission}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchonous {location,_Token} message.
%%      It validates the token and on fail returns {no_permission}. In case
%%      of success the current location of the player is replied. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({location,_Token},_From,_State) 
when is_record(_State,playerstate) -> 
  case ask_auth_server_for_user_permission(_Token,_State) of
    ok -> Location = _State#playerstate.location,
          {reply, {Location}, _State};
    fail -> {reply, {no_permission}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchonous {coordinates,_Token} 
%%      message.  It returns either {no_permission} or it replies the current 
%%      coordinates of the player.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({coordinates,_Token},_From,_State) 
when is_record(_State,playerstate) -> 
  case ask_auth_server_for_user_permission(_Token,_State) of
    ok -> Coordinates = _State#playerstate.coordinate,
          {reply, {Coordinates}, _State};
    fail -> {reply, {no_permission}, _State}
  end;


handle_call(_,_,_State)     -> io:format("unknonwn call...~n"),
	                       {noreply, _State}.
handle_info(_,_)            -> {ok}.
handle_cast({stop}, State)  -> {stop, normal, State}.
code_change(_,_,_)          -> {ok}.
terminate  (_,_)            -> {ok}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       Unit Tests         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc unit test to register the player at a town process 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

register_player_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,T}  = auth:login(auth,"Daniel","blabla"),

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
  PlayerState =
  #playerstate{ name = daniel, 
                coordinate = #coords{ latitude = 49.461, longitude = 11.062 },
                location = undefined,
                activity = undefined,
                paused = false
  },
  {ok,_}         = town:start(StateNuremberg),
  {ok, _}        = player:start(PlayerState),
  {ok}           = player:join( daniel, nuremberg, town, T ),
  {ok, State}    = town:state( nuremberg, T ),
  Head           = hd(State#townstate.players),
  Head           = #playerid{ name = daniel },
  world:stop(world),
  town:stop(nuremberg),
  auth:stop(auth),
  player:stop(daniel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Test to unregister a player from a town
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unregister_player_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,T}  = auth:login(auth,"Daniel","blabla"),

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
  PlayerState =
  #playerstate{ name = daniel, 
                coordinate = #coords{ latitude = 49.461, 
                             longitude = 11.062 },
                location = undefined,
                activity = undefined,
                paused = false
  },
  {ok,_}         = town:start(StateNuremberg),
  {ok, _}        = player:start(PlayerState),
  {ok}           = player:join( daniel, nuremberg, town, T ),
  {ok, State}    = town:state( nuremberg, T ),
  Head           = hd(State#townstate.players),
  Head           = #playerid{ name = daniel },
  {ok}           = player:leave( daniel, nuremberg, town, T ),
  {ok, State2}   = town:state( nuremberg, T ),
  []             = State2#townstate.players,
  world:stop(world),
  town:stop(nuremberg),
  auth:stop(auth),
  player:stop(daniel).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Test to call the help
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help_test() ->
  player:help().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Test to check the paused state
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause_and_resume_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,T}  = auth:login(auth,"Daniel","blabla"),
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
  PlayerState =
  #playerstate{ name = daniel, 
                coordinate = #coords{ latitude = 49.461, 
                             longitude = 11.062 },
                location = undefined,
                activity = undefined,
                paused = false
  },
  {ok,_}         = town:start(StateNuremberg),
  {ok, PlayerPid}        = player:start(PlayerState),
  Result = player:pause(PlayerPid, T),
  ?assert( Result =:= {paused} ),
  Result2 = player:resume(PlayerPid, T),
  ?assert( Result2 =:= {resumed} ),
  world:stop(world),
  town:stop(nuremberg),
  auth:stop(auth),
  player:stop(daniel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Test to check the paused state
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_not_in_town_heal_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,T}  = auth:login(auth,"Daniel","blabla"),
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
  PlayerState =
  #playerstate{ name = daniel, 
                coordinate = #coords{ latitude = 49.461, 
                             longitude = 11.062 },
                location = undefined,
                activity = undefined,
                paused = false
  },
  {ok,_}         = town:start(StateNuremberg),
  {ok, PlayerPid}       = player:start(PlayerState),
  Result = player:heal(PlayerPid, T),
  ?assert( Result =:= {not_in_town} ),
  world:stop(world),
  town:stop(nuremberg),
  auth:stop(auth),
  player:stop(daniel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Test to check the paused state
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

latitude_test() ->
  Map = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,T}  = auth:login(auth,"Daniel","blabla"),
  PlayerState =
  #playerstate{ name = daniel, 
                coordinate = #coords{ latitude = 49.461, 
                             longitude = 11.062 },
                location = undefined,
                activity = undefined,
                paused = false },
  {ok, PlayerPid}       = player:start(PlayerState),
  {Value} = player:latitude(PlayerPid,T),
  ?assert( Value =:= 49.461 ),
  world:stop(world),
  auth:stop(auth),
  player:stop(daniel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Test to check the paused state
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

longitude_test() ->
  Map     = ets:new(world,[public,set]),
  {ok, _} = world:start(Map),
  {ok, _} = auth:start("../auth_testfile3"),
  {ok,T}  = auth:login(auth,"Daniel","blabla"),
  PlayerState =
  #playerstate{ name       = daniel, 
                coordinate = #coords{ latitude  = 49.461, 
                                      longitude = 11.062 },
                location   = undefined,
                activity   = undefined,
                paused     = false },
  {ok, PlayerPid}          = player:start(PlayerState),
  {Value}                  = player:longitude(PlayerPid,T),
  ?assert( Value =:= 11.062 ),
  world:stop(world),
  auth:stop(auth),
  player:stop(daniel).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
