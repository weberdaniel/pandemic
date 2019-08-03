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
%% TODO: implement update function to calculate new coordinates
%% TODO: implement arrival at town
%% TODO: implement start of travel
%% TODO: implement XML save function
%% TODO: implement read from XML and create vehicles in supersvisor

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LOW PRIORITY TASKS %%&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Module Definition        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(vehicle).
-behaviour(gen_server).
-export([start_link/7, start_link/1, start/7, start/1, terminate/2, pause/2, 
	 help/0, init/1, handle_info/2, handle_cast/2, handle_call/3,
         code_change/3, resume/2, latitude/1, longitude/1, 
         stop/1, save/2, add_npcs/3, npcs/1, target/1, target/2,
         start_travel/1, travel/1, pause_travel/1 ]).  

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
%% @doc save the state of this process. Returns an XML record that can
%%      be stored to a file. Save operation is only permitted in the paused
%%      mode.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save( _PID, _AdminToken) when is_list(_AdminToken) ->
  gen_server:call(_PID, {save, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface functions sends the synchronous message
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

latitude( _PID )  ->
  gen_server:call(_PID, {latitude}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc get state of travel: halt or travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

travel( _PID )  ->
  gen_server:call(_PID, {travel}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Set the target to where the vehicle shall travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

target( _PID, _Location ) when is_record(_Location, location) ->
  gen_server:call(_PID, {set_target, _Location}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Set the target to where the vehicle shall travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
target( _PID ) ->
  gen_server:call(_PID, {get_target }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Start the travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_travel( _PID ) ->
  gen_server:call(_PID, {start_travel}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Pause the travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause_travel( _PID ) ->
  gen_server:call(_PID, {pause_travel}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc get passengers of the vehicle. Return {Total, Infected}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

npcs( _PID )  ->
  gen_server:call(_PID, {npcs}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc get longitude of the vehicle.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

longitude( _PID )  ->
  gen_server:call(_PID, {longitude}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc add population
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_npcs( _PID, _Total, _Infected ) when is_number(_Total), 
					      is_number(_Infected) ->
  gen_server:call( _PID, {add_npcs, _Total, _Infected} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc pause the process. This is helpful to save the state while in pause
%%      mode.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause( _PID, _AdminToken) when is_list(_AdminToken) ->
  gen_server:call(_PID, {pause ,  _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc resume the process
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resume( _PID, _AdminToken) when is_list(_AdminToken) ->
  gen_server:call(_PID, {resume,  _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc stop the process
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop( _PID ) ->
  gen_server:cast(_PID, {stop}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc help text
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help( ) ->
  io:format("vehicle:pause(PID, AdminToken)~n"),
  io:format("vehicle:resume(PID, AdminToken)~n"),
  io:format("vehicle:latitude(PID, Token)~n"),
  io:format("vehicle:longitude(PID, Token)~n"),
  io:format("vehicle:save(PID, AdminToken)~n").

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
%% @doc calculate new position due to traveling.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update( _State ) when is_record( _State, vehiclestate)  ->
  Target             = _State#vehiclestate.target,

  % only travel if not paused

  case is_paused(_State) of
    true ->
      CurrentTimeNative  = erlang:monotonic_time(),
      CurrentTimeSec     = erlang:convert_time_unit(CurrentTimeNative, 
                           native, seconds),
      _State#vehiclestate{ coordinateTimeSec = CurrentTimeSec};
    _ -> 

  % only travel if the activity is not set to halted

  case _State#vehiclestate.activity of
    halted -> 
      CurrentTimeNative  = erlang:monotonic_time(),
      CurrentTimeSec     = erlang:convert_time_unit(CurrentTimeNative, 
                           native, seconds),
      _State#vehiclestate{ coordinateTimeSec = CurrentTimeSec};
    _ ->

  % only travel if a Target exists
  case Target of
	 [] -> _State;
	 undef -> _State; 
	 _ -> case is_number(_State#vehiclestate.coordinateTimeSec) of
                true -> 
                  case is_number(_State#vehiclestate.speedKmH) of
                    true ->
		      OldCoords        = _State#vehiclestate.coordinate,
                      OldCoordsTimeSec = _State#vehiclestate.coordinateTimeSec,
                      SpeedKmH           = _State#vehiclestate.speedKmH,
                      CurrentTimeNative  = erlang:monotonic_time(),
                      CurrentTimeSec     = erlang:convert_time_unit(CurrentTimeNative, 
							 native, seconds),
                      DeltaTimeSec       = CurrentTimeSec - OldCoordsTimeSec,
                      DistanceKm         = DeltaTimeSec/60.0 * SpeedKmH,
                      {NewLat, NewLong}  = calc:great_circle_waypoint(OldCoords, Target,
						       DistanceKm),
                      NewState = _State#vehiclestate{ 
	                 coordinate = #coords{ latitude   = NewLat,
				            longitude  = NewLong },
	                 coordinateTimeSec = CurrentTimeSec 
	              },
                      NewState;
	            false -> _State
                  end;
		_ -> _State
              end
  end
  end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc check if the state is paused. return true or false.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_paused( _State ) when is_record(_State, vehiclestate) ->
  case _State#vehiclestate.paused of
    true -> true;
    _ -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc is halted
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_halted( _State ) when is_record(_State, vehiclestate) ->
  case _State#vehiclestate.activity of
    halted -> true;
    _ -> false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc ask the auth server for admin permission
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ask_auth_server_for_admin_permission(_Token) ->
  AuthServerPID = whereis(auth),
  Token         =  _Token,

  case auth:verify_admin(AuthServerPID, Token) of
    {verify_admin_failed} -> fail;
    {verify_admin_ok, _} -> ok
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
%% @doc start the server ( do not register with process id)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link( _Id, _Passengers, _Infected, _Type, _SpeedKmH, _Coord, _Location ) 
  when is_list(_Id), is_integer(_Passengers), is_integer(_Infected),
       is_number(_SpeedKmH), is_record(_Coord, coords), 
       is_record(_Location, location) ->

  % NOTE: The erlang monotonic time is strictly increase time that increases
  %       strictly monotonically within all Erlang nodes. This makes it also
  %       possible to determine the order of events.
	
  TimeNative = erlang:monotonic_time(),
  TimeSec    = erlang:convert_time_unit(TimeNative, native, seconds),

  Vehiclestate = #vehiclestate{ id                 = _Id,
				passengers         = _Passengers,
				infectedpassengers = _Infected,
				type               = _Type,
				location           = _Location,
				coordinate         = _Coord,
				speedKmH           = _SpeedKmH,
				coordinateTimeSec  = TimeSec,
				activity           = halted,
				players            = [],
				path               = [],
				paused             = false },

  gen_server:start_link({local, list_to_atom(lists:flatten(io_lib:format("~s",
                 [_Id]))) }, ?MODULE, [Vehiclestate],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start the server ( do not register with process id)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start( _Id, _Passengers, _Infected, _Type, _SpeedKmH, _Coord, _Location ) 
  when is_list(_Id), is_integer(_Passengers), is_integer(_Infected),
       is_number(_SpeedKmH), is_record(_Coord, coords), 
       is_record(_Location, location) ->

  % NOTE: The erlang monotonic time is strictly increase time that increases
  %       strictly monotonically within all Erlang nodes.

  TimeNative = erlang:monotonic_time(),
  TimeSec    = erlang:convert_time_unit(TimeNative, native, seconds),

  Vehiclestate = #vehiclestate{ id = _Id,
				passengers = _Passengers,
				infectedpassengers = _Infected,
				type = _Type,
				location = _Location,
				coordinate = _Coord,
				speedKmH = _SpeedKmH,
				coordinateTimeSec = TimeSec,
				activity = halted,
				players = [],
				path = [],
				paused = false },

  gen_server:start({local, list_to_atom(lists:flatten(io_lib:format("~s",
                 [_Id]))) }, ?MODULE, [Vehiclestate],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start the server ( do not register with process id)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link( _Vehiclestate ) when is_record(_Vehiclestate, vehiclestate) ->
  gen_server:start_link({local,
    list_to_atom(lists:flatten(io_lib:format("~s",
                 [_Vehiclestate#vehiclestate.id]))) },
    ?MODULE, [_Vehiclestate],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start the server ( do not register with process id)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start( _Vehiclestate ) when is_record(_Vehiclestate, vehiclestate) ->
  gen_server:start({local, list_to_atom(lists:flatten(io_lib:format("~s",
                 [_Vehiclestate#vehiclestate.id]))) },
    ?MODULE, [_Vehiclestate],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start the server ( do not register with process id)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([_Vehiclestate]) when is_record(_Vehiclestate, vehiclestate) ->
  {ok, _Vehiclestate}.

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
%% @doc start_travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({travel},_From,_State) when is_record(_State, vehiclestate) -> 
  case is_paused(_State) of
    true  -> {reply, {paused}, _State};
    _     -> NewState = update(_State),
             {reply,{NewState#vehiclestate.activity},NewState}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start_travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({start_travel},_From,_State) when is_record(_State, vehiclestate) -> 
  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    _    -> S2 = _State#vehiclestate{ activity =  travel},
            NewState = update(S2),
            {reply,{ok},NewState}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start_travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({pause_travel},_From,_State) when is_record(_State, vehiclestate) -> 
  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    _    -> S2 = _State#vehiclestate{ activity =  halt},
            NewState = update(S2),
            {reply,{_State#vehiclestate.coordinate#coords.latitude},  NewState}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc return latitude of vehicle
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({latitude},_From,_State) when is_record(_State, vehiclestate) -> 
  NewState = update(_State),
  {reply,{_State#vehiclestate.coordinate#coords.latitude},  NewState};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc return latitude of vehicle
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({set_target, _Location},_From,_State) when is_record(_State, vehiclestate),
						       is_record(_Location, location) -> 
  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    _    ->  NewState = update(_State),
             % TODO: Implement verification of location
             NState = NewState#vehiclestate{ target = _Location },
             {reply,{ok},NState}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc return latitude of vehicle
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({get_target},_From,_State) when is_record(_State, vehiclestate) -> 
  NewState = update(_State),
  {reply,_State#vehiclestate.target,NewState};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc return the number of total passengers as well as the number of
%%      infected passengers
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({npcs},_From,_State) when is_record(_State, vehiclestate) -> 
  NewState = update(_State),
  {reply,{_State#vehiclestate.passengers, 
	  _State#vehiclestate.infectedpassengers},  NewState};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc return longitude of vehicle
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({longitude},_From,_State) when is_record(_State, vehiclestate) -> 
  NewState = update(_State),
  {reply, {_State#vehiclestate.coordinate#coords.longitude}, NewState};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc return {paused} or {not_paused} depending on process state.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({is_paused},_From,_State) when is_record(_State, vehiclestate) -> 
  case is_paused(_State) of
    true -> {reply, {paused}, _State};
    _    -> {reply, {not_paused}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc add passengers
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({add_npcs,_Total,_Infected},_From,_State)
  when is_record(_State, vehiclestate), is_integer(_Total), 
       is_integer(_Infected) -> 

  case is_paused(_State) of
    true  -> {reply, {paused}, _State};
    false ->  case is_halted(_State) of
	        true -> Passengers    = _State#vehiclestate.passengers,
	                Infected      = _State#vehiclestate.infectedpassengers,
	                NewPassengers = Passengers+_Total,
                        NewInfected   = _Infected + Infected,
	                NewState      = _State#vehiclestate{
			                   passengers         = NewPassengers,
			                   infectedpassengers = NewInfected
			                },
	                {reply, {ok}, NewState};
                 false -> {reply, {traveling}, _State}
	       end
   end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc return _State.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call(_,_,_State)     -> {noreply, _State}.
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
%% @doc test if the correct latitude is returned
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

latitude_test() ->
  V = 
  #vehiclestate{ 
                 id         = "B50" ,
	         coordinate = #coords{ latitude = 22, longitude = 33},
	         location           = [], 
	         paused             = false ,
		 type               = aircraft,
		 speedKmH           = 40,
		 passengers         = 200,
		 infectedpassengers = 0
	       },
  {ok, PID} = vehicle:start(V),
  Latitude = vehicle:latitude(PID),
  vehicle:stop(PID),
  Latitude =:= 22.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test if the correct longitude is returned
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

longitude_test() ->
  V = 
  #vehiclestate{ 
                 id         = "B50" ,
	         coordinate = #coords{ latitude = 22, longitude = 33},
	         location   = [], 
	         paused     = false ,
		 type       = aircraft,
		 speedKmH   = 40,
		 passengers = 200,
		 infectedpassengers = 0
	       },
  {ok, PID} = vehicle:start(V),
  Long = vehicle:longitude(PID),
  vehicle:stop(PID),
  Long =:= 33.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc add passengers test
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpassenger_test() ->
  V = 
  #vehiclestate{ 
                 id         = "B50" ,
	         coordinate = #coords{ latitude = 22, longitude = 33},
	         location   = [], 
	         paused     = false ,
		 type       = aircraft,
		 speedKmH   = 40,
		 passengers = 200,
		 activity   = halted,
		 infectedpassengers = 0
	       },
  {ok, PID} = vehicle:start(V),
  {ok}      = vehicle:add_npcs(PID, 100, 10),
  {300,10}  = vehicle:npcs(PID),
  vehicle:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test if the vehicle does travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help_test() ->
  vehicle:help().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test if the vehicle does travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

travelvehicle_test() ->
  V = 
  #vehiclestate{ 
                 id         = "B50" ,
	         coordinate = #coords{ latitude = 22, longitude = 33},
	         location   = [], 
	         paused     = false ,
		 type       = aircraft,
		 speedKmH   = 40,
		 passengers = 200,
		 activity   = halted,
		 infectedpassengers = 0
	       },
  {ok, PID} = vehicle:start(V),
  {ok} = vehicle:add_npcs(PID, 100, 10),
  {300,10} = vehicle:npcs(PID),
  vehicle:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test if the vehicle does travel
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

settarget_test() ->
  V = 
  #vehiclestate{ 
                 id         = "B50" ,
	         coordinate = #coords{ latitude = 22, longitude = 33},
	         location   = [], 
	         paused     = false ,
		 type       = aircraft,
		 speedKmH   = 40,
		 passengers = 200,
		 activity   = halted,
		 infectedpassengers = 0
	       },
  {ok, PID} = vehicle:start(V),
  {ok} = vehicle:add_npcs(PID, 100, 10),
  A = #location{ name = "bla", latitude = 33.33, longitude=44.00 },
  {ok} = vehicle:target(PID, A),
  B = vehicle:target(PID),
  ?assert( A =:= B ),
  {300,10} = vehicle:npcs(PID),
  vehicle:stop(PID).

-endif.
