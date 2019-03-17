%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%% Copyright 2018 by Daniel Weber -- Full rights are reserved to the author %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% length of day in hours
-define( LENGTH_OF_DAY_HOUR, 12 ).
% length of hour in seconds 
-define( LENGTH_OF_HOUR_SEC , 1).
% this is the scaling factor that defines how much faster the game runs. if you
% map one game sec to 2 real seconds, the game becomes slower.
-define( LENGTH_OF_SEC_SEC,  1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STATE RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(townstate, 
	            { 
		     % string that describes the name of the town
	             name             :: string(), 
		     % must be a coordinate record.
                     coordinate ,
		     % a town like munich
                     population       :: integer(), 
		     % birthrate
                     birthrate        :: float(), 
		     % number of infected persons
                     infectedpopulation :: integer(), 
		     %connections to other towns
                     connections      :: list(), 
		     % one infected person infects n person per day
                     infectionrate    :: float(), 
		     % population that dies each day among infected
                     lethality        :: float(), 
		     % population that travels to other citys 
                     travelrate       :: number(), 
                     % airport open | closed
                     airport          :: atom(), 
		     % roads   open | closed
                     roads            :: atom(), 
		     % harbor  open | closed
                     harbor           :: atom(), 
		     % list of playerids that are present
                     players          :: list(), 
		     % a list of players that have already healed this day.
		     % only one heal is allowed per day.
                     healsofplayers   :: list(), 
		     % the process can be paused to save the game
                     paused           :: atom(),          
		     % the time at which the current day started
                     start_of_day_sec :: number()
                   }
       ).

%% currently unused, planned for future use
-record(countrystate, { 
	                name        , %name of the country
                        borders     , %borders can be closed or open
                        paused        % true or false
                      }).

-record(worldstate, { 
                      map,                %ets for the map
                      length_of_day_sec,  %at each end of day, a signal is
                                          %raised and forwarded to each town
                                          %to update the state
                      paused              %true or false
                    }).

-record(authserverstate, {records,
                          paused }).   % can be paused, either true or false}).

-record(playerstate, { %player nickname
	               name   :: atom(),       
                       %coordinate
                       coordinate ,   
                       %if the player has joined a specific location like a city
                       location :: atom(),      
		       %current activity
                       activity :: atom(),      
                       %paused: true or false
                       paused :: atom(),        
		       %character: character type 
                       character :: atom(),     
                       %level: character level 
		       level :: integer(),         
                       %vehicle 
		       vehicle
       }).

-record(vehiclestate, { id :: string(), %
                        %coordinate record is stored to store coordinate
                        coordinate,    
			%if the vehicle has joined a location
                        location,      
			% paused can be true or paused. Save only possible in pause mode.
                        paused :: atom(), 
                        %type: aircraft, car, ship
                        type :: atom(), 
                        %speed in km per Hour
                        speedKmH :: number(), 
                        %number of infected passengers on the vehicle
                        infectedpassengers :: number(), 
                        % number of passengers in total on this plane
                        passengers :: number(),    
                        % list of player characters that are on this plane
                        players :: list(),       
			% stores a path of coordinates that this vehicle is going to travel
                        path :: list(),          
			% the activity shows what vehicle is doing. either travel or halt.
                        activity :: atom(),      
			% time at which the coordinates have been measured the last time
			% since then, the vehicle has moved according to its path and speed.
                        coordinateTimeSec :: number(),
			% this shall be the coords record of the target that the 
			% vehicle will travel to
                        target      
       }).

-record(path, { 
	           origin,           % location record
		   target,           % target location
                   landmarks,        % a list of coordinates belonging to the 
                                     % path
                   medium            % can be a road | air | water/ship
	      }).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DATA RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(immigrants, { population,         % total population, including infected.
                      infectedpopulation, % number of the infected
                      connection          % connection that was used for travel
                                          % i.e. aircon, roadcon, shipcon
                    }).

-record(playerid, { name            :: string() %player nickname 
                  }).

-record(auth,     {  
	             username       :: string(), 
                     hashedpassword :: string(), 
                     admin          :: atom(),
                     loginstate     :: atom(), 
                     accesstoken    :: string(), 
                     level          :: integer(),
                     character      :: string()
                  }).

-record(coords,   {
	           latitude         :: number(), 
		   longitude        :: number()
                  }).

-record(location, {
	           name             :: string(), 
		   latitude         :: number(), 
		   longitude        :: number()  
		  }).

-record(aircon,  {from, 
		  to, 
		  distanceKm}).

-record(shipcon, {from, 
		  to, 
		  distanceKm}).

-record(roadcon, {from, 
		  to, 
		  distanceKm}).
