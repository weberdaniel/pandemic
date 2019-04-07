%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                         %%
%% File:        pandemic_sup.erl                                           %%
%%                                                                         %%
%% Description: This is the top-level-supervisor process of the pandemic   %%
%%              application. The init function reads a XML file and then   %%
%%              creates a Variable Called SupervisorTree. This supervisor  %%
%%              Tree holds a bunch of other supervisor process, for each   %%
%%              town, each player and for all other processes that need to %%
%%              be started.                                                %%
%%                                                                         %%
%%              The Supevisor tree is then returned by the init function   %%
%%              which means that the supervisor tree will be set up by the %%
%%              Erlang environment. All sub-supervisors will be started    %%
%%              under the top-level supervisor and will create their own   %%
%%              worker processes, like towns, vehicles or players, or the  %%
%%              authentification server or the world server.               %%
%%                                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HIGH PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LOW PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(pandemic_sup).
-export([start_link/1, init/1, start_link/2]).
-behaviour(supervisor).

-include("records.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

-compile([debug_info]).

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
%% @doc This function starts the top-level-supervisor while linking it to
%%      the calling process. This means if the top-level-supervisor dies,
%%      then also the calling process will die.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(_AuthFile) when is_list(_AuthFile) ->
  supervisor:start_link(?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function does the same as the above, but it initializes the
%%      whole game not from scatch, but from a file. In this file all 
%%      information on towns, player etc. will be provided.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(_Filename,_AuthFile) when is_list(_Filename), is_list(_AuthFile) ->
  supervisor:start_link(?MODULE, [_Filename,_AuthFile]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Initialize top-level-supervisor process by parsing a file that contains
%%      all towns, players etc. An appropriate supervisor tree will be built
%%      that contains all sub-level-supervisor processes. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([_Filename,_AuthFile]) when is_list(_Filename), is_list(_AuthFile) ->

  MaxRestart = 5,
  MaxTime    = 60,

  % parse a File that contains the whole config of the application in form
  % of an XML document. The XML document contains the configuratoin of each
  % town process and each player process
  
  XML = xml_parser:parse_config_file(_Filename),

  % now parse the <town> entries of the XML File and map them to
  % Town States
  
  XmlTownElements   = xml_parser:xml_town_elements_from_parsed_config_file(XML),
  XmlPlayerElements = xml_parser:xml_player_elements_from_parsed_config_file(XML),
  TownStates = lists:map( fun(X) -> 
                            xml_parser:create_town_state_from_xml_town_element(X)
                           end,
                           XmlTownElements), 

  % now parse the <player> entries of the XML File and map them to
  % Player States

  PlayerStates = lists:map( fun(X) -> 
                            xml_parser:create_player_state_from_xml_player_element(X)
                           end,
                           XmlPlayerElements), 

  % 
  % build the supervision tree, dependent on the TownStates and PlayerStates read
  % from the XML file.
  % 

  SupervisorTree = 
  
  %
  % lists:map will call the anonymous function fun(X) for all items of
  % the TownStates and PlayerStates list. Dependend on whether the
  % item is a townstate or playerstate record, the name of the process
  % is dynamically created by the name of the player or by the of the
  % town. (e.g. sup_town_munich, sup_town_nuremberg). The town record X
  % or the player record X is then handed over to the initialization 
  % function of the server, to create a town/player with the corresponding
  % properties.
  %
  
  lists:map( 
     fun 
      (X) when is_record(X, townstate) ->
       { list_to_atom(
          lists:flatten(
            io_lib:format( "sup_town_~s",
                           [X#townstate.name]
                         )
                       )
                     ),
         {town_sup, start_link, [X]},
         permanent,
         5000,
         worker,
         [town]
       };
      (X) when is_record(X, playerstate) ->
       {
         list_to_atom( 
          lists:flatten( 
           io_lib:format( "sup_player_~s", [X#playerstate.name] )
          )
         ),
         {player_sup, start_link, [X]},
         permanent,
         5000,
         worker,
         [town]
       }
     end,
       lists:flatten([TownStates,PlayerStates])
     ),


  % note it is important to start auth server and the world
  % server before the other processes are started. otherwise it
  % is not possible to authentificate or to register at the world
  % map. Not being able to register to the world map will lead to
  % a crash of a town or player process.
  
  SVTree = lists:append(
     [

      { auth,
        {sup_auth, start_link, [_AuthFile]},
        permanent,
        5000,
        worker,
        [auth]
      },

     { world,
       {sup_world, start_link, []},
       permanent,
       5000,
       worker,
       [world]
     }

   ], SupervisorTree),

  %
  % return the supervision tree.
  % 
  
  {ok, {{one_for_one, MaxRestart, MaxTime},
      SVTree } };
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% @doc This function does a default initialization of the game in case there
%      has no config file been provided. It uses two default towns, but
%      currently no auth or world server will be started.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
  MaxRestart = 5,
  MaxTime    = 60,

  StateMunich = 
  #townstate{ name            = munich,
              coordinate      = #coords
              { 
                latitude  = 48.144,
                longitude = 11.558
              },
              population      = 1300000,
              birthrate       = 20,
              infectionrate   = 5,
              lethality       = 30,
              travelrate      = 400,
              connections     = [],
              airport         = open,
              roads           = open,
              players         = []
            },

    StateNuremberg = 
    #townstate{ name          = nuremberg,
                coordinate    = #coords
                { 
                  latitude  = 49.461,
                  longitude = 11.062
                },
                population    = 1300000,
                connections   = [],
                birthrate     = 20,
                infectionrate = 5,
                lethality     = 30,
                travelrate    = 400,
                airport       = open,
                roads         = open,
                players       = []
              },

  {ok, {{one_for_one, MaxRestart, MaxTime},
        [
          {munich_sup,
            {town_sup, start_link, [StateMunich]},
            permanent,
            5000,
            worker,
            [town]
          },

          {nuremberg_sup,
            {town_sup, start_link, [StateNuremberg]},
            permanent,
            5000,
            worker,
            [town]
          }
        ]
       }
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  TESTS    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

start_test() ->
  {ok, PID} = pandemic_sup:start_link("../config/test_game_config.xml",
                                      "../auth_testfile3"),
  unlink(PID),
  exit(PID,shutdown),
  Ref = monitor(process, PID),
  receive
      {'DOWN', Ref, process, PID, _Reason} ->
          ok
  after 1000 ->
          error(exit_timeout)
  end.

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
