%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                         %%
%% File:        pandemic.erl                                               %%
%%                                                                         %%
%% Description: This is the application file. It retrieves the environment %%
%%              variable "filename". It starts the application supervisor  %%
%%              either with the argument "filename" or without an argument %%
%%              The "filename" is then used to be a XML config file which  %%
%%              holds a detailed specification on how to set up the game   %%
%%              world. Afterwards a welcome screen is printed              %%
%%                                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HIGH PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LOW PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

-module(pandemic).
-behaviour(application).
-export([start/2, stop/1, help/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("records.hrl").

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
%% @doc This functions start the whole pandemic application. It searches
%%      for file in the Environment variable "FILENAME". This FILENAME can
%%      be defined in the pandemic.app file in the ebin/ folder of the 
%%      project. If such a FILENAME is found, it will read an XML file and
%%      create the setup of the applicaton accordingly (start a process for
%%      each town, each player, each vehicle).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(normal, []) ->

  %% try to get the environment variable filename. If it is present, then
  %% read the file and start all processes from the xml structure in the file.
  %% otherwise start a default startup for testing purpose.

  R = case application:get_env(authconf) of 
    {ok, Value} -> case filelib:is_file(Value) of
                     true ->  
		       case application:get_env(gameconf) of
			 {ok, Value2} -> pandemic_sup:start_link(Value,Value2);
                         false        -> pandemic_sup:start_link(Value)
                       end
                   end;
              _ -> pandemic_sup:start_link()
    end,

    io:format("~n"),
    io:format("~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("         Welcome to pandemic - a game surviving             ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("      All shell commands must be finished with a dot '.'    ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("                   Basic Commands                           ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format(" registered():  See all registered processes                ~n"),
    io:format(" q():  Quit                                                 ~n"),
    io:format(" whereis(auth|munich|world|daniel): Get PID of process      ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("                   Help commands                            ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format(" town:help()  :  See all town interface functions           ~n"),
    io:format(" world:help() :  See all world interface functions          ~n"),
    io:format(" player:help():  See all player interface functions         ~n"),
    io:format(" auth:help()  :  See the auth server interface functions    ~n"),
    io:format(" auth:help()  :  See the auth server interface functions    ~n"),
    io:format(" xxx:help(command)  :  See detailed help on command        ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("                   First Steps                              ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("                                                            ~n"),
    io:format(" login and get your Key. Note Daniel is an Admin:          ~n"),
    io:format(" > {ok, Key} = auth:login(auth, \"Daniel\",\"blabla\").     ~n"),
    io:format("                                                            ~n"),
    io:format(" use your key to get information:                           ~n"),
    io:format(" > town:state(munich, Key).                                 ~n"),
    io:format("                                                            ~n"),
    io:format(" get the longitude of the process \"munich\":               ~n"),
    io:format(" > town:longitude(munich).                                  ~n"),
    io:format("                                                            ~n"),
    io:format(" the first argument is always the process to talk to:       ~n"),
    io:format(" > town:latitutde(munich).                                  ~n"),
    io:format("                                                            ~n"),
    io:format(" see population decreasing, by repeating the command:       ~n"),
    io:format(" > town:population(munich).                                 ~n"),
    io:format("                                                            ~n"),
    io:format(" see number of infected increasing by repeating:            ~n"),
    io:format(" > town:infected(munich).                                   ~n"),
    io:format("                                                            ~n"),
    io:format(" let player process \"daniel\" join the process/town \"munich\":~n"),
    io:format(" > player:join(daniel, munich, town, Key).                  ~n"),
    io:format("                                                            ~n"),
    io:format(" heal some people while in the town but only once per day:  ~n"),
    io:format(" > player:heal(daniel, Key).                                ~n"),
    io:format("                                                            ~n"),
    io:format(" try it again, only one heal per day is allowed:            ~n"),
    io:format(" > player:heal(daniel, Key).                                ~n"),
    io:format("                                                            ~n"),
    io:format(" check decrease of infected in the town, due to your heal:  ~n"),
    io:format(" > town:infected(munich).                                   ~n"),
    io:format("                                                            ~n"),
    io:format(" let player/process \"daniel\" leave town/process \"munich\":~n"),
    io:format(" > player:leave(daniel, munich, town, Key).                 ~n"),
    io:format("                                                            ~n"),
    io:format(" see a list of all processes on the world map:              ~n"),
    io:format(" > world:all(world).                                        ~n"),
    io:format("                                                            ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("                   Admin Steps                              ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("                                                            ~n"),
    io:format(" use your Admin key to pause all processes in the world:    ~n"),
    io:format("                                                            ~n"),
    io:format(" > world:pause(world,K).                                    ~n"),
    io:format("                                                            ~n"),
    io:format(" check if the town processes  and the player processes      ~n"),
    io:format(" are really paused:                                         ~n"),
    io:format("                                                            ~n"),
    io:format(" > player:paused(daniel,K).                                 ~n"),
    io:format(" > town:paused(erlangen,K).                                 ~n"),
    io:format(" > town:paused(munich,K).                                   ~n"),
    io:format("                                                            ~n"),
    io:format(" if all processes are paused, then create a XML savegame    ~n"),
    io:format(" from all of the processes by calling town:save() and       ~n"),
    io:format(" player:save() on each single process. For ease of use      ~n"),
    io:format(" you must not do it manually, you can use:                  ~n"),
    io:format("                                                            ~n"),
    io:format(" > world:save(world,K).                                     ~n"),
    io:format("                                                            ~n"),
    io:format(" write the received message to a file (To be done)          ~n"),
    io:format("                                                            ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("                   Quit                                     ~n"),
    io:format("------------------------------------------------------------~n"),
    io:format("                                                            ~n"),
    io:format(" quit the game:                                             ~n"),
    io:format(" > q().                                                     ~n"),
    io:format("~n"),
    io:format(" ~n"),

  R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function stops the whole pandemic application and exists with ok.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop(_State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This is a help function.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help() ->
  io:format("start/stop ~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       Unit Tests         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

%%UNIT TESTS are currently not implemented
application_test() ->
  ok.

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
