%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                         %%
%% File:        xml_parser.erl                                             %%
%%                                                                         %%
%% Description: This process shall only parse an xml file to create a      %%
%%              config of all processes that exist. Also it can be used    %%
%%              to save the state of a process in an xml structure         %%
%%                                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HIGH PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: implement 
%% TODO: implement xml_build_player
%% TODO: !! correct xml_build_town_conections !!
%% TODO: complete implementation of build_xml_town_savegame
%% TODO: implement unit tests for build_xml_town_savegame via creating xml
%%       parsing it and comparing it to the original data

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LOW PRIORITY TASKS %%&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: Void

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Module Definition        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(xml_parser).
-export([parse_config_file/1, xml_town_elements_from_parsed_config_file/1, 
         create_town_state_from_xml_town_element/1, build_xml_town_savegame/1,
         xml_player_elements_from_parsed_config_file/1,
         create_player_state_from_xml_player_element/1,
         build_xml_player_savegame/1 ]).

-include("records.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-ifdef(TEST).

-define(TESTXML,
	 {xmlElement,pandemic,pandemic,[],{xmlNamespace,[],[]},[],1,[],[{xmlText,[{pandemic,1}],1,[],[10,10],text},{xmlElement,world,world,[],{xmlNamespace,[],[]},[{pandemic,1}],2,[],[{xmlText,[{world,2},{pandemic,1}],1,[],[10,32,32],text},{xmlElement,length_of_day_sec,length_of_day_sec,[],{xmlNamespace,[],[]},[{world,2},{pandemic,1}],2,[],[{xmlText,[{length_of_day_sec,2},{world,2},{pandemic,1}],1,[],[32,50,48,48,32],text}],[],[99,111,110,102,105,103],undeclared},{xmlText,[{world,2},{pandemic,1}],3,[],[10],text}],[],[99,111,110,102,105,103],undeclared},{xmlText,[{pandemic,1}],3,[],[10,10],text},{xmlElement,towns,towns,[],{xmlNamespace,[],[]},[{pandemic,1}],4,[],[{xmlText,[{towns,4},{pandemic,1}],1,[],[10,10],text},{xmlElement,town,town,[],{xmlNamespace,[],[]},[{towns,4},{pandemic,1}],2,[],[{xmlText,[{town,2},{towns,4},{pandemic,1}],1,[],[10,32,32],text},{xmlElement,name,name,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],2,[],[{xmlText,[{name,2},{town,2},{towns,4},{pandemic,1}],1,[],[101,114,108,97,110,103,101,110],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],3,[],[10,32,32],text},{xmlElement,coordinate,coordinate,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],4,[],[{xmlText,[{coordinate,4},{town,2},{towns,4},{pandemic,1}],1,[],[32,10,32,32,32,32],text},{xmlElement,latitude,latitude,[],{xmlNamespace,[],[]},[{coordinate,4},{town,2},{towns,4},{pandemic,1}],2,[],[{xmlText,[{latitude,2},{coordinate,4},{town,2},{towns,4},{pandemic,1}],1,[],[32,49,48,46,49,50,52,32],text}],[],undefined,undeclared},{xmlText,[{coordinate,4},{town,2},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32],text},{xmlElement,longitude,longitude,[],{xmlNamespace,[],[]},[{coordinate,4},{town,2},{towns,4},{pandemic,1}],4,[],[{xmlText,[{longitude,4},{coordinate,4},{town,2},{towns,4},{pandemic,1}],1,[],[32,53,46,49,50,51,32],text}],[],undefined,undeclared},{xmlText,[{coordinate,4},{town,2},{towns,4},{pandemic,1}],5,[],[10,32,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],5,[],[10,32,32],text},{xmlElement,population,population,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],6,[],[{xmlText,[{population,6},{town,2},{towns,4},{pandemic,1}],1,[],[32,51,48,48,48,48,48,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],7,[],[10,32,32],text},{xmlElement,connections,connections,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],8,[],[{xmlText,[{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[10,32,32,32,32],text},{xmlElement,aircon,aircon,[],{xmlNamespace,[],[]},[{connections,8},{town,2},{towns,4},{pandemic,1}],2,[],[{xmlText,[{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,10,32,32,32,32,32,32],text},{xmlElement,from,from,[],{xmlNamespace,[],[]},[{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],2,[],[{xmlText,[{from,2},{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,101,114,108,97,110,103,101,110,32],text}],[],undefined,undeclared},{xmlText,[{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32,32,32],text},{xmlElement,to,to,[],{xmlNamespace,[],[]},[{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],4,[],[{xmlText,[{to,4},{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,109,117,110,105,99,104,32],text}],[],undefined,undeclared},{xmlText,[{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],5,[],[10,32,32,32,32,32,32],text},{xmlElement,distanceKm,distanceKm,[],{xmlNamespace,[],[]},[{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],6,[],[{xmlText,[{distanceKm,6},{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,49,48,48,46,48,32],text}],[],undefined,undeclared},{xmlText,[{aircon,2},{connections,8},{town,2},{towns,4},{pandemic,1}],7,[],[10,32,32,32,32],text}],[],undefined,undeclared},{xmlText,[{connections,8},{town,2},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32],text},{xmlElement,roadcon,roadcon,[],{xmlNamespace,[],[]},[{connections,8},{town,2},{towns,4},{pandemic,1}],4,[],[{xmlText,[{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,10,32,32,32,32,32,32],text},{xmlElement,from,from,[],{xmlNamespace,[],[]},[{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],2,[],[{xmlText,[{from,2},{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,101,114,108,97,110,103,101,110,32],text}],[],undefined,undeclared},{xmlText,[{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32,32,32],text},{xmlElement,to,to,[],{xmlNamespace,[],[]},[{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],4,[],[{xmlText,[{to,4},{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,109,117,110,105,99,104,32],text}],[],undefined,undeclared},{xmlText,[{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],5,[],[10,32,32,32,32,32,32],text},{xmlElement,distanceKm,distanceKm,[],{xmlNamespace,[],[]},[{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],6,[],[{xmlText,[{distanceKm,6},{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,49,48,48,46,48,32],text}],[],undefined,undeclared},{xmlText,[{roadcon,4},{connections,8},{town,2},{towns,4},{pandemic,1}],7,[],[10,32,32,32,32],text}],[],undefined,undeclared},{xmlText,[{connections,8},{town,2},{towns,4},{pandemic,1}],5,[],[10,32,32,32,32],text},{xmlElement,shipcon,shipcon,[],{xmlNamespace,[],[]},[{connections,8},{town,2},{towns,4},{pandemic,1}],6,[],[{xmlText,[{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,10,32,32,32,32,32,32],text},{xmlElement,from,from,[],{xmlNamespace,[],[]},[{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],2,[],[{xmlText,[{from,2},{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,101,114,108,97,110,103,101,110,32],text}],[],undefined,undeclared},{xmlText,[{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32,32,32],text},{xmlElement,to,to,[],{xmlNamespace,[],[]},[{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],4,[],[{xmlText,[{to,4},{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,109,117,110,105,99,104,32],text}],[],undefined,undeclared},{xmlText,[{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],5,[],[10,32,32,32,32,32,32],text},{xmlElement,distanceKm,distanceKm,[],{xmlNamespace,[],[]},[{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],6,[],[{xmlText,[{distanceKm,6},{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],1,[],[32,49,48,48,46,48],text}],[],undefined,undeclared},{xmlText,[{shipcon,6},{connections,8},{town,2},{towns,4},{pandemic,1}],7,[],[10,32,32,32,32],text}],[],undefined,undeclared},{xmlText,[{connections,8},{town,2},{towns,4},{pandemic,1}],7,[],[10,32,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],9,[],[10,32,32],text},{xmlElement,birthrate,birthrate,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],10,[],[{xmlText,[{birthrate,10},{town,2},{towns,4},{pandemic,1}],1,[],[32,48,46,49,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],11,[],[10,32,32],text},{xmlElement,infectionrate,infectionrate,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],12,[],[{xmlText,[{infectionrate,12},{town,2},{towns,4},{pandemic,1}],1,[],[32,49,50,46,48,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],13,[],[10,32,32],text},{xmlElement,infectedpopulation,infectedpopulation,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],14,[],[{xmlText,[{infectedpopulation,14},{town,2},{towns,4},{pandemic,1}],1,[],[32,49,48,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],15,[],[10,32,32],text},{xmlElement,lethality,lethality,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],16,[],[{xmlText,[{lethality,16},{town,2},{towns,4},{pandemic,1}],1,[],[32,48,46,49,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],17,[],[10,32,32],text},{xmlElement,travelrate,travelrate,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],18,[],[{xmlText,[{travelrate,18},{town,2},{towns,4},{pandemic,1}],1,[],[32,51,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],19,[],[10,32,32],text},{xmlElement,airport,airport,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],20,[],[{xmlText,[{airport,20},{town,2},{towns,4},{pandemic,1}],1,[],[32,111,112,101,110,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],21,[],[10,32,32],text},{xmlElement,roads,roads,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],22,[],[{xmlText,[{roads,22},{town,2},{towns,4},{pandemic,1}],1,[],[32,111,112,101,110,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],23,[],[10,32,32],text},{xmlElement,harbor,harbor,[],{xmlNamespace,[],[]},[{town,2},{towns,4},{pandemic,1}],24,[],[{xmlText,[{harbor,24},{town,2},{towns,4},{pandemic,1}],1,[],[32,111,112,101,110,32],text}],[],undefined,undeclared},{xmlText,[{town,2},{towns,4},{pandemic,1}],25,[],[10],text}],[],undefined,undeclared},{xmlText,[{towns,4},{pandemic,1}],3,[],[10,10],text},{xmlElement,town,town,[],{xmlNamespace,[],[]},[{towns,4},{pandemic,1}],4,[],[{xmlText,[{town,4},{towns,4},{pandemic,1}],1,[],[10,32,32],text},{xmlElement,name,name,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],2,[],[{xmlText,[{name,2},{town,4},{towns,4},{pandemic,1}],1,[],[32,109,117,110,105,99,104,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],3,[],[10,32,32],text},{xmlElement,coordinate,coordinate,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],4,[],[{xmlText,[{coordinate,4},{town,4},{towns,4},{pandemic,1}],1,[],[32,10,32,32,32,32],text},{xmlElement,latitude,latitude,[],{xmlNamespace,[],[]},[{coordinate,4},{town,4},{towns,4},{pandemic,1}],2,[],[{xmlText,[{latitude,2},{coordinate,4},{town,4},{towns,4},{pandemic,1}],1,[],[32,51,51,46,49,32],text}],[],undefined,undeclared},{xmlText,[{coordinate,4},{town,4},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32],text},{xmlElement,longitude,longitude,[],{xmlNamespace,[],[]},[{coordinate,4},{town,4},{towns,4},{pandemic,1}],4,[],[{xmlText,[{longitude,4},{coordinate,4},{town,4},{towns,4},{pandemic,1}],1,[],[32,57,57,46,51,51,32],text}],[],undefined,undeclared},{xmlText,[{coordinate,4},{town,4},{towns,4},{pandemic,1}],5,[],[10,32,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],5,[],[10,32,32],text},{xmlElement,population,population,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],6,[],[{xmlText,[{population,6},{town,4},{towns,4},{pandemic,1}],1,[],[32,49,48,48,48,48,48,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],7,[],[10,32,32],text},{xmlElement,connections,connections,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],8,[],[{xmlText,[{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,10,32,32,32,32],text},{xmlElement,aircon,aircon,[],{xmlNamespace,[],[]},[{connections,8},{town,4},{towns,4},{pandemic,1}],2,[],[{xmlText,[{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,10,32,32,32,32,32,32],text},{xmlElement,from,from,[],{xmlNamespace,[],[]},[{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],2,[],[{xmlText,[{from,2},{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,109,117,110,105,99,104,32],text}],[],undefined,undeclared},{xmlText,[{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32,32,32],text},{xmlElement,to,to,[],{xmlNamespace,[],[]},[{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],4,[],[{xmlText,[{to,4},{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,101,114,108,97,110,103,101,110,32],text}],[],undefined,undeclared},{xmlText,[{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],5,[],[10,32,32,32,32,32,32],text},{xmlElement,distanceKm,distanceKm,[],{xmlNamespace,[],[]},[{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],6,[],[{xmlText,[{distanceKm,6},{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,49,48,48,48,46,48,32],text}],[],undefined,undeclared},{xmlText,[{aircon,2},{connections,8},{town,4},{towns,4},{pandemic,1}],7,[],[10,32,32,32,32],text}],[],undefined,undeclared},{xmlText,[{connections,8},{town,4},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32],text},{xmlElement,roadcon,roadcon,[],{xmlNamespace,[],[]},[{connections,8},{town,4},{towns,4},{pandemic,1}],4,[],[{xmlText,[{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,10,32,32,32,32,32,32],text},{xmlElement,from,from,[],{xmlNamespace,[],[]},[{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],2,[],[{xmlText,[{from,2},{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,109,117,110,105,99,104],text}],[],undefined,undeclared},{xmlText,[{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32,32,32],text},{xmlElement,to,to,[],{xmlNamespace,[],[]},[{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],4,[],[{xmlText,[{to,4},{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,101,114,108,97,110,103,101,110,32],text}],[],undefined,undeclared},{xmlText,[{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],5,[],[10,32,32,32,32,32,32],text},{xmlElement,distanceKm,distanceKm,[],{xmlNamespace,[],[]},[{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],6,[],[{xmlText,[{distanceKm,6},{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,50,48,48,48,46,48,32],text}],[],undefined,undeclared},{xmlText,[{roadcon,4},{connections,8},{town,4},{towns,4},{pandemic,1}],7,[],[10,32,32,32,32],text}],[],undefined,undeclared},{xmlText,[{connections,8},{town,4},{towns,4},{pandemic,1}],5,[],[10,32,32,32,32],text},{xmlElement,shipcon,shipcon,[],{xmlNamespace,[],[]},[{connections,8},{town,4},{towns,4},{pandemic,1}],6,[],[{xmlText,[{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,10,32,32,32,32,32,32],text},{xmlElement,from,from,[],{xmlNamespace,[],[]},[{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],2,[],[{xmlText,[{from,2},{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,109,117,110,105,99,104],text}],[],undefined,undeclared},{xmlText,[{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],3,[],[10,32,32,32,32,32,32],text},{xmlElement,to,to,[],{xmlNamespace,[],[]},[{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],4,[],[{xmlText,[{to,4},{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,101,114,108,97,110,103,101,110],text}],[],undefined,undeclared},{xmlText,[{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],5,[],[10,32,32,32,32,32,32],text},{xmlElement,distanceKm,distanceKm,[],{xmlNamespace,[],[]},[{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],6,[],[{xmlText,[{distanceKm,6},{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],1,[],[32,51,48,48,48,46,48,32],text}],[],undefined,undeclared},{xmlText,[{shipcon,6},{connections,8},{town,4},{towns,4},{pandemic,1}],7,[],[10,32,32,32,32],text}],[],undefined,undeclared},{xmlText,[{connections,8},{town,4},{towns,4},{pandemic,1}],7,[],[10,32,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],9,[],[10,32,32],text},{xmlElement,birthrate,birthrate,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],10,[],[{xmlText,[{birthrate,10},{town,4},{towns,4},{pandemic,1}],1,[],[32,48,46,48,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],11,[],[10,32,32],text},{xmlElement,infectionrate,infectionrate,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],12,[],[{xmlText,[{infectionrate,12},{town,4},{towns,4},{pandemic,1}],1,[],[32,49,46,48,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],13,[],[10,32,32],text},{xmlElement,infectedpopulation,infectedpopulation,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],14,[],[{xmlText,[{infectedpopulation,14},{town,4},{towns,4},{pandemic,1}],1,[],[32,49,48,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],15,[],[10,32,32],text},{xmlElement,lethality,lethality,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],16,[],[{xmlText,[{lethality,16},{town,4},{towns,4},{pandemic,1}],1,[],[32,48,46,53,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],17,[],[10,32,32],text},{xmlElement,travelrate,travelrate,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],18,[],[{xmlText,[{travelrate,18},{town,4},{towns,4},{pandemic,1}],1,[],[32,49,49,51,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],19,[],[10,32,32],text},{xmlElement,airport,airport,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],20,[],[{xmlText,[{airport,20},{town,4},{towns,4},{pandemic,1}],1,[],[32,111,112,101,110,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],21,[],[10,32,32],text},{xmlElement,roads,roads,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],22,[],[{xmlText,[{roads,22},{town,4},{towns,4},{pandemic,1}],1,[],[32,99,108,111,115,101,100,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],23,[],[10,32,32],text},{xmlElement,harbor,harbor,[],{xmlNamespace,[],[]},[{town,4},{towns,4},{pandemic,1}],24,[],[{xmlText,[{harbor,24},{town,4},{towns,4},{pandemic,1}],1,[],[32,111,112,101,110,32,32],text}],[],undefined,undeclared},{xmlText,[{town,4},{towns,4},{pandemic,1}],25,[],[10],text}],[],undefined,undeclared},{xmlText,[{towns,4},{pandemic,1}],5,[],[10],text}],[],undefined,undeclared},{xmlText,[{pandemic,1}],5,[],[10,10,10],text},{xmlElement,players,players,[],{xmlNamespace,[],[]},[{pandemic,1}],6,[],[{xmlText,[{players,6},{pandemic,1}],1,[],[10,32,32],text},{xmlElement,player,player,[],{xmlNamespace,[],[]},[{players,6},{pandemic,1}],2,[],[{xmlText,[{player,2},{players,6},{pandemic,1}],1,[],[32,10,32,32,32,32],text},{xmlElement,name,name,[],{xmlNamespace,[],[]},[{player,2},{players,6},{pandemic,1}],2,[],[{xmlText,[{name,2},{player,2},{players,6},{pandemic,1}],1,[],[32,100,97,110,105,101,108,32],text}],[],undefined,undeclared},{xmlText,[{player,2},{players,6},{pandemic,1}],3,[],[10,10,32,32,32,32],text},{xmlElement,level,level,[],{xmlNamespace,[],[]},[{player,2},{players,6},{pandemic,1}],4,[],[{xmlText,[{level,4},{player,2},{players,6},{pandemic,1}],1,[],[32,51,32],text}],[],undefined,undeclared},{xmlText,[{player,2},{players,6},{pandemic,1}],5,[],[10,32,32,32,32],text},{xmlElement,character,character,[],{xmlNamespace,[],[]},[{player,2},{players,6},{pandemic,1}],6,[],[{xmlText,[{character,6},{player,2},{players,6},{pandemic,1}],1,[],[32,109,101,100,105,99,32],text}],[],undefined,undeclared},{xmlText,[{player,2},{players,6},{pandemic,1}],7,[],[10,10,32,32,32,32],text},{xmlElement,coordinate,coordinate,[],{xmlNamespace,[],[]},[{player,2},{players,6},{pandemic,1}],8,[],[{xmlText,[{coordinate,8},{player,2},{players,6},{pandemic,1}],1,[],[32,10,32,32,32,32,32,32],text},{xmlElement,latitude,latitude,[],{xmlNamespace,[],[]},[{coordinate,8},{player,2},{players,6},{pandemic,1}],2,[],[{xmlText,[{latitude,2},{coordinate,8},{player,2},{players,6},{pandemic,1}],1,[],[32,51,51,46,49,32],text}],[],undefined,undeclared},{xmlText,[{coordinate,8},{player,2},{players,6},{pandemic,1}],3,[],[10,32,32,32,32,32,32],text},{xmlElement,longitude,longitude,[],{xmlNamespace,[],[]},[{coordinate,8},{player,2},{players,6},{pandemic,1}],4,[],[{xmlText,[{longitude,4},{coordinate,8},{player,2},{players,6},{pandemic,1}],1,[],[32,57,57,46,51,51,32],text}],[],undefined,undeclared},{xmlText,[{coordinate,8},{player,2},{players,6},{pandemic,1}],5,[],[10,32,32,32,32],text}],[],undefined,undeclared},{xmlText,[{player,2},{players,6},{pandemic,1}],9,[],[10,10,32,32,32,32],text},{xmlElement,location,location,[],{xmlNamespace,[],[]},[{player,2},{players,6},{pandemic,1}],10,[],[{xmlText,[{location,10},{player,2},{players,6},{pandemic,1}],1,[],[32,10,32,32,32,32,32,32],text},{xmlElement,name,name,[],{xmlNamespace,[],[]},[{location,10},{player,2},{players,6},{pandemic,1}],2,[],[{xmlText,[{name,2},{location,10},{player,2},{players,6},{pandemic,1}],1,[],[32,109,117,110,105,99,104,32],text}],[],undefined,undeclared},{xmlText,[{location,10},{player,2},{players,6},{pandemic,1}],3,[],[10,32,32,32,32,32,32],text},{xmlElement,latitude,latitude,[],{xmlNamespace,[],[]},[{location,10},{player,2},{players,6},{pandemic,1}],4,[],[{xmlText,[{latitude,4},{location,10},{player,2},{players,6},{pandemic,1}],1,[],[32,51,51,46,49,32],text}],[],undefined,undeclared},{xmlText,[{location,10},{player,2},{players,6},{pandemic,1}],5,[],[10,32,32,32,32,32,32],text},{xmlElement,longitude,longitude,[],{xmlNamespace,[],[]},[{location,10},{player,2},{players,6},{pandemic,1}],6,[],[{xmlText,[{longitude,6},{location,10},{player,2},{players,6},{pandemic,1}],1,[],[32,57,57,46,51,51,32],text}],[],undefined,undeclared},{xmlText,[{location,10},{player,2},{players,6},{pandemic,1}],7,[],[10,32,32,32,32],text}],[],undefined,undeclared},{xmlText,[{player,2},{players,6},{pandemic,1}],11,[],[10,32,32],text}],[],undefined,undeclared},{xmlText,[{players,6},{pandemic,1}],3,[],[10],text}],[],undefined,undeclared},{xmlText,[{pandemic,1}],7,[],[10,10,10],text}],[],[99,111,110,102,105,103],undeclared}).
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile([debug_info]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Helper Functions    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_config_file(_Filename) when is_list(_Filename) ->
  {ParserResult, _} = xmerl_scan:file(_Filename),
  ParserResult.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% filter all xml town elements from the xml parser result
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_town_elements_from_parsed_config_file(_ParserResult) 
when is_record(_ParserResult, xmlElement), 
     _ParserResult#xmlElement.name =:= pandemic ->
  Content = _ParserResult#xmlElement.content,
  TownsContent = xml_get_towns( Content, [] ),
  xml_get_town_elements(TownsContent, []);
xml_town_elements_from_parsed_config_file(_) ->
  no_towns_in_config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% filter all xml player elements from the xml parser result
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_player_elements_from_parsed_config_file(_ParserResult) 
when is_record(_ParserResult, xmlElement), 
     _ParserResult#xmlElement.name =:= pandemic ->
  Content = _ParserResult#xmlElement.content,
  PlayersContent = xml_get_players( Content, [] ),
  R = xml_get_player_elements(PlayersContent, []),
  R;
xml_player_elements_from_parsed_config_file(_) ->
  no_players_in_config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% get the content of all top-level xml tags <towns>, which contains 
%% several <town> elements.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_get_towns([],[]) ->
  [];
xml_get_towns([],_Acc) ->
  _Acc;
xml_get_towns(_List, _Acc) ->
  Head = hd(_List),
  case is_record(Head, xmlElement) of 
    true -> case Head#xmlElement.name =:= towns of
               true  -> NewAcc = lists:append(_Acc, Head#xmlElement.content),
                        xml_get_towns(tl(_List), NewAcc );
               false -> xml_get_towns(tl(_List), _Acc) 
            end;
    false -> xml_get_towns(tl(_List), _Acc) 
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% get the content of all top-level xml tags <players>, which contains 
%% several <player> elements.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_get_players([],[]) ->
  [];
xml_get_players([],_Acc) ->
  _Acc;
xml_get_players(_List, _Acc) ->
  Head = hd(_List),
  case is_record(Head, xmlElement) of 
    true -> case Head#xmlElement.name =:= players of
               true  -> NewAcc = lists:append(_Acc, Head#xmlElement.content),
                        xml_get_players(tl(_List), NewAcc );
               false -> xml_get_players(tl(_List), _Acc) 
            end;
    false -> xml_get_players(tl(_List), _Acc) 
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get all elements named <town> in the list. Performs no recursive
%% search.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_get_town_elements([], []) ->
  [];
xml_get_town_elements([], _Acc) ->
  _Acc;
xml_get_town_elements(_XML, _Acc) when is_list(_XML) ->
  Head = hd(_XML),
  case is_record(Head, xmlElement) of
    true -> case Head#xmlElement.name =:= town of 
              true  -> NewAcc = lists:append(_Acc, [Head]),
                       xml_get_town_elements( tl(_XML), NewAcc );
              false -> NewAcc = _Acc,
                       xml_get_town_elements( tl(_XML), NewAcc )
            end;
    false -> xml_get_town_elements( tl(_XML), _Acc )
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get all elements named <player> in the list. Performs no recursive
%% search.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_get_player_elements([], []) ->
  [];
xml_get_player_elements([], _Acc) ->
  _Acc;
xml_get_player_elements(_XML, _Acc) when is_list(_XML) ->
  Head = hd(_XML),
  case is_record(Head, xmlElement) of
    true -> case Head#xmlElement.name =:= player of 
              true  -> NewAcc = lists:append(_Acc, [Head]),
                       xml_get_player_elements( tl(_XML), NewAcc );
              false -> NewAcc = _Acc,
                       xml_get_player_elements( tl(_XML), NewAcc )
            end;
    false -> xml_get_player_elements( tl(_XML), _Acc )
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% create one player state for a process from one <town> element.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%TODO: implement the location of the player
create_player_state_from_xml_player_element(_XmlElement) 
  when is_record(_XmlElement, xmlElement), 
       _XmlElement#xmlElement.name =:= player ->
  Name  = player_name(_XmlElement),
  Level = player_level(_XmlElement),
  Lat   = player_coordinate_latitude(player_coordinate(_XmlElement)),
  Long  = player_coordinate_longitude(player_coordinate(_XmlElement)),
  VLat  = player_coordinate_latitude_value(Lat),
  VLong = player_coordinate_longitude_value(Long),
  

  #playerstate{ name            = Name,
                coordinate      = #coords
              { 
                latitude  = VLat,
                longitude = VLong
              },
              location        = #location
              { 
                name      = Name,
                latitude  = VLat,
                longitude = VLong
              },
	      level = Level
            }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% create one town state for a process from one <town> element.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_town_state_from_xml_town_element(_XmlElement) 
  when is_record(_XmlElement, xmlElement), 
       _XmlElement#xmlElement.name =:= town ->
  Name = town_name(_XmlElement),
  Lat = town_coordinate_latitude(town_coordinate(_XmlElement)),
  Long = town_coordinate_longitude(town_coordinate(_XmlElement)),
  VLat = town_coordinate_latitude_value(Lat),
  VLong = town_coordinate_longitude_value(Long),
  Population = town_population(_XmlElement),
  Infectionrate = town_infectionrate(_XmlElement),
  Infectedpopulation = town_infectedpopulation(_XmlElement),
  Lethality = town_lethality(_XmlElement),
  Roads = town_roads(_XmlElement),
  Birthrate = town_birthrate(_XmlElement),
  Travelrate = town_travelrate(_XmlElement),
  Airport = town_airport(_XmlElement),
  Harbor  = town_harbor(_XmlElement),
  Connections = xml_connections_to_records(town_connections(_XmlElement)),

  #townstate{ name               = Name,
              coordinate         = #coords
              { 
                latitude  = VLat,
                longitude = VLong
              },
              population         = Population,
              infectionrate      = Infectionrate,
              birthrate          = Birthrate,
              lethality          = Lethality,
              travelrate         = Travelrate,
              airport            = Airport,
	      infectedpopulation = Infectedpopulation,
              harbor             = Harbor,
              roads              = Roads,
              connections        = Connections,
              players            = []
            }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town name string by handing over the xmlElement
%% with name town.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_name(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= name of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   string:strip(lists:flatten(R)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the player name string by handing over the xmlElement
%% with name player.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_name(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= player ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= name of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   string:strip(lists:flatten(R)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the player integer by handing over the xmlElement
%% with name level.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_level(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= player ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= level of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   list_to_integer(string:strip(lists:flatten(R))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town population as an integer by handing over the xmlElement
%% with name town.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_population(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= population of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   list_to_integer(string:strip(lists:flatten(R))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town travelrate as an integer by handing over the xmlElement
%% with name town.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_travelrate(_XmlElement) when is_record(_XmlElement, xmlElement),
                                  _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= travelrate of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   list_to_integer(string:strip(lists:flatten(R))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town infectionrate as an integer by handing over the xmlElement
%% with name town.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_infectionrate(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= infectionrate of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   list_to_float(string:strip(lists:flatten(R))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town infectedpopulation as an integer by handing over the xmlElement
%% with name town.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_infectedpopulation(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= infectedpopulation of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   list_to_integer(string:strip(lists:flatten(R))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town birthrate as an integer by handing over the xmlElement
%% with name town.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_birthrate(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= birthrate of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   list_to_float(string:strip(lists:flatten(R))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town lethality as an integer by handing over the xmlElement
%% with name town.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_lethality(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= lethality of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   list_to_float(string:strip(lists:flatten(R))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the content of the town coordinates xml element by handing 
%% over the town xmlElement
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_coordinate(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= coordinate of
                                 true -> C = X#xmlElement.content,
                                         C;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   lists:flatten(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the content of the player coordinates xml element by handing 
%% over the town xmlElement
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_coordinate(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= player ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= coordinate of
                                 true -> C = X#xmlElement.content,
                                         C;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   lists:flatten(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town coordinate latitude by handing over the content of
%% the town coordinates xml element
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_coordinate_latitude([]) ->
  [];
town_coordinate_latitude(_XmlContent) when is_list(_XmlContent) ->
  Head = hd(_XmlContent),
  case is_record(Head, xmlElement) of
    true -> case Head#xmlElement.name =:= latitude of
              true -> Head;
              false -> town_coordinate_latitude(tl(_XmlContent)) 
            end;
    false -> town_coordinate_latitude(tl(_XmlContent))
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the player coordinate latitude by handing over the content
%% of the player coordinsates xml element
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_coordinate_latitude([]) ->
  [];
player_coordinate_latitude(_XmlContent) when is_list(_XmlContent) ->
  Head = hd(_XmlContent),
  case is_record(Head, xmlElement) of
    true -> case Head#xmlElement.name =:= latitude of
              true -> Head;
              false -> player_coordinate_latitude(tl(_XmlContent)) 
            end;
    false -> player_coordinate_latitude(tl(_XmlContent))
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the player coordinate longitude by handing over the content
%% of the player coordinates xml element
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_coordinate_longitude([]) ->
  [];
player_coordinate_longitude(_XmlContent) when is_list(_XmlContent) ->
  Head = hd(_XmlContent),
  case is_record(Head, xmlElement) of
    true -> case Head#xmlElement.name =:= longitude of
              true -> Head;
              false -> player_coordinate_longitude(tl(_XmlContent)) 
            end;
    false -> player_coordinate_longitude(tl(_XmlContent))
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town coordinate longitude by handing over the content of
%% the town coordinates xml element
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_coordinate_longitude([]) ->
  [];
town_coordinate_longitude(_XmlContent) when is_list(_XmlContent) ->
  Head = hd(_XmlContent),
  case is_record(Head, xmlElement) of
    true -> case Head#xmlElement.name =:= longitude of
              true -> Head;
              false -> town_coordinate_longitude(tl(_XmlContent)) 
            end;
    false -> town_coordinate_longitude(tl(_XmlContent))
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town coordinate longitude value. As an input a xml longitude
%% element shall be inserted.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_coordinate_longitude_value(_XmlElement) when 
  is_record(_XmlElement, xmlElement),
  _XmlElement#xmlElement.name =:= longitude 
-> Content = _XmlElement#xmlElement.content,
   Text = hd(Content),
   {R, _} = string:to_float(string:strip(Text#xmlText.value)),
   R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the player coordinate longitude value. As an input a xml longitude
%% element shall be inserted.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_coordinate_longitude_value(_XmlElement) when 
  is_record(_XmlElement, xmlElement),
  _XmlElement#xmlElement.name =:= longitude 
-> Content = _XmlElement#xmlElement.content,
   Text = hd(Content),
   {R, _} = string:to_float(string:strip(Text#xmlText.value)),
   R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the town coordinate longitude value. As an input a xml latitude
%% element shall be inserted.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_coordinate_latitude_value(_XmlElement) when 
  is_record(_XmlElement, xmlElement),
  _XmlElement#xmlElement.name =:= latitude 
-> Content = _XmlElement#xmlElement.content,
   Text = hd(Content),
   {R, _} = string:to_float(string:strip(Text#xmlText.value)),
   R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get the player coordinate longitude value. As an input a xml latitude
%% element shall be inserted.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

player_coordinate_latitude_value(_XmlElement) when 
  is_record(_XmlElement, xmlElement),
  _XmlElement#xmlElement.name =:= latitude 
-> Content = _XmlElement#xmlElement.content,
   Text = hd(Content),
   {R, _} = string:to_float(string:strip(Text#xmlText.value)),
   R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Hand over a Xml Town element and get back all connections of
%% the towns noted in the xml file
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_connections(_XmlElement) when is_record(_XmlElement, xmlElement),
                                   _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true -> case X#xmlElement.name =:= connections of
                                true -> 
                                  lists:map( fun(Y) ->
                                               if Y#xmlElement.name =:= aircon;
                                                  Y#xmlElement.name =:= shipcon;
                                                  Y#xmlElement.name =:= roadcon
                                                    ->
                                                    Y;
                                                  true ->
                                                   []
                                               end
                                             end,
                                             X#xmlElement.content );
                                _ -> []
                              end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ), 
  lists:flatten(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% convert all the xml connections of the town to erlang records
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_connections_to_records(_XmlList) when is_list(_XmlList) ->
  lists:map( fun( X ) ->
               case is_record(X,xmlElement) of
                 true -> case X#xmlElement.name of
                           aircon  -> xml_aircon_to_record(X);
                           roadcon -> xml_roadcon_to_record(X);
                           shipcon -> xml_shipcon_to_record(X);
                           _ -> []
                         end;
                 false -> []
               end
             end,
             _XmlList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% convert all the xml connections of the town to erlang records
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_roadcon_to_record(_XmlElement) when 
 is_record(_XmlElement,xmlElement), _XmlElement#xmlElement.name =:= roadcon ->

  FromXmlElement = 
  hd(lists:flatten(
  lists:map( fun(X) ->
               case is_record(X,xmlElement) of
                 true -> case X#xmlElement.name =:= from of
                           true -> X;
                           _ -> []
                         end;
                 false -> []
               end
             end,
             _XmlElement#xmlElement.content
           )
          )),
   FromXmlText = hd(FromXmlElement#xmlElement.content),

   ToXmlElement = 
   hd(lists:flatten(
   lists:map( fun(X) ->
                case is_record(X,xmlElement) of
                  true -> case X#xmlElement.name =:= to of
                            true -> X;
                            _ -> []
                          end;
                  false -> []
                end
              end,
              _XmlElement#xmlElement.content
            )
          )),
   ToXmlText = hd(ToXmlElement#xmlElement.content),

   DistanceKmXmlElement = 
   hd(lists:flatten(
   lists:map( fun(X) ->
                case is_record(X,xmlElement) of
                  true -> case X#xmlElement.name =:= distanceKm of
                            true -> X;
                            _ -> []
                          end;
                  false -> []
                end
              end,
              _XmlElement#xmlElement.content
            )
          )),
   DistanceKmXmlText = hd(DistanceKmXmlElement#xmlElement.content),

   #roadcon{ from = list_to_atom(string:strip(FromXmlText#xmlText.value)),
            to   = list_to_atom(string:strip(ToXmlText#xmlText.value)),
            distanceKm = list_to_float(string:strip(DistanceKmXmlText#xmlText.value))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% convert all the xml connections of the town to erlang records
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_shipcon_to_record(_XmlElement) when 
 is_record(_XmlElement,xmlElement), _XmlElement#xmlElement.name =:= shipcon ->

  FromXmlElement = 
  hd(lists:flatten(
  lists:map( fun(X) ->
               case is_record(X,xmlElement) of
                 true -> case X#xmlElement.name =:= from of
                           true -> X;
                           _ -> []
                         end;
                 false -> []
               end
             end,
             _XmlElement#xmlElement.content
           )
          )),
   FromXmlText = hd(FromXmlElement#xmlElement.content),

   ToXmlElement = 
   hd(lists:flatten(
   lists:map( fun(X) ->
                case is_record(X,xmlElement) of
                  true -> case X#xmlElement.name =:= to of
                            true -> X;
                            _ -> []
                          end;
                  false -> []
                end
              end,
              _XmlElement#xmlElement.content
            )
          )),
   ToXmlText = hd(ToXmlElement#xmlElement.content),

   DistanceKmXmlElement = 
   hd(lists:flatten(
   lists:map( fun(X) ->
                case is_record(X,xmlElement) of
                  true -> case X#xmlElement.name =:= distanceKm of
                            true -> X;
                            _ -> []
                          end;
                  false -> []
                end
              end,
              _XmlElement#xmlElement.content
            )
          )),
   DistanceKmXmlText = hd(DistanceKmXmlElement#xmlElement.content),

   #shipcon{ from = 
     list_to_atom(string:strip(FromXmlText#xmlText.value)),
            to   = 
     list_to_atom(string:strip(ToXmlText#xmlText.value)),
            distanceKm = 
     list_to_float(string:strip(DistanceKmXmlText#xmlText.value))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% convert all the xml connections of the town to erlang records
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_aircon_to_record(_XmlElement) when 
 is_record(_XmlElement,xmlElement), _XmlElement#xmlElement.name =:= aircon ->

  FromXmlElement = 
  hd(lists:flatten(
  lists:map( fun(X) ->
               case is_record(X,xmlElement) of
                 true -> case X#xmlElement.name =:= from of
                           true -> X;
                           _ -> []
                         end;
                 false -> []
               end
             end,
             _XmlElement#xmlElement.content
           )
          )),
   FromXmlText = hd(FromXmlElement#xmlElement.content),

   ToXmlElement = 
   hd(lists:flatten(
   lists:map( fun(X) ->
                case is_record(X,xmlElement) of
                  true -> case X#xmlElement.name =:= to of
                            true -> X;
                            _ -> []
                          end;
                  false -> []
                end
              end,
              _XmlElement#xmlElement.content
            )
          )),
   ToXmlText = hd(ToXmlElement#xmlElement.content),

   DistanceKmXmlElement = 
   hd(lists:flatten(
   lists:map( fun(X) ->
                case is_record(X,xmlElement) of
                  true -> case X#xmlElement.name =:= distanceKm of
                            true -> X;
                            _ -> []
                          end;
                  false -> []
                end
              end,
              _XmlElement#xmlElement.content
            )
          )),
   DistanceKmXmlText = hd(DistanceKmXmlElement#xmlElement.content),

   #aircon{ from = 
     list_to_atom(string:strip(FromXmlText#xmlText.value)),
            to   = 
     list_to_atom(string:strip(ToXmlText#xmlText.value)),
            distanceKm = 
     list_to_float((string:strip((DistanceKmXmlText#xmlText.value))))
   }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% convert all the xml connections of the town to erlang records
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_roads(_XmlElement) when is_record(_XmlElement, xmlElement),
                            _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= roads of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   Text = string:strip(lists:flatten(R)),
   case string:equal(Text,"closed") of 
      true -> closed;
      false -> case string:equal(Text,"open") of
                 true -> open
               end
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% convert all the xml connections of the town to erlang records
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_airport(_XmlElement) when is_record(_XmlElement, xmlElement),
                               _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= airport of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   Text = string:strip(lists:flatten(R)),
   case string:equal(Text,"closed") of 
      true -> closed;
      false -> case string:equal(Text,"open") of
                 true -> open
               end
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% convert all the xml connections of the town to erlang records
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

town_harbor(_XmlElement) when is_record(_XmlElement, xmlElement),
                               _XmlElement#xmlElement.name =:= town ->
  XmlContent = _XmlElement#xmlElement.content,
  R = lists:map(  fun(X) -> 
                    case is_record(X, xmlElement) of
                      true  -> case X#xmlElement.name =:= harbor of
                                 true -> Head = hd(X#xmlElement.content),
                                         Head#xmlText.value;
                                 _ -> []
                               end;
                      _ -> []
                    end
                  end
                  ,
                  XmlContent
               ),
   Text = string:strip(lists:flatten(R)),
   case string:equal(Text,"closed") of 
      true -> closed;
      false -> case string:equal(Text,"open") of
                 true -> open
               end
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc build xml air connection entry 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_xml_town_connections(_State) when is_record(_State, townstate) ->
  Connections = _State#townstate.connections,
  build_xml_town_connections(Connections, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc build xml air connection entry 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_xml_town_connections([], _Acc) when is_list(_Acc) ->
  #xmlElement{
     name = connections,
     content = _Acc
  };
build_xml_town_connections(_Connections, _Acc) when is_list(_Connections) ->
  Head = hd(_Connections),
  Result = case element(1,Head) of
     aircon  -> build_xml_town_connection(Head,aircon  );
     roadcon -> build_xml_town_connection(Head,roadcon );
     shipcon -> build_xml_town_connection(Head,shipcon )
  end,
  NewAcc = lists:flatten(lists:append( [_Acc], [Result] )),
  build_xml_town_connections(tl(_Connections), NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc build xml air connection entry 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_xml_town_connection(_Con,_Name) when is_record(_Con,aircon),
                                           is_atom(_Name)  ->
  From       = atom_to_list(_Con#aircon.from),
  To         = atom_to_list(_Con#aircon.to),
  DistanceKm = float_to_list(_Con#aircon.distanceKm),

  #xmlElement{
      name    = _Name,
      content =
      [
        #xmlElement{
	   name = from,
	   content = [ #xmlText { value=From } ]
	}
        ,
        #xmlElement{
	   name = to,
	   content = [ #xmlText { value=To } ] 
	}
        ,
        #xmlElement{
	   name = distanceKm,
	   content = [ #xmlText { value=DistanceKm } ] 
	}
      ]
   };
build_xml_town_connection(_Con,_Name) when is_record(_Con,roadcon),
                                           is_atom(_Name)  ->
  From       = atom_to_list(_Con#roadcon.from),
  To         = atom_to_list(_Con#roadcon.to),
  DistanceKm = float_to_list(_Con#roadcon.distanceKm),

  #xmlElement{
      name    = _Name,
      content =
      [
        #xmlElement{
	   name = from,
	   content = [ #xmlText { value=From } ]
	}
        ,
        #xmlElement{
	   name = to,
	   content = [ #xmlText { value=To } ] 
	}
        ,
        #xmlElement{
	   name = distanceKm,
	   content = [ #xmlText { value=DistanceKm } ] 
	}
      ]
   };
build_xml_town_connection(_Con,_Name) when is_record(_Con,shipcon),
                                           is_atom(_Name)  ->
  From       = atom_to_list(_Con#shipcon.from),
  To         = atom_to_list(_Con#shipcon.to),
  DistanceKm = float_to_list(_Con#shipcon.distanceKm),

  #xmlElement{
      name    = _Name,
      content =
      [
        #xmlElement{
	   name = from,
	   content = [ #xmlText { value=From } ]
	}
        ,
        #xmlElement{
	   name = to,
	   content = [ #xmlText { value=To } ] 
	}
        ,
        #xmlElement{
	   name = distanceKm,
	   content = [ #xmlText { value=DistanceKm } ] 
	}
      ]
   }
.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc build xml coordinate entry
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_xml_town_coordinate(_State) when is_record(_State, townstate) ->
  Coords = _State#townstate.coordinate,
  Lat    = Coords#coords.latitude,
  Long   = Coords#coords.longitude,

  #xmlElement{
      name    = coordinate,
      content =
      [
        #xmlElement{
	   name = latitude,
	   content = 
                  [
                    #xmlText
                    {
                     value=float_to_list(Lat)
                    }
                  ]
	}
        ,
        #xmlElement{
	   name = longitude,
	   content = 
                  [
                    #xmlText
                    {
                     value=float_to_list(Long)
                    }
                  ]
	}
      ]
   }
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc build xml player <coordinate> </coordinate>
%% TODO: implement
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_xml_player_coordinate(_State) when is_record(_State, playerstate) ->
  #xmlElement
  {
     name   = coordinate,
     content = 
     [
       #xmlElement{ name = latitude,
		    content = 
		    [ #xmlText{ value = float_to_list(_State#playerstate.coordinate#coords.latitude) } ]
		  },
       #xmlElement{ name = longitude,
		    content = 
		    [ #xmlText{ value = float_to_list(_State#playerstate.coordinate#coords.longitude)} ]
		  }
     ]
  }
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc build xml record <location> </location>
%% TODO: implement
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_xml_player_location(_State) when is_record(_State, playerstate) ->
  #xmlElement
  {
     name   = location,
     content = 
     [
       #xmlElement{ name = name,
		    content = 
		    [ #xmlText{ value = atom_to_list(_State#playerstate.location#location.name) } ]
		  },
       #xmlElement{ name = latitude,
		    content = 
		    [ #xmlText{ value = float_to_list(_State#playerstate.location#location.latitude) } ]
		  },
       #xmlElement{ name = longitude,
		    content = 
		    [ #xmlText{ value = float_to_list(_State#playerstate.location#location.longitude) } ]
		  }
     ]
  }
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc build xml record <player> </player>
%% TODO: implement
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_xml_player_savegame(_State) when is_record(_State, playerstate) ->
  #xmlElement
  {
     name   = player,
     content = 
     [
       #xmlElement{ name = name,
		    content = 
		    [ #xmlText{ value = _State#playerstate.name } ]
		  },
       #xmlElement{ name = character,
		    content = 
		    [ #xmlText{ value = atom_to_list(_State#playerstate.character) } ]
		  },
       #xmlElement{ name = level,
		    content = 
		    [ #xmlText{ value = integer_to_list(_State#playerstate.level)} ]
		  },
       build_xml_player_location(_State), 
       build_xml_player_coordinate(_State), 
       #xmlElement{ name = activity,
		    content = 
		    [ #xmlText{ value = atom_to_list(_State#playerstate.activity)} ]
		  },
       #xmlElement{ name = paused,
		    content = 
		    [ #xmlText{ value = atom_to_list(_State#playerstate.paused)} ]
		  }
     ]
  }
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc build xml record <town> </town> from _State
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_xml_town_savegame(_State) when is_record(_State, townstate) ->
  XML = 
  #xmlElement{ 
    name          = town,
    content       = 
    [
      #xmlElement{
                  name = name,
                  content = 
                  [
                    #xmlText
                    {
                     value= _State#townstate.name
                    }
                  ]
                 }
      ,
      #xmlElement{
                  name = population, 
                  content = 
                  [
                    #xmlText
                    {
                     value= integer_to_list(_State#townstate.population )
                    }
                  ]
                 }
      ,
      #xmlElement{
                  name = birthrate,
                  content =
                  [
                    #xmlText
                    {
                     value = float_to_list( _State#townstate.birthrate )
                    }
                  ]
                 }
      ,
      #xmlElement{
                  name = infectionrate,
                  content =
                  [
                    #xmlText
                    {
                     value = float_to_list( _State#townstate.infectionrate)
                    }
                  ]
                 }
      ,
      #xmlElement{
                  name = lethality,
                  content =
                  [
                    #xmlText
                    {
                     value = float_to_list( _State#townstate.lethality)
                    }
                  ]
                 }
      ,
      #xmlElement{
                  name = travelrate,
                  content =
                  [
                    #xmlText
                    {
                     value = integer_to_list(_State#townstate.travelrate)
                    }
                  ]
                 }
      ,
      #xmlElement{
                  name = airport,
                  content =
                  [
                    #xmlText
                    {
                     value = atom_to_list( _State#townstate.airport)
                    }
                  ]
                 }
      ,
      #xmlElement{
                  name = roads,
                  content =
                  [
                    #xmlText
                    {
                     value = atom_to_list( _State#townstate.roads)
                    }
                  ]
                 }
      ,
      #xmlElement{
                  name = harbor,
                  content =
                  [
                    #xmlText
                    {
                     value = atom_to_list( _State#townstate.harbor)
                    }
                  ]
                 },
      build_xml_town_connections(_State),
      build_xml_town_coordinate(_State)
    ]
  },
  XML.

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

build_xml_town_savegame_test() ->
  StateMunich = 
  #townstate{ name            = "blubber",
              coordinate      = #coords
              { 
                latitude  = 48.144,
                longitude = 11.558
              },
              population      = 1300000,
              birthrate       = 0.4,
              infectionrate   = 1.0,
              lethality       = 0.3,
              infectedpopulation = 10,
              travelrate      = 300,
              connections     = [
				  #aircon{ from  = blubber,
					   to = munch,
					   distanceKm =  100.0 },
				  #roadcon{ from  = bla,
					   to = munch,
					   distanceKm =  10.0 },
				  #shipcon{ from  = bla,
					    to = munch,
					    distanceKm = 10.0 }
				 ],
              airport         = open,
              roads           = open,
              players         = []
            },

  XML = build_xml_town_savegame(StateMunich),

  {ok,IOF}=file:open('new_states_test.xml',[write]),
  Export=xmerl:export_simple([XML],xmerl_xml),
  io:format(IOF,"~s~n",[lists:flatten(Export)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc 
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_xml_player_savegame_test() ->
  StatePlayer= 
  #playerstate{ name            = "daniel",
                coordinate      = #coords
                { 
                  latitude  = 48.144,
                  longitude = 11.548
                },
	        location = #location
	        {
                  name = munich,
		  latitude = 48.144,
		  longitude = 11.558
	        },
	        activity = idle,
	        paused = false,
	        character = medic,
	        level = 3
              },

  XML = build_xml_player_savegame(StatePlayer),

  {ok,IOF}=file:open('new_states_player_test.xml',[write]),
  Export=xmerl:export_simple([XML],xmerl_xml),
  io:format(IOF,"~s~n",[lists:flatten(Export)])
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verify content of test config file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_config_test() ->
  XML               = xml_parser:parse_config_file("../config/test_game_config.xml"),
  XmlTownElements   = xml_parser:xml_town_elements_from_parsed_config_file(XML),
  XmlPlayerElements = xml_parser:xml_player_elements_from_parsed_config_file(XML),
  TownStates        = lists:map( fun(X) -> 
                            xml_parser:create_town_state_from_xml_town_element(X)
                           end,
                           XmlTownElements), 
  PlayerStates      = lists:map( fun(X) -> 
                            xml_parser:create_player_state_from_xml_player_element(X)
                           end,
                           XmlPlayerElements),

  XML =:= ?TESTXML.



-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
