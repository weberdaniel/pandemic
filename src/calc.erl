%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                         %%
%% File:        calc.erl                                                   %%
%%                                                                         %%
%% Description: This module perfoms calculation, e.g. calculates the       %%
%%              distance for a given set of latitude/longitude             %%
%%                                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HIGH PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LOW PRIORITY TASKS %%&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Increase Test coverage from 90% to 100%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Module Definition        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(calc).
-export([harvesine/2, course/2, great_circle_midpoint/2, 
	 great_circle_waypoint/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("records.hrl").

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc calculate distance between A and B using the harvesine formula
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

harvesine( A, B ) when is_record(A, coords), 
                      is_record(B, coords) ->

  DeltaLatitude  = (B#coords.latitude  - A#coords.latitude) /360.0*2.0*math:pi(),
  DeltaLongitude = (B#coords.longitude - A#coords.longitude)/360.0*2.0*math:pi(),

  X = math:pow( math:sin( DeltaLatitude/2.0 ), 2) + 
      math:cos( B#coords.latitude*math:pi()/180.0 ) * 
      math:cos( A#coords.latitude*math:pi()/180.0 ) * 
      math:pow( math:sin( DeltaLongitude/2.0 ), 2),

  Y = 2.0 * math:atan2( math:sqrt(X), math:sqrt(1.0-X) ),
  DistanceKm = 6371.0 * Y,

  DistanceKm;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc calculate distance between A and B using the harvesine formula
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

harvesine( A, B ) when is_record(A, location), 
                      is_record(B, location) ->

  DeltaLatitude = (B#location.latitude - A#location.latitude)/360*2*math:pi(),
  DeltaLongitude = (B#location.longitude - 
                    A#location.longitude)/360*2*math:pi(),

  X = math:pow( math:sin( DeltaLatitude/2.0 ), 2) + 
      math:cos( B#location.latitude*math:pi()/180.0 ) * 
      math:cos( A#location.latitude*math:pi()/180.0 ) * 
      math:pow( math:sin( DeltaLongitude/2.0 ), 2),

  Y = 2.0 * math:atan2( math:sqrt(X), math:sqrt(1-X) ),
  DistanceKm = 6371.0 * Y,

  DistanceKm.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc calculate the course between A and B. Returns {Lambda12, Alpha1,
%%      Alpha2, Sigma12, Distance}. All angles are in radian. Note that the
%%      course calculation is more precise than the harvesine formula.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

course( A, B ) when is_record(A, coords), 
                    is_record(B, coords) ->

  Phi1    = A#coords.latitude*math:pi()  / 180.0,
  Lambda1 = A#coords.longitude*math:pi() / 180.0,
  Phi2    = B#coords.latitude*math:pi()  / 180.0,
  Lambda2 = B#coords.longitude*math:pi() / 180.0,
  R = 6371.0,

  Lambda12 = case (Lambda2-Lambda1) > math:pi() of
               true  -> Lambda2-Lambda1-2.0*math:pi();
               false -> case (Lambda2-Lambda1) < (-math:pi()) of
                          true  -> (Lambda2-Lambda1+math:pi());
                          false -> (Lambda2-Lambda1)
                        end
             end,

  Alpha1 =  math:atan2( 
	      (math:cos(Phi2) * math:sin(Lambda12)) 
	      ,  
	      (math:cos(Phi1) * math:sin(Phi2) - math:sin(Phi1) * 
	       math:cos(Phi2) * math:cos(Lambda12))
	    ),


  Alpha2 = math:atan2(
	     (math:cos(Phi1) * math:sin(Lambda12))
	     ,
	     (- math:cos(Phi2) * math:sin(Phi1) + math:sin(Phi2) * 
	      math:cos(Phi1) * math:cos(Lambda12))
	   ),


  Sigma12 = math:atan2(
              math:sqrt
              (
                math:pow(
                  math:cos(Phi1) * math:sin(Phi2) - 
                  math:sin(Phi1) * math:cos(Phi2) * math:cos(Lambda12)
                  , 2)
                +
                math:pow((math:cos(Phi2) * math:sin(Lambda12)),2)
              )
              ,
              ( math:sin(Phi1) * math:sin(Phi2) + math:cos(Phi1) * 
                math:cos(Phi2) * math:cos(Lambda12) )
            ),

  Distance = Sigma12*R,
  {Lambda12/math:pi()*180.0, Alpha1/math:pi()*180, Alpha2/math:pi()*180.0, 
  Sigma12/math:pi()*180.0, Distance};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc calculate the course between A and B. Returns {Lambda12, Alpha1,
%%      Alpha2, Sigma12, Distance}. All angles are in radian. Note that the
%%      course calculation is more precise than the harvesine formula.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

course( A, B ) when is_record(A, location), 
                    is_record(B, location) ->

  Phi1    = A#location.latitude *math:pi()  / 180.0,
  Lambda1 = A#location.longitude*math:pi() / 180.0,
  Phi2    = B#location.latitude*math:pi()  / 180.0,
  Lambda2 = B#location.longitude*math:pi() / 180.0,
  R = 6371.0,

  Lambda12 = case (Lambda2-Lambda1) > math:pi() of
               true  -> Lambda2-Lambda1-2.0*math:pi();
               false -> case (Lambda2-Lambda1) < (-math:pi()) of
                          true  -> (Lambda2-Lambda1+math:pi());
                          false -> (Lambda2-Lambda1)
                        end
             end,

  Alpha1 =  math:atan2( 
	      (math:cos(Phi2) * math:sin(Lambda12)) 
	      ,  
	      (math:cos(Phi1) * math:sin(Phi2) - math:sin(Phi1) * 
	       math:cos(Phi2) * math:cos(Lambda12))
	    ),


  Alpha2 = math:atan2(
	     (math:cos(Phi1) * math:sin(Lambda12))
	     ,
	     (- math:cos(Phi2) * math:sin(Phi1) + math:sin(Phi2) * 
	      math:cos(Phi1) * math:cos(Lambda12))
	   ),

  Sigma12 = math:atan2
            (
              math:sqrt
              (
                math:pow(
                  math:cos(Phi1) * math:sin(Phi2) - 
                  math:sin(Phi1) * math:cos(Phi2) * math:cos(Lambda12)
                  , 2)
                +
                math:pow((math:cos(Phi2) * math:sin(Lambda12)),2)
              )
              ,
              ( math:sin(Phi1) * math:sin(Phi2) + math:cos(Phi1) * 
                math:cos(Phi2) * math:cos(Lambda12) )
            ),

  Distance = Sigma12*R,

  { Lambda12/math:pi()*180.0, Alpha1/math:pi()*180.0, Alpha2/math:pi()*180.0, 
   Sigma12/math:pi()*180.0, Distance }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc calculate new coordinates on the great circle (gc) between A and B, 
%%      just in the middle of the route.
%%
%%      Note this algorithm is copied from 
%%      https://en.wikipedia.org/wiki/Great-circle_navigation, 
%%      a copy is saved in pandemic/doc
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

great_circle_midpoint( A, B ) when is_record(A, coords), 
                      is_record(B, coords)  ->

  Phi1    = A#coords.latitude *math:pi()  / 180.0,
  Lambda1 = A#coords.longitude*math:pi() / 180.0,
  Phi2    = B#coords.latitude*math:pi()  / 180.0,
  Lambda2 = B#coords.longitude*math:pi() / 180.0,

  Lambda12 = case (Lambda2-Lambda1) > math:pi() of
               true  -> Lambda2-Lambda1-2.0*math:pi();
               false -> case (Lambda2-Lambda1) < (-math:pi()) of
                          true  -> (Lambda2-Lambda1+math:pi());
                          false -> (Lambda2-Lambda1)
                        end
             end,

  Alpha1 =  math:atan2( 
	      (math:cos(Phi2) * math:sin(Lambda12)) 
	      ,  
	      (math:cos(Phi1) * math:sin(Phi2) - math:sin(Phi1) * 
	       math:cos(Phi2) * math:cos(Lambda12))
	    ),

  Alpha0 = math:atan2
           (
	     (math:sin(Alpha1) * math:cos(Phi1))
	     ,
	     ( 
	       math:sqrt
	       (   
		math:pow(math:cos(Alpha1),2) + 
		math:pow(math:sin(Alpha1),2) * 
		math:pow(math:sin(Phi1),2)
               ) 
	     )
	   ),

  Sigma01 = math:atan2(math:tan(Phi1), math:cos(Alpha1)),

  Alpha2 = math:atan2(
	     (math:cos(Phi1) * math:sin(Lambda12))
	     ,
	     ( - math:cos(Phi2) * math:sin(Phi1) + math:sin(Phi2) * 
	      math:cos(Phi1) * math:cos(Lambda12) )
	   ),

  Sigma02 = math:atan2(math:tan(Phi2), math:cos(Alpha2)),

  Lambda01 = math:atan2( 
               ( math:sin(Alpha0) * math:sin(Sigma01) ),
               ( math:cos(Sigma01) )
             ),
                    
  Lambda0 = Lambda1 - Lambda01,

  %% now compute midpoint:

  Sigma = 0.5*(Sigma01+Sigma02),

  Phi = math:atan2(
	     math:cos(Alpha0) * math:sin(Sigma),
	     math:sqrt( 
	       math:pow(math:cos(Sigma),2)+
	       math:pow(math:sin(Alpha0),2)*
	       math:pow(math:sin(Sigma),2)
		      )
	),

  Lambda = math:atan2(
		      math:sin(Alpha0)*math:sin(Sigma),
                      math:cos(Sigma)
		     ) 
           + Lambda0,

                       
  {
   Lambda12/math:pi()*180.0, 
   Alpha1/math:pi()*180.0, 
   Alpha0/math:pi()*180.0, 
   Sigma01/math:pi()*180.0, 
   Sigma02/math:pi()*180.0, 
   Lambda01/math:pi()*180.0,
   Lambda0/math:pi()*180.0, 
   Sigma/math:pi()*180.0, 
   Phi/math:pi()*180.0, 
   Lambda/math:pi()*180.0
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc calculate new coordinates on the great circle (gc) between A and B, 
%%      with a Distance D from the starting point on the way to B.
%%
%%      Note this algorithm is copied from 
%%      https://en.wikipedia.org/wiki/Great-circle_navigation, 
%%      a copy is saved in pandemic/doc
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

great_circle_waypoint( A, B, DistanceKm ) when is_record(A, coords), 
                      is_record(B, coords), is_float(DistanceKm)  ->


  Phi1    = A#coords.latitude *math:pi()  / 180.0,
  Lambda1 = A#coords.longitude*math:pi() / 180.0,
  Phi2    = B#coords.latitude*math:pi()  / 180.0,
  Lambda2 = B#coords.longitude*math:pi() / 180.0,
  R       = 6371.0,

  Lambda12 = case (Lambda2-Lambda1) > math:pi() of
               true  -> Lambda2-Lambda1-2.0*math:pi();
               false -> case (Lambda2-Lambda1) < (-math:pi()) of
                          true  -> (Lambda2-Lambda1+math:pi());
                          false -> (Lambda2-Lambda1)
                        end
             end,

  Alpha1 =  math:atan2( 
	      (math:cos(Phi2) * math:sin(Lambda12)) 
	      ,  
	      (math:cos(Phi1) * math:sin(Phi2) - math:sin(Phi1) * 
	       math:cos(Phi2) * math:cos(Lambda12))
	    ),

  Alpha0 = math:atan2
           (
	     (math:sin(Alpha1) * math:cos(Phi1))
	     ,
	     ( 
	       math:sqrt
	       (   
		math:pow(math:cos(Alpha1),2) + 
		math:pow(math:sin(Alpha1),2) * 
		math:pow(math:sin(Phi1),2)
               ) 
	     )
	   ),

  Sigma01 = math:atan2(math:tan(Phi1), math:cos(Alpha1)),

  Lambda01 = math:atan2( 
               ( math:sin(Alpha0) * math:sin(Sigma01) ),
               ( math:cos(Sigma01) )
             ),
                    
  Lambda0 = Lambda1 - Lambda01,

  %% now compute waypoint:

  Sigma = Sigma01 + DistanceKm/R,

  Phi = math:atan2(
	     math:cos(Alpha0) * math:sin(Sigma),
	     math:sqrt( 
	       math:pow(math:cos(Sigma),2)+
	       math:pow(math:sin(Alpha0),2)*
	       math:pow(math:sin(Sigma),2)
		      )
	),

  Lambda = math:atan2(
		      math:sin(Alpha0)*math:sin(Sigma),
                      math:cos(Sigma)
		     ) 
           + Lambda0,

  {Phi/math:pi()*180.0,Lambda/math:pi()*180.0}
.
	


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test the course/2 function, with #coords arguments.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

course_coords_test() ->

  Valparaiso = #coords{ latitude = -33 , longitude = -71.6 },
  Shanghai   = #coords{ latitude = 31.4, longitude = 121.8 },

  {-166.60000000000002,
   -94.41302236945862,
   -78.42236042001993,
   168.5567762850174,
   18742.658374455805} = course(Valparaiso, Shanghai).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test the harvesine/2 function, with #coords arguments
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

harvesine_coords_test() ->

  A = #coords{ latitude = 51.5007 , longitude = 0.1246 },
  B = #coords{ latitude = 40.6892,  longitude = 74.0445 },

  Result = harvesine(A,B),
  ?assert( 5574 =:= trunc(harvesine(A, B))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test the harvesine/2 function, with #location arguments
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

harvesine_location_test() ->

  A = #location{ latitude = 51.5007 , longitude = 0.1246 },
  B = #location{ latitude = 40.6892,  longitude = 74.0445 },

  Result = harvesine(A,B),
  ?assert( 5574 =:= trunc(harvesine(A, B))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test the course/2 function, with #location arguments
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

course_location_test() ->

  Valparaiso = #location{ latitude = -33 , longitude = -71.6 },
  Shanghai   = #location{ latitude = 31.4, longitude = 121.8 },

  {-166.60000000000002,
   -94.41302236945862,
   -78.42236042001993,
   168.5567762850174,
   18742.658374455805} = course(Valparaiso, Shanghai).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test to calculate the midpoint between two coordinate records.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

great_cricle_midpoint_test() ->

  Valparaiso = #coords{ latitude = -33 , longitude = -71.6 },
  Shanghai   = #coords{ latitude = 31.4, longitude = 121.8 },

  { 
    -166.60000000000002,
    -94.41302236945862,
    -56.73934232551105,
    -96.75723778796355,
    71.79953849705383,
    98.06502439887996,
    -169.66502439887995,
    -12.478849645454861,
    -6.806024577533078,
    -159.18082868525363
  } = great_circle_midpoint(Valparaiso, Shanghai).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc test to calculate a waypoint with a distance D from the starting
%%      point A in the direction of the end-point B. In this test we use
%%      half the distance between A and B, to find the midpoint.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

great_cricle_waypoint_test() ->

  Valparaiso = #coords{ latitude = -33 , longitude = -71.6 },
  Shanghai   = #coords{ latitude = 31.4, longitude = 121.8 },

  { -6.805196138288599,
    -159.18213148936573 } = great_circle_waypoint(Valparaiso, Shanghai, 18743.0/2.0).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
