%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                         %%
%% File:        auth.erl                                                   %%
%%                                                                         %%
%% Description: This is the authentification server. It reads all users    %%
%%              from a CVS file and provides AccessTokens to everyone      %%
%%              who authentificates correctly via login(...). To logout    %%
%%              use logout(...). The AccessTokens can then be used to      %%
%%              verify() a user. Also the server holds information on      %%
%%              level and character which can be retrieved                 %%
%%              by character() and level() of a player to validate if a    %%
%%              certain action is allowed or not. It is also possible to   %%
%%              verify admin privileges of a user with verfiy_admin(..)    %%
%%              instead of verify(..). In the future it shall be possible  %%
%%              to create users, then pause the process and write the      %%
%%              configuration to a file, while the process is in the       %%
%%              pause state.                                               %%
%%                                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HIGH PRIORITY TASKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LOW PRIORITY TASKS %%&%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: implement create_user
%% TODO: implement a test case for the save functionality

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Module Definition        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(auth).
-behaviour(gen_server).
-export([
         help/0, 
         login/3, 
         create_user/4, 
         verify/2, 
         logout/3, 
         stop/1,
         code_change/3, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         init/1, 
         terminate/2, 
         start/0, 
         start_link/0, 
         start/1, 
         start_link/1, 
         verify_admin/2, 
         pause/2, 
         resume/2, 
         help/1,
         level/2, 
	 save/3,
         character/2]).

-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
  -export([state/2]).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function gives an overview of all available interface
%%      functions.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help() ->
  io:format(" login(PID, Username, Password)~n"),
  io:format(" create_user(PID, Username, Password, AdminToken)~n"),
  io:format(" verify(PID, Username, Token)~n"),
  io:format(" character(PID, _Token)~n"),
  io:format(" level(PID, _Token)~n"),
  io:format(" logout(PID, Username, Token) ~n"),
  io:format(" verify_admin(PID, Token) ~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function gives detailed information on all interface
%%      functions.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help(login) ->
  io:format("  login(PID, Username, Password): ~n"
	    "    Login to the auth server. Return {ok, AccessToken} or ~n"
	    "    {fail}. ~n");
help(logout) ->
  io:format(" logout(PID, Username, Token): Logout and revoke the  ~n"
            "        AccessToken. Return {ok} or {fail}. ~n");
help(verify) ->
  io:format(" verify(PID, Username, Token): Verify that the username ~n"),
  io:format("        belongs to the Token. Returns {verify_ok, Username}"),
  io:format("~n        or {verify_failed}. ~n");
help(create_user) ->
  io:format(" ");
help(character) ->
  io:format(" ");
help(level) ->
  io:format(" level(PID, Token): ");
help(verify_admin) ->
  io:format(" ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {login, _AdminToken}
%%      message to the server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

login(_PID, _Username, _Password) when is_list(_Username), 
				       is_list(_Password) ->
  gen_server:call(_PID, {login, _Username, 
                lists:flatten(io_lib:format("~p",[erlang:phash2(_Password)]))}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {pause, _AdminToken}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pause(_PID, _AdminToken) when is_list(_AdminToken) ->
  gen_server:call(_PID, {pause, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {pause, _AdminToken}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save(_PID, _File, _AdminToken) when is_list(_AdminToken), is_list(_File) ->
  gen_server:call(_PID, {save, _File, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {state, _AdminToken}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

state(_PID, _AdminToken) when is_list(_AdminToken) ->
  gen_server:call(_PID, {state, _AdminToken}).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface functions sends a synchronous {level, _Token}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

level(_PID, _Token) when is_list(_Token) ->
  gen_server:call(_PID, {level,_Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {character, _Token}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

character(_PID, _Token) when is_list(_Token) ->
  gen_server:call(_PID, {character, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {resume, _AdminToken}
%%      message to the server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resume(_PID, _AdminToken) when is_list(_AdminToken) ->
  gen_server:call(_PID, {resume, _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc NOT IMPLEMENTED YET.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_user(_PID, _Username, _Password, _AdminToken) ->
  gen_server:call(_PID, {create_user, _Username, 
                 lists:flatten(io_lib:format("~p",[erlang:phash2(_Password)])),
                  _AdminToken}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface functions sends a synchronous {verify, _Token}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify(_PID, _Token) when is_list(_Token) ->
  gen_server:call(_PID, {verify, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {verify_admin, _Token}
%%      message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_admin(_PID, _Token) when is_list(_Token) ->
  gen_server:call(_PID, {verify_admin, _Token}, 5000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface function sends a synchronous {logout, _Username,
%%      _Token} message to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logout(_PID, _Username, _Token) when is_list(_Username),
				     is_list(_Token) ->
  gen_server:call(_PID, {logout, _Username, _Token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This interface functions sends a synchronous {stop} message
%%      to the Server located at _PID.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
%% @doc This function returns character of the user if _Token is valid. 
%%      Else fail.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_character_by_access_token_if_online(_Token, _Records) when is_list(_Records), 
					      is_list(_Token) ->

  % call get_character(_Token,X) for each element X of _Records in order to
  % find the character type belonging to the token
	
  Result = lists:map( 
	              fun(X) ->
                        get_character_if_online(_Token, X)
                      end, 
                      _Records 
		    ),

  % if found, there will be an {ok,Character} in the list. Find this
  % and if found return the Character.
  
  KeySearch = lists:keysearch(ok,1,Result),

  case KeySearch of
    {value, {ok, Character}} -> Character;
    false                    -> fail
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function returns the level of the user, if _Token is valid. Else 
%%      fail.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_level_by_access_token_if_online(_Token, _Records) when is_list(_Records), 
							   is_list(_Token) ->

  % call get_level(_Token,X) for each element X of _Records in order to
  % find the level belonging to the token

  Result = lists:map( fun(X) ->
                        get_level_if_online(_Token, X)
                      end, 
                      _Records ),
  
  % if found, there will be an {ok,Level} in the list. Find this
  % and return the corresponding Level.
 
  KeySearch = lists:keysearch(ok,1,Result),
  case KeySearch of
    {value, {ok, Level}} -> Level;
    false                -> fail
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function returns Username if the _Token is valid. Else fail.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_access_token(_Token, _Records) when is_list(_Records), is_list(_Token) ->

  % call verify_token(_Token,X) for each element X of _Records in order to
  % check if the token is correct for each record.
	
  Result = lists:map( 
	              fun(X) ->
                        verify_token(_Token, X)
                      end, 
                      _Records 
	            ),

  % if an ok is in the list, then the access token belongs to one of the
  % users and thus is correct. 
  
  KeySearch = lists:keysearch(ok,1,Result),

  case KeySearch of
    {value, {ok, Username}} -> Username;
    false ->                   fail
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function verifies a single _Token against a single #auth record. 
%%      Return {ok, Username} or {fail}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_token(_Token, X) when is_record(X, auth), is_list(_Token) ->
  if X#auth.accesstoken == _Token, X#auth.loginstate == online ->
       {ok, X#auth.username};
  X#auth.accesstoken =/= _Token; X#auth.loginstate =/= online ->
       {fail}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function compares a _Token to an #auth record. If the _Token 
%%      belongs to the record, and if the loginstate is set to online, then 
%%      return {ok,LevelOfUser}. Else return {fail}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_level_if_online(_Token, X) when is_record(X, auth), is_list(_Token) ->

  % compare the token and check that the loginstate is online
  % if the loginstate is online return {ok, Level}
	
  if X#auth.accesstoken == _Token, X#auth.loginstate == online ->
       {ok, X#auth.level};

  % if the accestoken is not valid or state is not online return fail.
     
     X#auth.accesstoken =/= _Token; X#auth.loginstate =/= online ->
       {fail}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function checks if a _Token belongs to an #auth record X. If the 
%%      state of the #auth record is online and if the _Token belongs to the 
%%      #auth record, return the character type of the player {ok, Character} 
%%      or else return {fail}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_character_if_online(_Token, X) when is_record(X, auth), is_list(_Token) ->
	
  % compare the token and check that the loginstate is online
  % if so return {ok, Character}

  if X#auth.accesstoken == _Token, X#auth.loginstate == online ->
       {ok, X#auth.character};
     
  % if the token does not match or the loginstate is not online,
  % return {fail}
     
  X#auth.accesstoken =/= _Token; X#auth.loginstate =/= online ->
       {fail}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function creates a new access token, belonging to _Username, 
%%      _HashedPassword. Returns the Token.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_access_token(_Username, _HashedPassword) ->
  Token = erlang:phash2([node(), erlang:monotonic_time(), 
          erlang:unique_integer(), _Username, _HashedPassword]),
  Token.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function reads a file consisting of lines of the following format:
%%      username, hashed_password, adminflag, character, level
%%      Returns a list of #auth records that are created by each of the lines
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_lines(_Filename) when is_list(_Filename) ->

  {ok, Data} = file:read_file(_Filename),

  if size(Data) > 0 ->
    Lines = binary:split(Data, <<$\n>>,[global]),
    Records = foreach_line(fun parse_line/2 , Lines),
    Records;
  size(Data) == 0 ->
    fail;
  size(Data) < 0 ->
    fail
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function executes Function _F for each element of _Lines
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
foreach_line(_F, _Lines) when is_list(_Lines)->
  foreach_line(_F, _Lines, []).
foreach_line(_F, [], _Acc) ->
  _F([], _Acc);
foreach_line(_F, _Lines, _Acc) when is_list(_Lines) ->
  NewAcc = _F(hd(_Lines), _Acc),
  foreach_line(_F, tl(_Lines), NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function tries to authentificate _Username and _HashedPassword 
%%      against a list of #auth records. This can only succeed if the 
%%      loginstate is offline.
%%
%%      Returns fail, fail_no_loginstate, fail_no_password or ok.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

authentificate(_Username, _HashedPassword, _Records) 
when is_list(_Records), is_list(_HashedPassword), is_list(_Records) ->

  % execute authentificat for each of the #auth records. If one record can
  % be authentificated, ok is in to the list.

  Result = lists:map( fun(X) -> 
                        authentificate(_Username, _HashedPassword, X) 
                      end,
                      _Records),

  % Search if there is an ok in the list, if so, the authentification
  % suceeds and ok is returned. Else extract the most probable reason for 
  % failure. If a user exists, then there is a fail_no_password in the list, 
  % this is the most probable reason for failure. If the password was correct 
  % but the user is already logged in, a fail_no_loginstate will be present. As 
  % the password and username were correct, this is the obvious reason for 
  % failure. If no user was present with that name, fail_no_user is returned.

  case lists:member(ok, Result) of
    true -> ok;
    false -> case lists:member(fail_no_password, Result) of
               true  -> fail_no_password;
               false -> case lists:member(fail_no_loginstate, Result) of
                          true -> fail_no_loginstate;
                          false -> case lists:member(fail_no_user,Result) of
			             true -> fail_no_user;
				     false -> fail
                                   end
                        end
             end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function tries to authentificate _Username and _HashedPassword 
%%      against one single #auth record _Record. This can only succeed if the 
%%      loginstate is offline, because logging in two times is not permitted.
%%
%%      Returns fail_no_password, fail_no_loginstate, fail_no_user or ok
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

authentificate(_Username, _HashedPassword, _Record) 
when is_record(_Record, auth), is_list(_HashedPassword), is_list(_Username) ->

  % if the username and password is equal and loginstate is offline, return ok.
  % else return fail_no_loginstate or fail_no_password or fail_no_user

  case _Username =:= _Record#auth.username of
    true  -> case string:equal(_HashedPassword, 
                               _Record#auth.hashedpassword) of

               %if pw and username match, check if loginstate is online or
	       %offline. If online, return fail_no_loginstate. Only persons
	       %shall be allowed to authentificate, which are currently 
               %offline.
		     
               true -> case _Record#auth.loginstate of
                         online  -> fail_no_loginstate;
                         offline -> ok
                       end;

               %pw and username don't match, so return fail_no_password
		     
               false -> {fail_no_password, _HashedPassword, 
                         _Record#auth.hashedpassword}
             end;
    %username don't match
    false -> fail_no_user
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function reads the lines of the authentification file. 
%%      returns a list of #auth records. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_line([],Acc) ->
  Acc;
parse_line(_Line,Acc) when is_binary(_Line) ->

  % split the entries to a list of comma separated values. 

  Entries = binary:split(_Line, [<<",">>],[global]),

  % only lines are valid that contain exactly 5 comma separated
  % values, else just return the Accumulator again and ignore the
  % line

  case length(Entries) of

    %
    % ignore all lines, with less than 5 entries 
    %

    0 -> Acc;
    1 -> Acc;
    2 -> Acc;
    3 -> Acc;
    4 -> Acc;

    % if five entries are present, construct the corresponding
    % #auth record

    5  -> Username = binary_to_list(lists:nth(1,Entries)),
          Password = binary_to_list(lists:nth(2,Entries)),
          Character = binary_to_list(lists:nth(3,Entries)),
          Level =    binary_to_list(lists:nth(4,Entries)),
          Admin    = binary_to_list(lists:nth(5,Entries)),

          Record = #auth{ username       = Username,
                          hashedpassword = Password,
                          admin          = case string:strip(Admin) of
                                                   "true" -> true;
                                                   "false" -> false
                                                 end,
                          loginstate     = offline,
                          accesstoken    = undef,
                          character      = list_to_atom(Character),
                          level          = list_to_integer(
					     string:strip(Level)
					   )
                        },

          lists:flatten( [Acc], [Record] )
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function writes an authentification file in the format:
%%
%%      User,HashedPassword,Character,Level,Admin
%%
%%      by reading the input from _Records, which must be a list of #auth
%%      Records.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_file(_Filename,_Records) when is_list(_Filename), is_list(_Records) ->

  % use the function authrecord_to_string/1 on each of the #auth records, to
  % create the strings that can be written to the file.

  Lines = lists:map( fun authrecord_to_string/1, _Records),

  % create what will be written to the file, by concatenating the Lines with
  % newline characters.

  FileContent = case length(Lines) > 1 of
                  true -> string:join( Lines,"\n");
                  false -> Lines
                end,

  % convert the list to a binary list for writing it to a file.

  BinaryContent = list_to_binary([FileContent,"\n"]),
  
  % write the BinaryContent to the file
  
  file:write_file(_Filename, BinaryContent).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function creates a string from an #auth record that can be written 
%%      to a file in the format: 
%% 
%%      User, HashedPassword, Character, Level, Admin
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

authrecord_to_string(_Record) when is_record(_Record, auth) ->

  % read information from #auth record and store it into variables

  Username       = _Record#auth.username,
  HashedPassword = _Record#auth.hashedpassword,
  Admin          = _Record#auth.admin,
  Character      = _Record#auth.character,
  Level          = _Record#auth.level,

  % create a CSV string and return it.

  lists:concat([Username, ",", HashedPassword, ",", Character, ",",
                Level, ",", Admin]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function sets the _AccessToken and the _Loginstate for a user 
%%      _Username inside of list of different auth records _Records. Update 
%%      the list of #auth records and return the new List.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_accesstoken_and_loginstate(_Username, _Token, _Records,_Loginstate) 
when is_list(_Username), is_list(_Token), is_list(_Records), 
     is_atom(_Loginstate) ->

  % iterate all existing auth records and search for the _Username.
  % if found, then set the accesstoken to _Token and the loginstate to
  % _Loginstate

  NewRecords = 
    lists:map( 
               fun(X) ->
                 case string:equal(X#auth.username, _Username) of
                   true -> Y = X#auth{ accesstoken = _Token,
                                 loginstate  = _Loginstate}, 
                           Y;
                   false -> X
                 end
               end,
               _Records
	     ),

  % return the new list of states.

  NewRecords.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function sets a _Username to be offline inside the correct #auth 
%%      record in a list of #auth records named _Records. The function returns
%%      the new - updated - list.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_offline(_Username, _Records) when is_list(_Records), is_list(_Username) ->

  % iterate over all records and search for _Username. Set the accesstoken
  % to undef and the loginstate to offline.

  NewRecords = 
    lists:map( fun(X) ->
                 case string:equal(X#auth.username, _Username) of
                   true -> Y = X#auth{ accesstoken = undef,
                                 loginstate  = offline}, 
                           Y;
                   false -> X
                 end
               end,
               _Records),

  % return the updated list of records.

  NewRecords.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function verifys if the _Token has Admin prvileges. To do so, it
%%      checks the list of #auth records, _Records. It can either return ok
%%      or it can return fail.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_admin_token(_Token,_Records) when is_list(_Token), is_list(_Records) ->

  % for each record, check if the accesstoken exists, if it exists and is
  % equal to _Token, then check if the #auth.admin field, which indicates
  % admin permission is set or not. If it is set, then return ok, else
  % return fail.
	
  % thus a list emerges, containing fail and ok.

  Result = lists:map( fun(X) ->
              case is_record(X,auth) of
                true -> case X#auth.accesstoken of
		          undef -> fail;
		          _  -> case 
                                string:equal(X#auth.accesstoken, _Token) of
                                  true -> case X#auth.admin of
                                            true -> ok;
                                            _    -> fail
                                          end;
                                  _    -> fail
                                end
		        end;
	        _ -> fail
	      end
	      end,
             _Records ),

  % search if an ok is present and return true or false.

  lists:member(ok, Result).

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
%% @doc This function starts the auth server without linking it to the 
%%      starting process. (Linking in Erlang means, that if one process
%%      crashes, both processes are terminated).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function starts the auth server from a file without linking it
%%      to the starting process. The argument _Filename is used to get the 
%%      authentification records from. (Linking in Erlang means, that if one
%%      process crashes, both processes are terminated).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(_Filename) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [_Filename],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function starts the auth server via start_link, but does not
%%      provide a file from which to initialize. A link is established 
%%      between the calling process and the started process. This means if
%%      one process dies, both processes die.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function starts the auth server via start_link. The argument
%%      _Filename is used to get the authentification records from. A link
%%      is established between the calling process and the started process.
%%      This means if one process dies, both processes die.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(_Filename) -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [_Filename],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function initializes the auth server with an empty list of
%%      authentification records.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
  rand:seed(exs1024,{erlang:phash2([node()]),
              erlang:monotonic_time(),
              erlang:unique_integer()}),
  State = #authserverstate{ records = []  } ,
  {ok, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc Initialize the server from a file and create a list of #auth records
%%      from that file.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([_Filename]) when is_list(_Filename) ->
  rand:seed(exs1024,{erlang:phash2([node()]),
              erlang:monotonic_time(),
              erlang:unique_integer()}),
  Result = read_lines(_Filename),
  State  = #authserverstate{ records = Result },
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

handle_call({state, _AdminToken},_From,_State) 
when is_record(_State, authserverstate), is_list(_AdminToken) -> 
  UsernameOfToken = verify_access_token(
                       _AdminToken,_State#authserverstate.records),
  IsAdmin = is_admin_token(_AdminToken, _State#authserverstate.records),
  if 
      %% if Username is not present OR no admin privileges are present,
      %% return {verify_admin_failed}.

      UsernameOfToken == fail; IsAdmin == false ->
          {reply, {verify_admin_failed}, _State};

      %% if Username exists AND admin privileges are present, pause the
      %% state and return {paused}.

      is_list(UsernameOfToken), IsAdmin == true ->
          {reply, {_State}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc handle an incoming synchronous {pause, _AdminToken} message. Verify 
%%      if that AdminToken is correct, otherwise reply with 
%%      {verify_admin_failed}. If the AdminToken is correct, then set the 
%%      state of the process to paused.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({pause, _AdminToken},_From,_State) 
when is_record(_State, authserverstate) -> 
  UsernameOfToken = verify_access_token(
                       _AdminToken,_State#authserverstate.records),
  IsAdmin = is_admin_token(_AdminToken, _State#authserverstate.records),

  if 
      %% if Username is not present OR no admin privileges are present,
      %% return {verify_admin_failed}.

      UsernameOfToken == fail; IsAdmin == false ->
          {reply, {verify_admin_failed}, _State};

      %% if Username exists AND admin privileges are present, pause the
      %% state and return {paused}.

      is_list(UsernameOfToken), IsAdmin == true ->
          NewState = _State#authserverstate{ paused = true },
          {reply, {paused}, NewState}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {resume,_AdminToken} 
%%      message.  It checks if the User belonging to the Token has Admin 
%%      Privileges and returns {verify_admin_failed}. If Admin Privileges 
%%      are detected, the function replies with a {resumed} message.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({resume, _AdminToken},_From,_State) 
when is_record(_State, authserverstate)-> 

  UsernameOfToken = verify_access_token(_AdminToken,
                                        _State#authserverstate.records),
  IsAdmin         = is_admin_token(     _AdminToken, 
                                        _State#authserverstate.records),

  if 
      %% if Username is not present OR no admin privileges are present,
      %% return {verify_admin_failed}.

      UsernameOfToken == fail; IsAdmin == false ->
          {reply, {verify_admin_failed}, _State};

      %% if Username exists AND admin privileges are present, resume the
      %% state and return {resumed}.

      is_list(UsernameOfToken), IsAdmin == true ->
          NewState = _State#authserverstate{ paused = false },
          {reply, {resumed}, NewState}

  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handles an incoming synchronous {verify,_Token} 
%%      message. It checks if the _Token is correct and replies with
%%      either {verify_failed} or {verify_ok,  _Username}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({verify, _Token},_From,_State) 
when is_record(_State, authserverstate), is_list(_Token) -> 

  case _State#authserverstate.paused of
    true  -> {reply, {paused}, _State};
    _ -> 
      % get the username belonging to the accesstoken

      Result = verify_access_token(_Token,_State#authserverstate.records),

      if Result == fail ->
        {reply, {verify_failed}, _State};
      is_list(Result) ->
        {reply, {verify_ok, Result}, _State}
      end
   end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {character, _Token} 
%%      message and verifies the _Token. It responds with {verify_failed},
%%      or it responds with a message containing the character type of the
%%      user {verify_ok, Character}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({character, _Token},_From,_State) 
when is_record(_State, authserverstate), is_list(_Token) -> 

  case _State#authserverstate.paused of
    true -> 
      {reply, {paused}, _State};
    _ ->
      % get the character belonging to the accesstoken
      Result = get_character_by_access_token_if_online(
	       _Token, _State#authserverstate.records),

      if Result =:= fail ->
        {reply, {verify_failed}, _State};
      is_atom(Result) ->
        {reply, {verify_ok, Result}, _State}
      end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {save, File, _AdminToken} 
%%      message and verifies the _Token. It responds with {verify_failed},
%%      or it responds with a message {saved}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({save, _File, _AdminToken},_From,_State) 
when is_record(_State, authserverstate), is_list(_AdminToken) -> 

  case _State#authserverstate.paused of
    true -> 
      UsernameOfToken = verify_access_token(_AdminToken,
                                        _State#authserverstate.records),
      IsAdmin         = is_admin_token(_AdminToken, 
                                       _State#authserverstate.records),

      if 
          %% if Username is not present OR no admin privileges are present,
          %% return {verify_admin_failed}.

          UsernameOfToken == fail; IsAdmin == false ->
              {reply, {verify_admin_failed}, _State};

          %% if Username exists AND admin privileges are present, save the
          %% game and return {paused}.

          is_list(UsernameOfToken), IsAdmin == true ->
            write_file(_File, _State#authserverstate.records)
      end;
    _ ->
      {reply, {not_paused}, _State}
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchonrous {level, _Token} 
%%      request message. It verifies the _Token and either responds with
%%      {verify_failed} or with {verify_ok, LevelOfCharacter}.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({level, _Token},_From,_State) 
when is_record(_State, authserverstate), is_list(_Token) -> 

  case _State#authserverstate.paused of
    true -> 
      {reply, {paused}, _State};
    _ ->
      % get the level belonging to the accesstoken

      Result = get_level_by_access_token_if_online(_Token,
	          _State#authserverstate.records),

      if Result =:= fail ->
         {reply, {verify_failed}, _State};
      is_integer(Result) ->
        {reply, {verify_ok, Result}, _State}
      end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handles an incoming synchronous {verify_admin, _Token} 
%%      message. Call verify_access_token and return either 
%%      {verify_admin_failed} or {verify_admin_ok, _Username}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({verify_admin, _Token},_From,_State) 
when is_list(_Token), is_record(_State,authserverstate) -> 

  case _State#authserverstate.paused of
    true -> 
      {reply, {paused}, _State};
    _ ->
      % get the username of the token and check if it has admin privileges

      UsernameOfToken = verify_access_token(_Token,_State#authserverstate.records),
      IsAdmin         = is_admin_token(_Token, _State#authserverstate.records),

      if UsernameOfToken == fail; IsAdmin == false  ->
         {reply, {verify_admin_failed}, _State};
      is_list(UsernameOfToken), IsAdmin == true ->
        {reply, {verify_admin_ok, UsernameOfToken}, _State}
      end
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incoming synchronous {login,_Username,
%%      _HashedPassword} message to login to the server. Receive a hashed 
%%      password and compares it with the stored hashed password in the database. 
%%      If they match, then create an Accesstoken and answer with 
%%      {ok, StringToken}. 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({login, _Username, _HashedPassword},_From,_State) 
when is_record(_State,authserverstate), 
     is_list(_Username), 
     is_list(_HashedPassword) -> 

  case _State#authserverstate.paused of
    true -> 
      {reply, {paused}, _State};
    _ ->
      % try to authentificate the user, that wants to login

      Result = authentificate(_Username, _HashedPassword, 
                              _State#authserverstate.records),

      if Result == ok ->

        % create a new access token and add it to the username, also
        % set the new loginstate of the user to online.
	     
        Token = generate_access_token(_Username, _HashedPassword),
        StringToken = lists:flatten(io_lib:format("~p", [Token])),
        OldRecords = _State#authserverstate.records,
        NewRecords = set_accesstoken_and_loginstate(_Username, StringToken, 
                                                   OldRecords, online),
        NewState = _State#authserverstate{records = NewRecords},
        {reply, {ok, StringToken}, NewState };
      true ->
        {reply, {Result}, _State}
      end
  end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc This function handels an incomming synchonous {logout, _Username, 
%%      _Token} message. It verfies the token and replies {fail} if not
%%      possible. Otherwise it removes the token from the database and
%%      replies with {ok}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({logout, _Username, _Token},_From,_State) -> 

  case _State#authserverstate.paused of
    true -> 
      {reply, {paused}, _State};
    _ ->
      % check if the _Token belongs to the _Username

      Result = verify_access_token(_Token,_State#authserverstate.records),

      if Result == fail ->
        {reply, {fail}, _State};
      is_list(Result) ->

       % delete the access token of the user and set loginstate to offline
	     
       NewRecords = set_offline( _Username, _State#authserverstate.records ),
       NewState = _State#authserverstate{ records = NewRecords },
       {reply, {ok}, NewState}
     end
  end.

handle_info(_,_)   -> {ok}.
handle_cast({stop},_State)   -> {stop,normal,_State}.
code_change(_,_,_) -> {ok}.
terminate(_,_)     -> {ok}.

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
%% @doc try to read lines from "auth_testfile1" and verify the content
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_file_test() ->
  Result = read_lines("../auth_testfile1"),
  Result =:= #auth{ username       = "Daniel",
                    hashedpassword = "blabla",
                    admin          = true,
		    level          = 1,
		    character      = mechanic  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to read lines from "auth_testfile2" and verify the content
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_file2_test() ->
  Result = read_lines("../auth_testfile2"),
  Result =:= [#auth{ username          = "username",
                        hashedpassword = "hello",
                        admin          = true,
		        character      = medic,
		        level          = 2  },
              #auth{ username          = "vuffi",
                        hashedpassword = "raa",
                        admin          = true,
		        character      = politic,
		        level          = 1  }].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to write an auth record to a string and check if that string
%%      has the expected form
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

authrecord_to_string_test() ->
 R = #auth{ username       = "vuffi",
            hashedpassword = "raa",
            level          = 3,
	    character      = medic,
            admin          = true  },
  Result = authrecord_to_string(R),
  Result =:= "vuffi,raa,medic,3,true".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to write auth records to a file.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_authrecord_to_file_test() ->
 R = [#auth{ username             = "r2",
                   hashedpassword = "d2",
                   admin          = true,
	           character      = medic,
	           level          = 3 },
      #auth{ username             = "k2",
                   hashedpassword = "so4",
                   admin          = true,
	           character      = medic,
		   level          = 3 }],
 write_file("../output_test2", R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to write auth records to a file and read from it 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_authrecord_to_file_and_read_it_test() ->
 R = [#auth{ username             = "r2",
             hashedpassword = "d2",
             admin          = true,
	     loginstate     = offline,
	     accesstoken    = undef,
             character      = medic,
	     level          = 3 },
      #auth{ username             = "k2",
             hashedpassword = "so4",
             admin          = true,
	     loginstate     = offline,
	     accesstoken    = undef,
	     character      = medic,
	     level          = 3 }],
 write_file("../output_test2", R),
 Result = read_lines("../output_test2"),
 ?assert( Result =:= R ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start the auth server and stop the server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_start_and_stop_server_from_file_test() ->
  {ok, PID} = auth:start("../auth_testfile1"),
  auth:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start the auth server, login and stop the server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_login_test() ->
  {ok, PID} = auth:start("../auth_testfile3"),
  {ok, _}   = auth:login(PID, "Daniel", "blabla"),
  auth:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start the auth server, login, verify the token and stop the server
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_verify_test() ->
  {ok, PID} = auth:start("../auth_testfile3"),
  {ok, Token} = auth:login(PID, "Daniel", "blabla"),
  {verify_ok, "Daniel" } = auth:verify(PID, Token),
  auth:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc start the auth server and try to verify a token when not logged in and 
%%      check that the verifcation fails as expected.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_verify_when_not_logged_in_test() ->
  {ok, PID} = auth:start("../auth_testfile3"),
  {verify_failed} = auth:verify(PID, "abcd"),
  auth:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to login and try to verfy a token, then logout and check that the
%%      token has become invalid.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_login_logout_verify_success_verify_failed_test() ->
  {ok, PID} = auth:start("../auth_testfile3"),
  {ok, Token} = auth:login(PID, "Daniel", "blabla"),
  {verify_ok, "Daniel" } = auth:verify(PID, Token),
  {ok} = auth:logout(PID, "Daniel", Token),
  {verify_failed} = auth:verify(PID, Token),
  auth:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to verify the character level of a certain player
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_verify_character_level_test() ->
  {ok, PID}   = auth:start("../auth_testfile3"),
  {ok, Token} = auth:login(PID,"Daniel","blabla"),
  {verify_ok,3} = auth:level(PID,Token),
  auth:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to verify the character level of a certain player
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_verify_character_level_fail_test() ->
  {ok, PID}   = auth:start("../auth_testfile3"),
  {ok, Token} = auth:login(PID,"Daniel","blabla"),
  {verify_failed} = auth:level(PID,"asdfasdf"),
  auth:stop(PID).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to verify the character of a certain player
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_verify_character_test() ->
  {ok, PID}   = auth:start("../auth_testfile3"),
  {ok, Token} = auth:login(PID,"Daniel","blabla"),
  {verify_ok,medic} = auth:character(PID,Token),
  auth:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to fail the character level verification
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_fail_verify_character_test() ->
  {ok, PID}   = auth:start("../auth_testfile3"),
  {ok, _} = auth:login(PID,"Daniel","blabla"),
  {verify_failed} = auth:character(PID,"asdfasdf"),
  auth:stop(PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc try to pause the auth server and resume it
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_pause_auth_test() ->
  {ok, PID}   = auth:start("../auth_testfile3"),
  {ok, Token} = auth:login(PID,"Daniel","blabla"),
  {paused}    = auth:pause(PID,Token),
  {State}     = auth:state(PID,Token),
  {resumed}   = auth:resume(PID,Token),
  auth:stop(PID),
  State#authserverstate.paused =:= paused.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc help test
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

help_test() ->
  auth:help(),
  auth:help(login),
  auth:help(create_user),
  auth:help(verify),
  auth:help(character),
  auth:help(level),
  auth:help(logout),
  auth:help(verify_admin).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           Copyright 2018 by Daniel Weber. Full rights reserved.           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
