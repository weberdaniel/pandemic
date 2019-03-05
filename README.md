# pandemic

CONTENT

This is a tech demonstration project of Erlang. Erlang is known for its concurrency and
fault-tolerance and redudancy mechanisms. Also it is known for distributed computing. It
does not excel at performance but is highly used in the field of telecommunication. 
Supervisor processes provide automatic recovery of crashed processes.

The game is an Erlang Application called "pandemic". It consists of a supervisor process
called 'pandemic_sup'. An XML File config/game_config.xml is read which then sets up the whole
strucutre of the game. Each town in the game is represented by one Erlang process, also
each player is represented by a separate Erlang process. Futhermore there is an 
authentification server to provide logins/logouts for users and a world server, that holds
all coordinates of all processes living in the world.

One can interact with the world by standard Erlang interfaces to the processes, to travel
between towns, heal the population, login, logout, expand facilities like hospitals in the
town, or do politics closing the borders of a town. All towns have connections to other
towns. The population is being infected and travels from time to time to other towns.

INSTALLATION:

1. Install Erlang
2. goto pandemic folder
3. type "erl -make"
4. use ./start.sh to start application
5. Follow instructions on the screen

DESIGN DOCUMENT:

See https://github.com/ajjiket/pandemic/blob/master/doc/softwaredesign/softwaredesign.pdf
