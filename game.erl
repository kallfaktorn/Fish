%Header

-module(game).
-import(lists).
-import(random).
-export([example_new_game/0,new_player/1,turn/2,test/0]).


%Test

test() ->
    example_new_game(),
	new_player(p1),
	new_player(p2),
	new_player(p3),
	turn(p1,pirch),
	turn(p2,pike),
	turn(p3,pirch).


%Structure

-record(game,
    {game_data,
	game_state,
	players_state,
	lake_state}).

-record(game_data,
    {nr_of_players=2,
	nr_of_turns}).

-record(game_state,
    {turn=0}).

-record(players_state,
    {before_turn,
	after_turn}).

-record(lake_state,
    {lake}).


%Inititialize

create_game(NrOfPlayers, NrOfTurns) ->
    GameData     = create_game_data(NrOfPlayers, NrOfTurns),
	GameState    = create_game_state(NrOfTurns),
	PlayersState = create_empty_players_state(),
	LakeState    = create_lake_state(),
	Game = #game{game_data     = GameData,
	             game_state    = GameState,
                 players_state = PlayersState,
                 lake_state    = LakeState},
    Game.


create_game_data(NrOfPlayers, NrOfTurns) ->
    #game_data{nr_of_players = NrOfPlayers,
	           nr_of_turns   = NrOfTurns}.

create_game_state(NrOfTurns) ->
    #game_state{turn = NrOfTurns}.

create_empty_players_state() ->
	#players_state{before_turn = [],
	               after_turn  = []}.

create_lake_state() -> #lake_state{lake = pick_random(lakes())}.


create_new_player(Game, Name) ->
    create_player(Game, Name, 0).

create_player(Game, Name, Value) ->
    PlayerPid      = spawn_player(Name),
    GameWithPlayer = add_player(Game, PlayerPid, Value),
    GameWithPlayer.

spawn_player(Name) ->
    PlayerPid = spawn_link(fun() -> player() end ),
    register(Name,PlayerPid),
    PlayerPid.

add_player(Game, PlayerPid, Value) ->
    {players_state, BeforeTurn, AfterTurn}  = Game#game.players_state,
	GameWithAddedPlayer = Game#game{players_state = {players_state, 
                                                     [{PlayerPid, Value}|BeforeTurn],
                                                     AfterTurn}},
    GameWithAddedPlayer.


%Startup

new_game(NrOfPlayers, NrOfTurns) -> 
    case whereis(game_pid) of
        undefined ->
            Game = create_game(NrOfPlayers,NrOfTurns), 
            GamePid = spawn( fun() ->
                process_flag(trap_exit, true),
                loop(Game) end ),
                register(game_pid, GamePid);
        GamePid  -> io:format("The game is already started with Pid: ~w ~n", [GamePid])
    end.


example_new_game() ->
    new_game(2,2).


new_player(Name) ->
    game_pid ! {join, self(), Name},
    receive
        {'Full'} -> 
            io:format("No more room.~n");
        {'Ok'} -> 
            io:format("Welcome: ~w.~n" ++ 
                      "Enter game:turn(~w, Parameter). to make your turn.~n" ++
                      "Parameter is either pirch or pike.~n~n"
                     , [Name,Name])
    end.


%Loop

loop(Game) ->
    receive
        {join,Pid,Name} ->
            case space_left(Game) of
                true  -> 
                    ModifiedGame = create_new_player(Game, Name),
                    Pid ! {'Ok'},
                    loop(ModifiedGame);
                false -> 
                    Pid ! {'Full'},
                    loop(Game)
            end;
        {play,Pid,Hunting} -> 
            GameSt1 = play(Game,Pid,Hunting),
            case GameSt1 of
                nix -> 
                    loop(Game);
                _   -> 
                    GameSt2 = update(GameSt1), 
                    loop(GameSt2)
            end;
        {print, _} -> 
            printPlayers(Game),
            loop(Game)
	end.


player() ->
	player(). 
    

%Gameplay

turn(Name, Hunting) ->
    case whereis(Name) of
	    undefined -> io:format("Invalid name.~n");
		Pid       -> game_pid ! {play, Pid, Hunting}
    end.


space_left(Game) -> 
    {game_data, Nop, _}            = Game#game.game_data,
	{players_state, Performing, _} = Game#game.players_state,
	length(Performing) < Nop.
    

%Game logic

play(Game, Pid, Hunting) ->
	{players_state, Performing, Waiting} = Game#game.players_state,
	case remove(Pid, Performing) of
	    {{Pid, Value}, PerformingRest}  ->
            GameChanged = Game#game{players_state = {players_state, 
                                                     PerformingRest, 
                                                     [{Pid, Hunting, Value}|Waiting]}},
            GameChanged;
        nix -> 
            io:format("Error: Not yet players turn or player does not exist.~n"),
	        nix
	end.

update(Game) ->
	{players_state, Performing, _} = Game#game.players_state,
	case Performing of 
	    [] -> GameNewTurn = evaluate(Game),
		      printPlayers(GameNewTurn),
			  GameNewTurn;
        _  -> Game
	end.

evaluate(Game) ->
    {lake_state, Fishes} = Game#game.lake_state,
    {players_state, _, Players} = Game#game.players_state,
    evaluate(Game, Players, [], Fishes).

evaluate(Game, [], EvalPlayers, _) ->
  GameLakeUpdate        = Game#game{lake_state = create_lake_state()},
  GameLakePlayersUpdate = GameLakeUpdate#game{players_state = {players_state, EvalPlayers, []}},
  GameLakePlayersUpdate;

evaluate(Game, Players, EvalPlayers, Fishes) ->
    {Pid, Hunting, Value} = pick_random(Players),
    {_, PlayersRest}      = remove(Pid,Players),
    case removeFromList(Hunting, Fishes) of
        {Fish, FishesRest} -> 
            evaluate(Game,
            PlayersRest,
            [{Pid,valueOf(Fish)+Value}|EvalPlayers],
            FishesRest);
        _ -> 
            evaluate(Game,PlayersRest,[{Pid,Value}|EvalPlayers],Fishes)
    end.


%Output

printPlayers(Game) ->
	{players_state, Performing, _} = Game#game.players_state,
	printPlayersStat(Performing).

printPlayersStat([]) -> [];
printPlayersStat([{Pid,Value}|Players]) ->
    io:format("Player: ~w Points: ~w. ~n",[Pid,Value]),
	printPlayersStat(Players).


%Game properties
valueOf(Fish) ->
    case Fish of
        pirch -> 1;
        pike  -> 2
    end.


lakes() -> [[pirch,pike],[pirch,pirch],[pirch],[pike]].

%Personal library


%Given an element e and a list l.
%If e is in list, return a tuple consisting of the element e and
%the list without element e: {e,restOfList}
%If e is not in list, return nix.
removeFromList(Elem, List) -> removeFromList(Elem, List, []).

removeFromList(_, [], _) -> nix;
removeFromList(Elem, [X|XS], YS)
    when Elem == X -> {X, XS ++YS};
removeFromList(Elem, [X|XS], YS) ->
    removeFromList(Elem,XS,[X|YS]).


%Given a Variable v and a structure [{Var, _, ...}|List],
%Returns a tuple: {{Var, _, ...}, List} if v == Var. List is the rest of the list.
%Returns nix if no proper matching tuple was found.
remove(Atom,List) -> remove(Atom,List,[]).

remove(_,[],_) -> nix;
remove(Atom,[Tpl|List], Tmp) ->
    TplAtom = element(1,Tpl),
	case TplAtom == Atom of
        true  -> {Tpl, Tmp ++ List};
        false -> remove(Atom,List,[Tpl|Tmp])
	end.

%Given a list, return a random element from the list.
pick_random(List) ->
    lists:nth(random:uniform(length(List)),List).
