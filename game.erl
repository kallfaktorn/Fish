-module(game).
-import(lists).
-import(random).
-export([new_game/0,new_player/1,turn/2,create_game_data/0,test/0]).

test() ->
    new_game(),
	new_player(p1),
	new_player(p2),
	new_player(p3),
	turn(p1,pirch),
	turn(p2,pike),
	turn(p3,pirch).

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


new_game() -> case whereis(game_pid) of
                  undefined -> NrOfPlayers = 2,
				               NrOfTurns   = 2, %Unused. For now...
				               Game = init_game(NrOfPlayers,NrOfTurns), 
				               GamePid = spawn( fun() ->
				                                    process_flag(trap_exit, true),
												    loop(Game)
										            end ),
				               register(game_pid, GamePid);
                  GamePid  -> io:format("The game is already started with Pid: ~w ~n", [GamePid])
			  end.

player() ->
	player(). 


new_player(Name) ->
    game_pid ! {join, self(), Name},
	receive
	{'Full'} -> io:format("No more room.");
	{'Ok', _} -> io:format("Welcome: ~w.~n" ++ 
		                     "Enter game:turn(~w, Parameter). to make your turn.~n" ++
							 "Parameter is either pirch or pike.~n~n"
							 , [Name,Name])
    end.


loop(Game) ->
    receive
	{join,Pid,Name}    -> case space_left(Game) of
	                          true  -> PlayerPid = spawn_link( fun() -> player() end ),
						               register(Name,PlayerPid),
						      	       Value = 0,
							           ModifiedGame = add_player(Game, PlayerPid, Value),
						               Pid ! {'Ok', PlayerPid},
								       loop(ModifiedGame);
						      false -> Pid ! {'Full'},
							           loop(Game)
							  end;
    {play,Pid,Hunting} -> GameSt1 = play(Game,Pid,Hunting),      %Confirm Player move.
                          case GameSt1 of
    					  nix -> loop(Game);
						  _   -> GameSt2 = tryEvaluate(GameSt1), %Evaluate if all players have moved.
						         loop(GameSt2)
    					  end;
	{print, _}           -> printPlayers(Game),
	                      loop(Game)
	end.


turn(Name, Hunting) ->
    case whereis(Name) of
	    undefined -> io:format("Invalid name.");
		Pid       -> game_pid ! {play, Pid, Hunting}
		end.


create_game_data() ->
    #game_data{nr_of_turns=[1,2,3]}.


init_game(NrOfPlayers,NrOfTurns) ->
    %Game = [],
    GameData = #game_data{nr_of_players = NrOfPlayers,
	                            nr_of_turns   = NrOfTurns},   
	GameState = init_game_state(NrOfTurns),
	PlayersState = #players_state{before_turn = [],
	                              after_turn  = []}, 
	LakeState = create_lake_state(),
	Game = #game{game_data     = GameData,
	             game_state    = GameState,
                 players_state = PlayersState,
                 lake_state    = LakeState},
    Game.

	%[GameData|[GameState|[PlayersState|[LakeState|Game]]]].

%Unfinished.
%Parameter should be NrOfTurns.
init_game_state(NrOfTurns) ->
    #game_state{turn = NrOfTurns}.
    %Turn =                         {turn, 0},
	%{gameState, Turn}.

create_lake_state() -> #lake_state{lake = pick_random(lakes())}.
%{lake_state, pick_random(lakes())}.

lakes() -> [[pirch,pike],[pirch,pirch],[pirch],[pike]].



space_left(Game) -> 
    {game_data, Nop, _}            = Game#game.game_data,
	{players_state, Performing, _} = Game#game.players_state,
    %{{game_data, Nop, _}, GameRest1}          = remove(game_data, Game),
	%{{players_state, Performing, _}, _}     = remove(players_state, GameRest1),
	length(Performing) < Nop.
    

add_player(Game, PlayerPid, Value) ->
    {players_state, BeforeTurn, AfterTurn}  = Game#game.players_state,
    %{{players_state, Performing, Waiting}, GameRest} = remove(players_state, Game),
	GameWithAddedPlayer = Game#game{players_state = {players_state, [{PlayerPid, Value}|BeforeTurn], AfterTurn}},
    GameWithAddedPlayer.
	%[{players_state, [{PlayerPid, Value}|Performing], Waiting}|GameRest].



play(Game, Pid, Hunting) ->
    io:format("Test"),
    %{{players_state, Performing, Waiting}, GameRest} = remove(players_state, Game),
	{players_state, Performing, Waiting} = Game#game.players_state,
	case remove(Pid, Performing) of
	{{Pid, Value}, PerformingRest}  ->
	    GameChanged = Game#game{players_state = {players_state, PerformingRest, [{Pid, Hunting, Value}|Waiting]}},
		GameChanged;
	    %[{players_state, PerformingRest, [{Pid, Hunting, Value}|Waiting]}|GameRest];
	nix -> io:format("Error: Not yet players turn or player does not exist.~n"),
	       nix
	end.

tryEvaluate(Game) ->
    %{{players_state, Performing, _}, _} = remove(players_state, Game),
	{players_state, Performing, _} = Game#game.players_state,
	case Performing of 
	    [] -> GameRest = evaluate(Game),
		      printPlayers(GameRest),
			  GameRest;
        _  -> Game
	end.

evaluate(Game) ->
  %{{lake_state, Fishes}, GameRest1}         = remove(lake_state, Game),
  {lake_state, Fishes} = Game#game.lake_state,
  %{{players_state, _, Players},GameRest2} = remove(players_state, GameRest1),
  {players_state, _, Players} = Game#game.players_state,
  %evaluate(GameRest2, Players, [], Fishes).
  evaluate(Game, Players, [], Fishes).



    
evaluate(Game, [], EvalPlayers, _) ->
  GameLakeUpdate        = Game#game{lake_state = create_lake_state()},
  GameLakePlayersUpdate = GameLakeUpdate#game{players_state = {players_state, EvalPlayers, []}},
  GameLakePlayersUpdate;
  %[create_lake_state()|[{players_state, EvalPlayers, []}|Game]];
evaluate(Game, Players, EvalPlayers, Fishes) ->
  {Pid, Hunting, Value} = pick_random(Players),
  {_, PlayersRest} = remove(Pid,Players),
  case removeFromList(Hunting, Fishes) of
  {Fish, FishesRest} -> evaluate(Game,PlayersRest,[{Pid,valueOf(Fish)+Value}|EvalPlayers],FishesRest);
  _                  -> evaluate(Game,PlayersRest,[{Pid,Value}|EvalPlayers],Fishes)
  end.

printPlayers(Game) ->
    %{{players_state, Performing,_}, _} = remove(players_state, Game),
	{players_state, Performing, _} = Game#game.players_state,
	printPlayersStat(Performing).

printPlayersStat([]) -> [];
printPlayersStat([{Pid,Value}|Players]) ->
    io:format("Player: ~w Points: ~w. ~n",[Pid,Value]),
	printPlayersStat(Players).

valueOf(Fish) ->
 case Fish of
 pirch -> 1;
 pike  -> 2
 end.

%test() ->
%  new_game(),
%  new_game(),
%  new_player('mat'),
%  new_player(mat2),
%  new_player(mat3).


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
