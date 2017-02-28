:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(random)).
:- use_module(library(system)).

/* PART 1 - QUESTION 3 */ 
test_strategy(N, StrategyP1, StrategyP2) :-
  stats_time_record(N, StrategyP1, StrategyP2, MovesList, WinnerList, GameTimeList),
  numWins(draw, WinnerList, Draws),
  numWins(stalemate,WinnerList, Stalemate),
  numWins(exhaust, WinnerList, Exhaustives),
  TotalDraws is Draws + Stalemate + Exhaustives, 
  numWins(b, WinnerList, P1NumWins),
  numWins(r, WinnerList, P2NumWins),
  longest(MovesList, LongestGame),
  numWins(exhaust,MovesList,Exhaust),
  LongestGameNE is LongestGame - Exhaust, 
  shortest(MovesList, ShortestGame),
  avg(MovesList, AverageGameLength),
  avg(GameTimeList, AverageGameTime),
  write('-------------------------------'), write('\n'),
  write(StrategyP1), write(' vs '), write(StrategyP2),write('\n'),
  write('Number of wins for player 1 (blue): '), write(P1NumWins), write('\n'),
  write('Number of wins for player 2 (red): '), write(P2NumWins), write('\n'),
  write('Number of draws: '), write(TotalDraws), write('\n'), 
  write('Longest (non-exhaustive) game: '), write(LongestGameNE), write('\n'),
  write('Shortest Game: '), write(ShortestGame), write(' \n'),
  write('Average Game Length (including exhaustives): '), write(AverageGameLength), write(' \n'),
  write('Average Game Time: '), write(AverageGameTime), write(' seconds \n'),
  write('-------------------------------'), write('\n').


% Recursive function to calculate the game time for each round. 
stats_time_record(0, _, _, [], [], []).
stats_time_record(N, StrategyP1, StrategyP2, [MoveNum | MovesList], [WinningPlayer | WinnerList], [GameTime | GameTimeList]) :-
  N > 0,
  % Calculate the game time using predicate now, which records the time stamp at the begging and end of play. 
  now(StartTime),
  play(quiet, StrategyP1, StrategyP2, MoveNum, WinningPlayer),
  now(EndTime),
  GameTime is EndTime - StartTime,
  RemainingNum is N - 1,
  stats_time_record(RemainingNum, StrategyP1, StrategyP2, MovesList, WinnerList, GameTimeList).


%recursively check how many times the given player appears in the list of winners
numWins(_, [], 0) :- 
!.  
numWins(Player, [Player|List], PNumWins) :- 
    numWins(Player, List, N), 
    PNumWins is N + 1.    

numWins(Player, [H|List], PNumWins) :- 
    Player \= H,          
    numWins(Player, List, PNumWins).  

%finds the minimum element in the given list
shortest([X],X).
shortest([B,R|L], Min) :-
  B < R -> shortest([B|L], Min);
  shortest([R|L], Min).

%finds the maximum element in the given list
longest([X],X).
longest([B,R|L], Max) :-
  B > R -> longest([B|L], Max);
  longest([R|L], Max).

%sum of all the elements in the list
sum_list([], 0).
sum_list([H|Tail], Result ) :-
    sum_list(Tail, Sum),
    Result is H + Sum.

%Calculates the mean
avg(List, Avg) :-
  length(List, TotalValues),
  sum_list(List, Sum),
  Avg is Sum / TotalValues.

/* PART 2 - IMPLEMENTING STRATEGIES */

/* Get the possible moves given the player and the board state */
empty_board(X, Y, [Blue, Red]) :-
    \+ member([X, Y], Blue),
    \+ member([X, Y], Red).

get_possible_moves(Player, Board, PossMoves) :-
    findall(
        [X, Y, NX, NY],
        (
            cell(X, Y),
            what_in_cell(Board, X, Y, Player),
            neighbour_position(X, Y, [NX, NY]),
            empty_board(NX, NY, Board)
        ),
        PossMoves
    ).

/* Run a move and get cranked board, regardless of Strategy */
run_move(r, [Blue, Red], Move, [Blue, NewRed], CrankedNewBoard) :-
  alter_board(Move, Red, NewRed),
  next_generation([Blue, NewRed], CrankedNewBoard).
  
run_move(b, [Blue, Red], Move, [NewBlue, Red], CrankedNewBoard) :-
  alter_board(Move, Blue, NewBlue),
  next_generation([NewBlue, Red], CrankedNewBoard).


/* Get relevent pieces for corresponding strategies and board states */
get_relevant_pieces(Player, bloodlust, [B, R], Opponent) :-
    (Player == b) -> (Opponent = R) ; (Opponent = B).

get_relevant_pieces(Player, self_preservation, [B,R], CurrPlayer) :- 
    (Player == b) -> (CurrPlayer = B) ; (CurrPlayer = R). 

get_relevant_pieces(CurrPlayer, land_grab, [B,R], [Player,Opponent]) :- 
    (CurrPlayer == b) -> (Player = B, Opponent = R) ; (Player = R, Opponent = B). 


/* Get relevent measures for corresponding strategies */
measure(bloodlust, Opponent, NumPieces) :- 
    length(Opponent, NumPieces).

measure(self_preservation, Player, NumPieces) :-
    length(Player, NumPieces).  

measure(land_grab, [Player, Opponent], Difference) :-
    length(Player, NumPlayerPieces),
    length(Opponent, NumOpponentPieces),
    Difference is NumPlayerPieces - NumOpponentPieces.   


/* Main helper functions */
get_move_options(Player, BoardState, Strategy, MovesAndValues) :-
    get_possible_moves(Player, BoardState, PossMoves),
    findall(
        [Value, Move, BeforeCrankedBoardState, NewCrankedBoardState],
        (
            member(Move, PossMoves),
            run_move(Player, BoardState, Move, BeforeCrankedBoardState, NewCrankedBoardState), 
            get_relevant_pieces(Player, Strategy, NewCrankedBoardState, ReleventState),
            measure(Strategy, ReleventState, Value)
        ),
        MovesAndValues
    ).

best_member(Type, Ans, [M|MovesAndValues]) :-
  best_member(Type, Ans, MovesAndValues, M).

best_member(_Type, Ans, [], Ans).
best_member(Type, Ans, [[P1,X,Y,A]|Rest], [P2,W,Z,B]) :-
  (Type == min -> (P1 < P2 -> NewState = [P1,X,Y,A] ; NewState = [P2,W,Z,B]) ; 
                  (P1 > P2 -> NewState = [P1,X,Y,A] ; NewState = [P2,W,Z,B])) ,
  best_member(Type, Ans, Rest, NewState).

/* End of main */



%%%%%%%%%%%%%%%
%  BLOODLUST  %
%%%%%%%%%%%%%%%

bloodlust(PlayerColor, CurrentBoardState, NewBoardState, Move) :-   
  get_move_options(PlayerColor, CurrentBoardState, bloodlust, MovesAndValues),
  best_member(min, [_,Move,NewBoardState,_], MovesAndValues). 


%%%%%%%%%%%%%%%%%%%%%%%
%  SELF_PRESERVATION  %
%%%%%%%%%%%%%%%%%%%%%%%

self_preservation(PlayerColor, CurrentBoardState, NewBoardState, Move) :- 
  get_move_options(PlayerColor, CurrentBoardState, self_preservation, MovesAndValues),
  best_member(max, [_,Move,NewBoardState,_], MovesAndValues). 


%%%%%%%%%%%%%%%
%  LAND_GRAB  %
%%%%%%%%%%%%%%%

land_grab(PlayerColor, CurrentBoardState, NewBoardState, Move) :-
  get_move_options(PlayerColor, CurrentBoardState, land_grab, MovesAndValues),
  length(MovesAndValues, N), N > 0,
  best_member(max, [_,Move,NewBoardState,_], MovesAndValues). 
land_grab(_, BoardState, BoardState, _).

%%%%%%%%%%%%%
%  MINIMAX  %
%%%%%%%%%%%%%

opposite_color(b, r).
opposite_color(r, b).

opponent_moves(_, [], []).
opponent_moves(Opponent, [[_, PlayerMove, PlayerBeforeCranked, PlayerCranked]|Rest], [R|Results]) :-
  /* Getting opponent worst move for each of our move (maximising in their perspective) */
  land_grab(Opponent, PlayerCranked, AfterMove, _),
  next_generation(AfterMove, CrankedAfterMove),

  /* Getting player landgrab value based on each of opponent worst move */
  opposite_color(Opponent, Player),
  get_relevant_pieces(Player, land_grab, CrankedAfterMove, Result),
  measure(land_grab, Result, PlayerValue),

  /* Put the value into result for recursing later */
  R = [PlayerValue, PlayerMove, PlayerBeforeCranked, CrankedAfterMove],
  opponent_moves(Opponent, Rest, Results).


minimax(PlayerColor, CurrentBoardState, NewBoardState, PlayerChosenMove) :-
  opposite_color(PlayerColor, OpponentColor),
  
  /* 1st look ahead -> getting ALL possible boards after Player's move  */
  get_move_options(PlayerColor, CurrentBoardState, land_grab, PlayerMoveBoardList),
  
  /* 2nd look ahead */
  opponent_moves(OpponentColor, PlayerMoveBoardList, Results),

  /* Now recursing to find the highest value for our move */ 
  best_member(max, [_, PlayerChosenMove, NewBoardState, _], Results).
