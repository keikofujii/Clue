%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PrologClient.pl 
%     A simple Glomus player implemented in Prolog.
%     This player can play a legal, but somewhat
%     unintelligent game. Extension is left as an
%     exercise for the student.
%
%     NOTE: to use this player with the PrologClient.java wrapper we
%     must implement the messages required by that client which
%     generally correspond to the those listed in Protocol.java.
%     However, the wrapper also assumes that the iAm/1 and isSuspect/1,
%     isWeapon/1, isRoom/1 predicates are used. Be careful not to change
%     the names or arity of any method required by the wrapper.
%
% Author:   1668650
% Written:  3/29/05
% Version:  2.0 
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Hunches? 

% Identify which rules we can assert and retract dynamically
:- dynamic adjacent/2.
:- dynamic showedMe/2.
:- dynamic iHave/1.
:- dynamic isIn/2.
:- dynamic iAm/1. 
:- dynamic suspect/1.
:- dynamic weapon/1.
:- dynamic room/1.
:- dynamic activePlayer/1.
:- dynamic refuted/4.
:- dynamic suggested/4.
:- dynamic didntRefute/4.
:- dynamic accused/4.
:- dynamic suspectPointsFor/2.
:- dynamic weaponPointsFor/2.
:- dynamic roomPointsFor/2.
:- dynamic gamesWon/2.
:- dynamic totalGames/1.
:- dynamic deducedHasCard/2. 
:- dynamic isSusAnswer/1.
:- dynamic isWeapAnswer/1.
:- dynamic isRoomAnswer/1.

% A rule to use instead of asserta to prevent reasserting the same fact 
% Deduces hasCard at the end of adding the new fact
addNewFact(F) :- \+ F, asserta(F), deduceHasCard. 
addNewFact(_).

% Rules to deduce if a player has a card
% It is deduced if a player has a card if they refuted a 
% suggestion and it is known that they cannot have two of the 
% three cards in the suggestion that they refuted
% Checks to see if we can deduce that they knew the suspect
deduceHasCard :- 
  refuted(Suspect, Weapon, Room, Player), 
  cantHave(Player, Weapon), 
  cantHave(Player, Room), 
  \+ deducedHasCard(Player, Suspect), % See if the fact already is known
  addNewFact(deducedHasCard(Player, Suspect)), writeLog(['DHS1']),
  isSuspect(_S).

% Checks to see if we can deduce that they knew the weapon
deduceHasCard :- 
  refuted(Suspect, Weapon, Room, Player), 
  cantHave(Player, Suspect), 
  cantHave(Player, Room), 
  \+ deducedHasCard(Player, Weapon), % See if the fact already is known
  addNewFact(deducedHasCard(Player, Weapon)), writeLog(['DHS2']),
  isWeapon(_W).

% Checks to see if we can deduce that they knew the room
deduceHasCard :- 
  refuted(Suspect, Weapon, Room, Player), 
  cantHave(Player, Weapon), 
  cantHave(Player, Suspect), 
  \+ deducedHasCard(Player, Room), % See if the fact already is known
  addNewFact(deducedHasCard(Player, Room)), writeLog(['DHS3']),
  isRoom(_R).

% Base case, just pass
deduceHasCard.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Game start and reset rules
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This matching rule is used to clear all assertions from the database
% when initing the game. Be sure to modify this rule if you add new
% dynamic facts to the list above.
reset :- 
   retractall(adjacent(_,_)),
   retractall(showedMe(_,_)),
   retractall(iHave(_)),
   retractall(isIn(_,_)),
   retractall(iAm(_)),
   retractall(suspect(_)),
   retractall(weapon(_)),
   retractall(room(_)),
   retractall(activePlayer(_)),
   retractall(refuted(_,_,_,_)),
   retractall(suggested(_,_,_,_)),
   retractall(didntRefute(_,_,_,_)),
   retractall(accused(_,_,_,_)),
   retractall(deducedHasCard(_,_)),
   retractall(isSusAnswer(_)),
   retractall(isWeapAnswer(_)),
   retractall(isRoomAnswer(_)).



% init removes all old assertaions from the knowledgebase and asserts who 
% I am, where I am, what cards I hold, as well as all of the other information
% about the game.
init(Me, StartRoom, Suspects, Weapons, RoomLists, Cards, ActivePlayers) :- 
   reset, 
   asserta(iAm(Me)),
   asserta(isIn(Me, StartRoom)),
   addMyCards(Cards),
   addSuspects(Suspects),
   addWeapons(Weapons),
   addRooms(RoomLists),
   addPlayers(ActivePlayers).
  

% Init a game with default configuration; should not use this for a
% server-game since the configuration may change; could use this to play
% interactively for testing and for fun...
initDefault(Me,StartRoom, MyCards) :-
   Suspects = [missscarlet,mrgreen,mrswhite,professorplum,mrspeacock,
   colonelmustard],
   Weapons = [rope,leadpipe,wrench,candlestick,knife,revolver],
   Rooms = [[hall,[study,lounge]],
       [study,[hall,library,kitchen]],
       [library,[billiardroom,study]],
       [billiardroom,[library,conservatory]],
       [conservatory,[billiardroom,lounge,ballroom]],
       [ballroom,[conservatory,kitchen]],
       [kitchen,[diningroom,study,ballroom]],
       [diningroom,[kitchen,lounge]],
       [lounge,[diningroom,hall,conservatory]]],
   % Assume everyone is active
   init(Me, StartRoom, Suspects, Weapons, Rooms, MyCards, Suspects).

% Init a game with default configuration (could use this to play
% interactively)
initDefault(Me, StartRoom, MyCards, Suspects) :-
   Weapons = [rope,leadpipe,wrench,candlestick,knife,revolver],
   Rooms = [[hall,[study,lounge]],
       [study,[hall,library,kitchen]],
       [library,[billiardroom,study]],
       [billiardroom,[library,conservatory]],
       [conservatory,[billiardroom,lounge,ballroom]],
       [ballroom,[conservatory,kitchen]],
       [kitchen,[diningroom,study,ballroom]],
       [diningroom,[kitchen,lounge]],
       [lounge,[diningroom,hall,conservatory]]],
   % Assume everyone is active
   init(Me, StartRoom, Suspects, Weapons, Rooms, MyCards, Suspects).


% Adds cards to the given set via assertion
addSuspects([]).
addSuspects([Card|T]) :- asserta(suspect(Card)), addSuspects(T).
addWeapons([]).
addWeapons([Card|T]) :- asserta(weapon(Card)), addWeapons(T).
addMyCards([]).
addMyCards([Card|T]) :- asserta(iHave(Card)), addMyCards(T).
addPlayers([]).
addPlayers([Card|T]) :- asserta(activePlayer(Card)), addPlayers(T).

% Add rooms and set up the adjacency
addRooms([]).
addRooms([[Room|[RoomList]]|T]) :- 
   asserta(room(Room)), 
   addAdjacent(Room, RoomList), addRooms(T).

% Add adjacency rule
addAdjacent(_, []).
addAdjacent(Room, [Adjacent|T]) :- 
   asserta(adjacent(Room, Adjacent)),
   addAdjacent(Room, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Knowledgebase Update Rules 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remember cards shown to me.
showsMe(Player, Card) :- addNewFact(showedMe(Player, Card)).


% Suggestions from other players- moves suspects to new rooms
% and player must be in the room as well 
suggests(Suspect, Weapon, Room, Player) :- 
  moveTo(Player,Room),
  moveTo(Suspect,Room),
  addNewFact(suggested(Suspect, Weapon, Room, Player)).


% Refutations I have overheard - might want to
% remember...
refutes(Suspect, Weapon, Room, Player) :- 
  addNewFact(refuted(Suspect, Weapon, Room, Player)).

% Failed refutations I have overheard
cantRefute(Suspect, Weapon, Room, Player) :-
  addNewFact(didntRefute(Suspect, Weapon, Room, Player)).

% Failed accusations I have overheard
accuse(Suspect, Weapon, Room, Player) :- 
  addNewFact(accused(Suspect, Weapon, Room, Player)).

% Predicate to move people to rooms - need to remember where we are!
moveTo(Suspect, Room) :- 
   retractall(isIn(Suspect, _OldRoom)), 
   addNewFact(isIn(Suspect, Room)). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Handle requests of my player
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% End of the game, server tells me what each player knew 
knew(Suspect, Weapon, Room, Player) :- updateSuspectScore(Suspect, Player),
                                       updateWeaponScore(Weapon, Player), 
                                       updateRoomScore(Room, Player).



% What the answer was and who won. Keep track of my wins and loses and
% total points and print out point totals after each game. 
answer(Suspect, Weapon, Room, WinningPlayer) :- 
   writeLog([WinningPlayer,'won, answer:',Suspect,Weapon,Room]),
   (gamesWon(WinningPlayer, Won), retract(gamesWon(WinningPlayer,_)); 
    Won is 0), 
   NewScore is Won+1, asserta(gamesWon(WinningPlayer, NewScore)),
   (totalGames(Games),retract(totalGames(_)); Games is 0), 
   NewNumber is Games+1, asserta(totalGames(NewNumber)), 
   iAm(Me), writeLog(['\n\nI am',Me]),
   forall(activePlayer(P), printPoints(P)), nl,nl,nl.


% Refute a suggestion with a suspect, weapon, or room. 
% If something has been refuted already, then refute it again
% Otherwise pick a card that we can refute
refute(Suspect, _, _, _, Suspect) :- 
    refuted(Suspect, _, _, _), iHave(Suspect), writeLog(['R1']).
refute(_, Weapon, _, _, Weapon) :- 
    refuted(_, Weapon, _, _), iHave(Weapon), writeLog(['R2']).
refute(_, _, Room, _, Room) :- 
    refuted(_, _, Room, _), iHave(Room), writeLog(['R3']).
refute(Suspect, _W, _R, _A, Suspect) :- iHave(Suspect), writeLog(['R4']).
refute(_S, Weapon, _R, _A, Weapon) :- iHave(Weapon), writeLog(['R5']).
refute(_S, _W, Room, _A, Room) :- iHave(Room), writeLog(['R6']).


% Make accusation succeeds if I know the three items
makeAccusation(Suspect, Weapon, Room, Me) :- 
  iAm(Me), 
  isSuspect(Suspect), 
  isWeapon(Weapon),
  isRoom(Room), writeLog(['MA1']).


% Make a suggestion - moves me and the suspect to the new room
makeSuggestion(Suspect, Weapon, Room, Me) :- 
  pickSuspect(Suspect),
  pickWeapon(Weapon),
  pickRoom(Room), 
  iAm(Me),
  moveTo(Suspect, Room),
  moveTo(Me, Room), writeLog(['MS1']).

% Get a random element from a list 
randomElement(List, RandomElement) :-
     length(List, Len), Len > 0,
     Index is random(Len), writeLog(['RandomElementList', List]),
     nth0(Index, List, RandomElement), writeLog(['RandomElement', RandomElement]).

%%%%%%%%%%%%%%%%%%%%%%%%%
% Suspect selection rules
%%%%%%%%%%%%%%%%%%%%%%%%%
% If I know who it is, use that suspect
pickSuspect(S) :- isSuspect(S), writeLog(['PS1']).

pickSuspect(S) :- 
  writeLog(['trying PS2']), setof(possibleSuspects(Poss), (member(Sus, Poss), numCantHave(Sus, NumSuspects)), NumCantHaveSus),  
  last(NumCantHaveSus, MaxNum), writeLog(['setof2']),
  setof(Suspect, (suspect(Suspect), numCantHave(Suspect, MaxNum)), MaxCantHaveSus), 
  randomElement(MaxCantHaveSus, S), writeLog(['PS2', S]).

% Pick a random suspect from the possible suspects
pickSuspect(S) :- possibleSuspects(SusList), 
    randomElement(SusList, S), writeLog(['PS3']).

% Pick a random suspect from any of the suspects
pickSuspect(S) :- setof(Sus, suspect(Sus), Ss),
     randomElement(Ss, S), writeLog(['PS99']).


% Can pick based on probability?

%%%%%%%%%%%%%%%%%%%%%%%%%
% Weapon selection rules
%%%%%%%%%%%%%%%%%%%%%%%%%
% If I know what they used, use that weapon 
pickWeapon(W) :- isWeapon(W), writeLog(['PW1']).

%*****
% Pick the weapon with the highest probability of being in the answer
% The weapon that the highest amount of people can't have it
% Get the number of people that can't have the weapon
% Get a list of all the weapons and the number of people that can't have them
% Get number of people that can't have a weapon (sorted with setof)
% Use numCantHave and send in a number to bind to a weapon
%pickWeapon(W) :- setof(NumWeapons, weapon(W), %No one has it, numCantHave(Player, Weapon), )

%Hansen's
%find # of people who can't have a card
%get setof numbers of people who can't have
% max num is the last 
%get the last
% do the same thing as the first query but send in the max(last)
pickWeapon(W) :- 
  writeLog(['trying PW2']), setof(NumWeapons, (possibleWeapons(Poss), member(Weap, Poss), numCantHave(Weap, NumWeapons)), NumCantHaveWeap),  
  last(NumCantHaveWeap, MaxNum), writeLog(['setof2']),
  setof(Weapon, (weapon(Weapon), numCantHave(Weapon, MaxNum)), MaxCantHaveWeap), 
  randomElement(MaxCantHaveWeap, W), writeLog(['PW2', W]).

% Pick a random weapon from the possible weapons
pickWeapon(W) :- possibleWeapons(WeapList), 
    randomElement(WeapList, W), writeLog(['PW3']).

% Pick a random weapon from any of the weapons
pickWeapon(W) :- setof(Weap, weapon(Weap), Ws),
    randomElement(Ws, W), writeLog(['PW99']).


%%%%%%%%%%%%%%%%%%%%%%%%%
% Room selection rules
%%%%%%%%%%%%%%%%%%%%%%%%%
% Pick a room next to the room I'm in that is still possible
pickRoom(NewRoom) :- 
    iAm(Me), isIn(Me, Room), adjacent(Room, NewRoom),
    possibleRooms(P), member(NewRoom, P), writeLog(['PickRoom1']).

% If none of the rooms around me are possible then choose a room that is closest 
% to another possible room
% Get a list of all possible rooms and their distances 
% Pick a random? one from the list 
% Find path to any of the unknown nodes 
% Not sure if this works well or not
pickRoom(NewRoom) :- writeLog(['Trying PR2']), iAm(Me), isIn(Me, MyRoom), 
    possibleRooms(Good), setof(D, G^(member(G, Good), 
    distance(MyRoom, G, D)), [Dist | _]),
    setof(R, distance(MyRoom, R, Dist), [ DestRoom | _]), 
    nextRoom(MyRoom, DestRoom, NewRoom), writeLog(['Pick Room', NewRoom]).

% Hansen's player loves corners  because there are more options
% Find the room that has the most still possible
% find all the rooms adjacent to the one that you're looking at 

% If rooms are equal probability, then Hansen knows the turn order so he chooses the one farthest away

% You can get each players' number of cards by dividing, then use modulo and the remainder 
% if remainder is 4 then first four have extra cards

% don't give away extra info at the end of the turn 
% Check to make sure that you know the answer, then suggest trash and then make the accusation

% Pick a room next to the room I'm in
pickRoom(NewRoom) :- iAm(Me), isIn(Me, Room), adjacent(Room, NewRoom), 
    writeLog(['PickRoom3']).

% Get the number of players that can't have a card
numCantHave(Card, Num) :-
    writeLog(['trying NCH']),
    findall(Player, (suspect(Player), 
    cantHave(Player, Card)), WhoCantHaveCard), 
    list_to_set(WhoCantHaveCard, SetWhoCantHaveCard),
    length(SetWhoCantHaveCard, Num), 
    writeLog(['numCantHave', Card, ' ', WhoCantHaveCard]).

%% % The actual relation we will use is adjacent() which is defined by noting
%% % that the nextTo() relation is really bi-directional
%% adjacent(X,Y) :- nextTo(X,Y).
%% adjacent(X,Y) :- nextTo(Y,X).


%%%%%%%%%%%%%%%%
% nextRoom() uses breadth-first search below to find a path from X to Y 
% and returns the next room we should move to in order to move from X to Y.
% The nextToLastItem() relation helps pick off the next room to visit from 
% the path list.  The distance() relation returns the distance between rooms
%%%%%%%%%%%%%%%

% Find a path from X to Y as a list [Y..X] and our next room is 
% the next-to-last item in that path list - note that this works
% correctly for immediately adjacent rooms as well since the list will 
% have two elements
nextRoom(Current, Destination, Next) :-
        breadthFirst([[Current] | Temp] - Temp, Path, Destination),
        nextToLastItem(Path, Next).

% Distance is length of the path - 1
distance(Current, Destination, Length) :-
        breadthFirst([[Current] | Temp] - Temp, Path, Destination),
        length(Path, L), Length is L-1, writeLog(['Distance']).

% Given a list, pick off the next-to-last item
nextToLastItem([Item | [_|[]]], Item).
nextToLastItem([_ | Tail], Item) :- nextToLastItem(Tail, Item).



%%%%%%%%%%%%%%%%%%%
% Breadth-first search
%
% General breadth-first search returns shortest path between two nodes
% where distances are uniform.  
%%%%%%%%%%%%%%%%%%%%

% If the head of the list for the path is the goal node, then we have a path and
% are done!
breadthFirst([[Node | Path]|_] - _, [Node | Path], Goal) :- Goal = Node.

% Otherwise we need to extend the path with adjacent nodes
breadthFirst([Path | Paths] - Z, Solution, Goal) :-
        extend(Path, NewPaths),
        append(NewPaths, Z1, Z),
        Paths \== Z1,
        breadthFirst(Paths - Z1, Solution, Goal).
        
% Paths are extended with nodes that are adjacent
extend([Node | Path], NewPaths) :- 
        bagof([NewNode, Node | Path],
              (adjacent(Node, NewNode), 
                 \+ member(NewNode, [Node | Path])), % no cycles
              NewPaths), !.             % if we got this far, don't try others
% Otherwise, we are at a dead end - no way to extend the path so we succeed but
% return an emtpy list of new paths
extend(_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Helper rules for determining what I know
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Helpful to have a list of all of the weapons
allWeapons(AllWeapons) :- setof(W, weapon(W), AllWeapons). 
% A list of weapons that I know an active player has in their hand
knownWeapons(KnownWeapons) :- 
   findall(W, (weapon(W), activePlayer(A), hasCard(A, W)), KW),
   list_to_set(KW, KnownWeapons).
% A list of all weapons - known weapons
possibleWeapons(Possible) :- 
   allWeapons(All), 
   knownWeapons(Known),
   subtract(All, Known, Possible).  

% Ditto for suspects and rooms
allSuspects(AllSuspects) :- setof(S, suspect(S), AllSuspects). 
knownSuspects(KnownSuspects) :- 
   findall(S, (suspect(S), activePlayer(A), hasCard(A, S)), KS),
   list_to_set(KS, KnownSuspects).
possibleSuspects(Possible) :- 
   allSuspects(All), 
   knownSuspects(Known),
   subtract(All, Known, Possible).

allRooms(AllRooms) :- setof(R, room(R), AllRooms). 
knownRooms(KnownRooms) :- 
   findall(R, (room(R), activePlayer(A), hasCard(A, R)), KR),
   list_to_set(KR, KnownRooms).
possibleRooms(Possible) :- 
   allRooms(All), 
   knownRooms(Known),
   subtract(All, Known, Possible).


% One way to prove that I know a part of the solution is if there is 
% only one card left in the set of possible objects 
isSuspect(S) :- possibleSuspects([S]), writeLog(['isSus1', S]),
    addNewFact(isSusAnswer(S)).

% if we know that the number of players that can't have a suspect is the number
% of suspects, then we know that the suspect must be the answer
isSuspect(S) :- suspect(S), numCantHave(S, Num), allSuspects(AllS), 
    length(AllS, Num), writeLog(['isSus2', S]), addNewFact(isSusAnswer(S)).

% One way to prove that I know a part of the solution is if there is 
% only one card left in the set of possible objects 
isWeapon(W) :- possibleWeapons([W]), writeLog(['isWeap1', W]),
    addNewFact(isWeapAnswer(W)).

% if we know that the number of players that can't have a weapon is the number
% of weapons, then we know that the weapon must be the answer
isWeapon(W) :- weapon(W), numCantHave(W, Num), allWeapons(AllW), 
    length(AllW, Num), writeLog(['isWeap2', W]), addNewFact(isWeapAnswer(W)).

% One way to prove that I know a part of the solution is if there is 
% only one card left in the set of possible objects 
isRoom(R) :- possibleRooms([R]), writeLog(['isRoom1', R]), 
    addNewFact(isRoomAnswer(R)), addNewFact(isRoomAnswer(R)).

% if we know that the number of players that can't have a room is the number
% of rooms, then we know that the room must be the answer
isRoom(R) :- room(R), numCantHave(R, Num), allRooms(AllR), 
    length(AllR, Num), writeLog(['isRoom2', R]),
    addNewFact(isRoomAnswer(R)).

% Ways to prove a player has a card
% 1) I have the card
hasCard(Player, Card) :- iAm(Player), iHave(Card).

% 2) If they've shown it to me
hasCard(Player, Card) :- showedMe(Player, Card).

% 3) If I have deduced that they have a card
hasCard(Player, Card) :- deducedHasCard(Player, Card).


% Ways to prove a person does NOT have a card - the key to finding solution!
% 1) non-active suspects can not have any cards
cantHave(Player, _Card) :- suspect(Player), \+ activePlayer(Player).

% 2) If I am the player and do not have the card
cantHave(Player, Card) :- iAm(Player), \+ iHave(Card).

% 3) If someone else has the card
cantHave(Player, Card) :- hasCard(Player2, Card), Player2 \== Player.

% 4) If the solution has the card
cantHave(_Player, Card) :- isSusAnswer(Card).
cantHave(_Player, Card) :- isWeapAnswer(Card).
cantHave(_Player, Card) :- isRoomAnswer(Card).

% 5) If a suggestion was made with the card and they didn't refute it
cantHave(Player, Card) :- suggested(Card, Weapon, Room, _P), 
  didntRefute(Card, Weapon, Room, Player).
cantHave(Player, Card) :- suggested(Suspect, Card, Room, _P), 
  didntRefute(Suspect, Card, Room, Player).
cantHave(Player, Card) :- suggested(Suspect, Weapon, Card, _P), 
  didntRefute(Suspect, Weapon, Card, Player).
  

% Turn logging on/off
%writeLog(_). % not logging, just succeed.
% writeLog takes a list e.g., writeLog(['suspect is’, S, ‘!!!']).
writeLog([]) :- nl.
writeLog([H|T]) :- write(H),write(' '),writeLog(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% End of the game clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The items the player knew will be bound to a valid card, the unknown
% items will be invalid; so count the number of valid cards and
% ignore invalid cards - note that the first time through we'll
% assert that a player has 0 points
updateSuspectScore(Card, Player) :- 
   suspect(Card), 
   (suspectPointsFor(Player, Points), retract(suspectPointsFor(Player, _)); 
    Points is 0),
   NewScore is Points+1, asserta(suspectPointsFor(Player, NewScore)).
% Else, if card is not valid, just succeed
updateSuspectScore(_Card, _Player). 

updateWeaponScore(Card, Player) :- 
   weapon(Card),
   (weaponPointsFor(Player, Points), retract(weaponPointsFor(Player, _)); 
    Points is 0),
   NewScore is Points+1, asserta(weaponPointsFor(Player, NewScore)).
% Else, if card is not valid, just succeed
updateWeaponScore(_Card, _Player). 

updateRoomScore(Card, Player) :- 
   room(Card),
   (roomPointsFor(Player, Points), retract(roomPointsFor(Player, _)); 
    Points is 0),
   NewScore is Points+1, asserta(roomPointsFor(Player, NewScore)).
% Else, if card is not valid, just succeed
updateRoomScore(_Card, _Player). 

printPoints(Player) :- 
   (suspectPointsFor(Player, Spoints); (\+ suspectPointsFor(Player, _), 
    Spoints is 0)), 
   (weaponPointsFor(Player, Wpoints); (\+ weaponPointsFor(Player, _), 
    Wpoints is 0)), 
   (roomPointsFor(Player, Rpoints); (\+ roomPointsFor(Player, _), 
    Rpoints is 0)), 
   (gamesWon(Player, Won); (\+ gamesWon(Player, _), Won is 0)), 
      totalGames(Total), Points is Spoints+Wpoints+Rpoints,
   writeLog([Player,'\twon',Won,'of',Total,'games\tknown',Points,'cards,',
             Spoints,'suspects,',Wpoints,'weapons,',Rpoints,'rooms']).

