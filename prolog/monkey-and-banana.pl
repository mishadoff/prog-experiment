% Monkey & Banana problem

/*
  There is a monkey at the door into a room.
  In the middle of the room a banana is hanging from the ceiling.
  The monkey is hungry and wants to get the banana.
  At the window of the room there is a box the monkey may use.
  The monkey can perform following actions:

    1. Walk on the floor
    2. Climb the box
    3. Push the box around
    4. Grasp the banana

  Can the monkey get banana?

*/

% Grasp banana
move(state(middle, box, middle, empty),     % state before move
     grasp,                                   % type of move
     state(middle, floor, middle, banana)).   % state after move

% Climb onto the box
move(state(P, floor, P, E),
     climb,
     state(P, box, P, E)).

% Push the box
move(state(P1, floor, P1, H),
     push(P1, P2),
     state(P2, floor, P2, H)).

% Walk from P1 to P2
move(state(P1, floor, B, H),
     walk(P1, P2),
     state(P2, floor, B, H)).

canget(state(_, _, _, banana)).

canget(S1) :-
        move(S1, M, S2),
        canget(S2).

% Goal
% canget(state(door, floor, window, empty)).