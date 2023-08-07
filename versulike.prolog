% versulike
% versu w/o the exclusion logic base;
%uses Prolog predicate logic instead

:- dynamic world_data/2.
:- multifile world_data/2.

action_definition(action(Name, Preconditions, Postconditions), Name, Preconditions, Postconditions).

social_practice_definition(social_practice(Name, Roles, Actions), Name, Roles, Actions).

agent_definition(agent(Name, Desires, Data), Name, Desires, Data).

general_definition(general(Name, Data), Name, Data).

state_definition(state(SaveFilename, Agents, SocialPractices, GeneralStuff), SaveFilename, Agents, SocialPractices, GeneralStuff).

list_append([], A, [A]).
list_append([E | Next], A, Out) :-
    Out = [E | OutNext],
    list_append(Next, A, OutNext).

list_has([A | _], A).
list_has([_ | Next], A) :-
    list_has(Next, A).

list_replace([O | OldNext], Old, New, [N | NewNext]) :-
    (O = Old, N = New; O \= Old, N = O),
    (OldNext = [], NewNext = []; list_replace(OldNext, Old, New, NewNext)).

atoms_list_concat(List, Atom) :-
    atoms_list_concat_(List, '', Atom).
atoms_list_concat_([], A, A).
atoms_list_concat_([A | Next], Temp, Atom) :-
    atom_concat(Temp, A, TempNext),
    atoms_list_concat_(Next, TempNext, Atom).

agent_add_desire(ABefore, Desire, AAfter) :-
    agent_definition(ABefore, Name, OldDesires, Data),
    list_append(OldDesires, Desire, NewDesires),
    agent_definition(AAfter, Name, NewDesires, Data).
agent_add_data(ABefore, DataName, Value, AAfter) :-
    agent_definition(ABefore, AgentName, Desires, OldData),
    general_definition(AddedData, DataName, Value),
    list_append(OldData, AddedData, NewData),
    agent_definition(AAfter, AgentName, Desires, NewData).

agent_base_preset(Name, Agent) :-
    general_definition(LatestAction, latestaction, none),
    agent_definition(Agent, Name, [], [LatestAction]).
agent_npc_preset(Name, Agent) :-
    agent_base_preset(Name, PresetAgent),
    agent_add_data(PresetAgent, npc, true, Agent).
agent_player_preset(Name, Agent) :-
    agent_base_preset(Name, A1),
    agent_add_data(A1, player, true, A2),
    agent_add_data(A2, playerinput, none, Agent).

world_data_add(Data, ShouldInstance) :-
    (
        social_practice_definition(Data, _, _, _), Category = social_practices;
        agent_definition(Data, _, _, _), Category = characters;
        general_definition(Data, _, _), Category = general_stuff
    ),
    assertz(world_data(Category, Data)),
    (ShouldInstance = true, assertz(world_data(instances, Data)); ShouldInstance = false).

% adding definitions for a social practice to contain the default "wait" action
world_data(social_practices, WaitSP) :-
    social_practice_definition(WaitSP, 'Default', [_], [WaitAction]),
    action_definition(WaitAction, 'wait', wait_precondition, wait_postcondition).
world_data(instances, WaitSP) :-
    social_practice_definition(WaitSP, 'Default', _, _),
    world_data(social_practices, WaitSP).

world_data(flavor(ToDisplay), displaystate(mainmenu)) :-
    ToDisplay = 'Welcome to Versulike!\nIt''s like Versu, except not.'.
world_data(flavor(ToDisplay), displaystate(State)) :-
    state_definition(State, _, _, _, _),
    ToDisplay = State.
world_data(flavor(D), displayoptions(S)) :-
    findall(X, (process_state_input(S, X, _)), D).

wait_precondition(C, C).

wait_postcondition(C, C).

new_game(S) :-
    state_definition(S, 'new_save.txt', Agents, SocialPractices, GeneralStuff),
    findall(A, (world_data(instances, A), agent_definition(A, _, _, _)), Agents),
    findall(SP, (world_data(instances, SP), social_practice_definition(SP, _, _, _)), SocialPractices),
    findall(G, (world_data(instances, G), general_definition(G, _, _)), GeneralStuff).

% dream predicates
% The following are predicates to eventually implement after getting the
% base of versulike working

% given a game state with a filename, saves current instances to file
% save_game(state('', )) :-

% given a filename, loads instances from file into current state
% load_game(state('', )) :-

% world_data_save/2 saves current world_data/2 to given file
% world_data_load/2 loads world_data/2 from given file

display_state(S) :-
    write('-~-~-~-~-'),
    nl,
    world_data(flavor(D1), displaystate(S)),
    write(D1),
    nl,
    write('   -~-   '),
    nl,
    write('Your options are:'),
    nl,
    world_data(flavor(D2), displayoptions(S)),
    write(D2),
    nl,
    write('-~-~-~-~-'),
    nl.

get_possible_actions(State, ActionsOut) :-
    state_definition(State, _, [TopAgent | _], SocialPractices, _),
    findall(SuccessfulAction,
        (
            list_has(SocialPractices, SP),
            social_practice_definition(SP, _, Roles, Actions),
            list_has(Roles, TopAgent), 
            list_has(Actions, SuccessfulAction),
            action_definition(SuccessfulAction, _, PreConditions, _),
            call(PreConditions, State, _)
        ),
    ActionsOut).

sum_desire_score(Desires, State, Score) :-
    sum_desire_score_(Desires, State, 0, Score).
sum_desire_score_([], _, Temp, Final) :-
    Final is Temp.
sum_desire_score_([Desire | NextDesire], State, Temp, Final) :-
    call(Desire, State, Temp2),
    Temp3 is Temp + Temp2,
    sum_desire_score_(NextDesire, State, Temp3, Final).

% rate the actions of the top agent in the list
rate_single_action(A, State, RA) :-
    state_definition(State, _, [TopAgent | _], _, _),
    action_definition(A, _, PreCond, PostCond),
    call(PreCond, State, State2),
    call(PostCond, State2, State3),
    agent_definition(TopAgent, _, Desires, _),
    sum_desire_score(Desires, State3, DesiresScore),
    RA = -((-DesiresScore), A).
rate_actions(UnRatedActions, State, RatedActions) :-
    state_definition(State, _, _, _, _),
    findall(RA, (list_has(UnRatedActions, A), rate_single_action(A, State, RA)), RatedActions).

agent_is_player(A) :-
    agent_definition(A, _, _, Data),
    general_definition(IsPlayer, player, true),
    list_has(Data, IsPlayer),
    general_definition(PlayerInput, playerinput, _),
    list_has(Data, PlayerInput).

agent_is_npc(A) :-
    agent_definition(A, _, _, Data),
    general_definition(IsNPC, npc, true),
    list_has(Data, IsNPC).

npc_choose_action(RatedActions, ActionOut) :-
    keysort(RatedActions, RatedSortedActions),
    RatedSortedActions = [-(_, ActionOut) | _].

% this is the most important agent-related predicate, makes the agent choose & do an action
%   (if player, chosen action is stored within the agent as data)
process_single_agent(Agent, StateIn, StateOut) :-
    agent_definition(Agent, _, _, _),
    get_possible_actions(StateIn, Actions),
    rate_actions(Actions, StateIn, RatedActions),
    (
        agent_is_player(Agent),
        list_has(Actions, ChoosenAction);
        agent_is_npc(Agent),
        npc_choose_action(RatedActions, ChoosenAction)
    ),
    action_definition(ChoosenAction, _, Precond, Postcond),
    call(Precond, StateIn, StateTemp1),
    call(Postcond, StateTemp1, StateTemp2),
    general_definition(OldLatestAction, latestaction, _),
    general_definition(NewLatestAction, latestaction, ChoosenAction),
    state_definition(StateTemp2, FN, [AOld | OtherAgents], SP, GN),
    agent_definition(AOld, NM, DS, OldData),
    list_replace(OldData, OldLatestAction, NewLatestAction, NewData),
    agent_definition(ANew, NM, DS, NewData),
    state_definition(StateOut, FN, [ANew | OtherAgents], SP, GN).

% process any present npc agents (if any) until a player is encountered
process_npcs(State, State) :-
    state_definition(State, _, [Agent | _], _, _),
    agent_is_player(Agent).
process_npcs(State, StateOut) :-
    state_definition(State, FN, [TopAgent | _], _, _),
    agent_is_npc(TopAgent),
    process_single_agent(TopAgent, State, State2),
    state_definition(State2, FN, [UpdatedTopAgent | OtherAgents], SP, GT),
    list_append(OtherAgents, UpdatedTopAgent, ReorderedAgents),
    state_definition(State3, FN, ReorderedAgents, SP, GT),
    process_npcs(State3, StateOut).

process_players(State, State) :-
    state_definition(State, _, [Agent | _], _, _),
    agent_is_npc(Agent).
process_players(State, StateOut) :-
    state_definition(State, FN, [TopAgent | _], _, _),
    agent_is_player(TopAgent),
    process_single_agent(TopAgent, State, State2),
    state_definition(State2, FN, [UpdatedTopAgent | OtherAgents], SP, GT),
    list_append(OtherAgents, UpdatedTopAgent, ReorderedAgents),
    state_definition(State3, FN, ReorderedAgents, SP, GT),
    process_players(State3, StateOut).

process_state_input(mainmenu, exit, exit).
process_state_input(mainmenu, newgame, State) :-
    new_game(State).
process_state_input(State, mainmenu, mainmenu) :-
    state_definition(State, _, _, _, _).
process_state_input(State, PlayerAction, StateOut) :-
    state_definition(State, SaveFileName, Agents, SocialPractices, GeneralThings),
    Agents = [TopAgent | OtherAgents],
    agent_is_player(TopAgent),
    agent_definition(TopAgent, AgentName, AgentDesires, AgentData),
    get_possible_actions(State, PossibleActions),
    list_has(PossibleActions, A),
    action_definition(A, PlayerAction, _, _),
    general_definition(DOld, playerinput, _),
    general_definition(DNew, playerinput, PlayerAction),
    list_replace(AgentData, DOld, DNew, UpdatedAgentData),
    agent_definition(UpdatedTopAgent, AgentName, AgentDesires, UpdatedAgentData),
    state_definition(StateWPlayerInput, SaveFileName, [UpdatedTopAgent | OtherAgents], SocialPractices, GeneralThings),
    process_players(StateWPlayerInput, StateOut).

game_loop(exit).
game_loop(S) :-
    (
        \+(state_definition(S, _, _, _, _)), S2 = S;
        state_definition(S, _, _, _, _), process_npcs(S, S2)
    ),
    display_state(S2),
    repeat,
    read(Input),
    nonvar(Input),
    process_state_input(S2, Input, S3),
    game_loop(S3).

play :-
    State = mainmenu,
    game_loop(State).
