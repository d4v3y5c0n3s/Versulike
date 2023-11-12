% versulike
% versu w/o the exclusion logic base;
%uses Prolog predicate logic instead

:- multifile agent_data/2.
:- multifile social_practice_data/2.
:- multifile general_data/2.
:- multifile flavor_text/3.
:- multifile describe_action/2.
:- multifile apply_input/3.

action_definition(action(Name, Precondition, Postcondition), Name, Precondition, Postcondition).

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

agent_data_named(Agent, Name) :-
  Agent = agent(Name, _, _),
  agent_data(Agent, _).

social_practice_preset(Name, SP) :-
    social_practice_definition(SP, Name, roles([], []), []).

social_practice_add_role(SP_In, Role, SP_Out) :-
    social_practice_definition(SP_In, Name, roles(OldPossibleRoles, ActiveRoles), Agents),
    list_append(OldPossibleRoles, Role, NewPossibleRoles),
    social_practice_definition(SP_Out, Name, roles(NewPossibleRoles, ActiveRoles), Agents).

social_practice_assign_role(SP_In, Role, Agent, SP_Out) :-
    social_practice_definition(SP_In, Name, roles(PossibleRoles, OldActiveRoles), Agents),
    list_has(Agents, Agent),
    list_has(PossibleRoles, Role),
    list_append(OldActiveRoles, active_role(Role, Agent), NewActiveRoles),
    social_practice_definition(SP_Out, Name, roles(PossibleRoles, NewActiveRoles), Agents).

social_practice_add_agent(SP_In, Agent, SP_Out) :-
    social_practice_definition(SP_In, Name, Roles, OldAgents),
    list_append(OldAgents, Agent, NewAgents),
    social_practice_definition(SP_Out, Name, Roles, NewAgents).

action_condition_preset([], State, State).
action_condition_preset([Condition | Next], StateIn, StateOut) :-
    call(Condition, StateIn, StateTemp),
    action_precondition_preset(Next, StateTemp, StateOut).

social_practice_add_action(SP_In, Action, SP_Out) :-
    social_practice_definition(SP_In, Name, Roles, OldActions),
    action_definition(Action, _, _, _),
    list_append(OldActions, Action, NewActions),
    social_practice_definition(SP_Out, Name, Roles, NewActions).

action_preset(Name, Action) :-
    action_definition(Action, Name, action_precondition_preset, action_postcondition_preset).

action_change_precondition(A_In, Precond, A_Out) :-
    action_definition(A_In, Name, _, Postcond),
    action_definition(A_Out, Name, Precond, Postcond).

action_change_postcondition(A_In, Postcond, A_Out) :-
    action_definition(A_In, Name, Precond, _),
    action_definition(A_Out, Name, Precond, Postcond).

describe_action(A, '...') :-
    action_definition(A, 'wait', _, _).

wait_precondition(C, C).

wait_postcondition(C, C).

social_practice_data(WaitSP, true) :-
  social_practice_definition(WaitSP, 'Default', [_], [WaitAction]),
  action_definition(WaitAction, 'wait', wait_precondition, wait_postcondition).

default_save_file_name('new_save.txt').

default_flavor_text(mainmenu, default, 'Welcome to Versulike!\nIt''s like Versu, except not.').
default_flavor_text(savegame(State), default, Text) :-
  state_definition(State, FileName, _, _, _),
  atoms_list_concat([
    'You may save to the file "',
    FileName,
    '" or change which file you wish to save to.'
  ], Text).
default_flavor_text(loadgame(SaveFileName), default, Text) :-
  atoms_list_concat([
    'You may load the file "',
    SaveFileName,
    '" or choose a different file to load instead.'
  ], Text).
default_flavor_text(State, default, Text) :-
  state_definition(State, _, Agents, _, _),
  findall(
    ZF,
    (
      list_has(Agents, A),
      agent_definition(A, AgentName, _, Data),
      list_has(Data, G),
      general_definition(G, latestaction, Latest),
      atom_concat(AgentName, ': ', Z1),
      describe_action(Latest, DescribedLatest),
      atom_concat(Z1, DescribedLatest, Z2),
      atom_concat(Z2, '\n', ZF)
    ),
    DisplayLatest
  ),
  atoms_list_concat(DisplayLatest, Text).
default_flavor_text(State, options, Text) :-
  findall(X, (process_state_input(State, X, _)), ActionOptions),
  findall(FO, (list_has(ActionOptions, Opt), atom_concat(Opt, '.\n', FO)), FormattedOptions),
  atoms_list_concat(FormattedOptions, Text).

show_flavor_text(State, Purpose, Text) :-
  (
    current_predicate(flavor_text/3),
    flavor_text(State, Purpose, Text);
    default_flavor_text(State, Purpose, Text)
  ).

new_game(S) :-
  default_save_file_name(SaveFileName),
  state_definition(S, SaveFileName, Agents, SocialPractices, GeneralStuff),
  findall(A, (agent_data(A, true), agent_definition(A, _, _, _)), Agents),
  findall(SP, (social_practice_data(SP, true), social_practice_definition(SP, _, _, _)), SocialPractices),
  findall(G, (general_data(G, true), general_definition(G, _, _)), GeneralStuff).

% given a game state with a filename, saves current instances to file
save_game(State) :-
  state_definition(State, FileName, Agents, SocialPractices, GeneralStuff),
  open(FileName, write, Stream, [encoding(utf8)]),
  save_values(Stream, Agents),
  nl(Stream),
  save_values(Stream, SocialPractices),
  nl(Stream),
  save_values(Stream, GeneralStuff),
  close(Stream).
save_values(_, []).
save_values(Stream, [Value | Next]) :-
  write_term(Stream, Value, [quoted(true)]),
  write(Stream, '. '),
  save_values(Stream, Next).

% given a filename, loads instances from file into current state
load_game(State) :-
  state_definition(State, FileName, Agents, SocialPractices, GeneralStuff),
  open(FileName, read, Stream, [encoding(utf8)]),
  load_values(Stream, Agents, SocialPractices, GeneralStuff),
  close(Stream).
load_values(Stream, A, SP, G) :-
  read(Stream, Value),
  (
    Value = end_of_file,
    A = [],
    SP = [],
    G = [];
    (
      agent_definition(Value, _, _, _),
      A = [Value | ANext],
      SP = SPNext,
      G = GNext;
      social_practice_definition(Value, _, _, _),
      A = ANext,
      SP = [Value | SPNext],
      G = GNext;
      general_definition(Value, _, _),
      A = ANext,
      SP = SPNext,
      G = [Value | GNext]
    ),
    load_values(Stream, ANext, SPNext, GNext)
  ).

display_state(S) :-
  nl,
  nl,
  nl,
  write('-~-~-~-~-'),
  nl,
  show_flavor_text(S, default, D1),
  write(D1),
  nl,
  write('   -~-   '),
  nl,
  write('Your options are:'),
  nl,
  show_flavor_text(S, options, D2),
  write(D2),
  nl,
  write('-~-~-~-~-'),
  nl.

perform_action(Action, StateIn, StateOut) :-
  action_definition(Action, _, PreConds, PostConds),
  call(PreConds, StateIn, StateTemp),
  call(PostConds, StateTemp, StateOut).

get_possible_actions(State, Agent, ActionsOut) :-
  state_definition(State, _, AgentList, SocialPractices, _),
  list_has(AgentList, Agent),
  findall(SuccessfulAction,
    (
      list_has(SocialPractices, SP),
      social_practice_definition(SP, _, Roles, Actions),
      list_has(Roles, Agent), 
      list_has(Actions, SuccessfulAction),
      perform_action(SuccessfulAction, State, _)
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

% rate the actions of the given agent
rate_single_action(Action, Agent, State, RatedAction) :-
  state_definition(State, _, AgentList, _, _),
  list_has(AgentList, Agent),
  perform_action(Action, State, StateAfter),
  agent_definition(Agent, _, Desires, _),
  sum_desire_score(Desires, StateAfter, DesiresScore),
  RatedAction = -((-DesiresScore), Action).
rate_actions(UnRatedActions, Agent, State, RatedActions) :-
  state_definition(State, _, _, _, _),
  findall(RA, (list_has(UnRatedActions, A), rate_single_action(A, Agent, State, RA)), RatedActions).

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
process_single_agent(Agent, StateIn, StateOut) :-
  agent_definition(Agent, NM, DS, OldData),
  get_possible_actions(StateIn, Agent, Actions),
  rate_actions(Actions, Agent, StateIn, RatedActions),
  (
    agent_is_player(Agent),
    list_has(Actions, ChoosenAction);
    agent_is_npc(Agent),
    npc_choose_action(RatedActions, ChoosenAction)
  ),
  perform_action(ChoosenAction, StateIn, StateTemp),
  general_definition(OldLatestAction, latestaction, _),
  general_definition(NewLatestAction, latestaction, ChoosenAction),
  state_definition(StateTemp, FN, PreviousAgents, SP, GN),
  list_replace(OldData, OldLatestAction, NewLatestAction, NewData),
  agent_definition(NewAgent, NM, DS, NewData),
  list_replace(PreviousAgents, Agent, NewAgent, NewAgents),
  state_definition(StateOut, FN, NewAgents, SP, GN).

process_agents([Agent | Next], StateIn, PlayerAction, StateOut) :-
  state_definition(StateIn, SaveFileName, AllAgents, SocialPractices, GeneralThings),
  agent_is_player(Agent),
  agent_definition(Agent, AgentName, AgentDesires, AgentData),
  get_possible_actions(StateIn, Agent, PossibleActions),
  list_has(PossibleActions, A),
  action_definition(A, PlayerAction, _, _),
  general_definition(DOld, playerinput, _),
  general_definition(DNew, playerinput, PlayerAction),
  list_replace(AgentData, DOld, DNew, UpdatedAgentData),
  agent_definition(UpdatedAgent, AgentName, AgentDesires, UpdatedAgentData),
  list_replace(AllAgents, Agent, UpdatedAgent, UpdatedAllAgents),
  state_definition(StateWPlayerInput, SaveFileName, UpdatedAllAgents, SocialPractices, GeneralThings),
  process_single_agent(UpdatedAgent, StateWPlayerInput, StateTemp),
  process_agents_after_player(Next, StateTemp, PlayerAction, StateOut).
process_agents([Agent | Next], StateIn, PlayerAction, StateOut) :-
  agent_is_npc(Agent),
  process_single_agent(Agent, StateIn, StateTemp),
  process_agents(Next, StateTemp, PlayerAction, StateOut).
process_agents_after_player([], StateOut, _, StateOut).
process_agents_after_player([Agent | Next], StateIn, PlayerAction, StateOut) :-
  agent_is_npc(Agent),
  process_single_agent(Agent, StateIn, StateTemp),
  process_agents_after_player(Next, StateTemp, PlayerAction, StateOut).

process_state_input(mainmenu, exit, exit).
process_state_input(mainmenu, newgame, State) :-
  new_game(State).
process_state_input(State, mainmenu, mainmenu) :-
  state_definition(State, _, _, _, _).
process_state_input(State, savegame, savegame(State)) :-
  state_definition(State, _, _, _, _).
process_state_input(savegame(State), save, State) :-
  save_game(State).
process_state_input(mainmenu, loadgame, loadgame(SaveFileName)) :-
  default_save_file_name(SaveFileName).
process_state_input(loadgame(SaveFileName), load, State) :-
  state_definition(State, SaveFileName, _, _, _),
  load_game(State).
process_state_input(State, PlayerAction, StateOut) :-
  state_definition(State, _, Agents, _, _),
  process_agents(Agents, State, PlayerAction, StateOut).
process_state_input(loadgame(SaveFileName), input_new, receive_input(SaveFileName)).
process_state_input(savegame(State), input_new, receive_input(SaveFileName)) :-
  state_definition(State, SaveFileName, _, _, _).

wants_input(input_new, receive_input(_)).

get_user_input(UserInput) :-
  repeat,
  nl,
  write('Please, type something in:'),
  nl,
  read(UserInput),
  nonvar(UserInput),
  nl,
  write('Thanks!'),
  nl.

apply_input(loadgame(_), UserInput, loadgame(UserInput)).
apply_input(savegame(State), UserInput, savegame(NewState)) :-
  state_definition(State, _, Agents, SocialPractices, GeneralStuff),
  state_definition(NewState, UserInput, Agents, SocialPractices, GeneralStuff).

game_loop(exit).
game_loop(S) :-
  display_state(S),
  repeat,
  read(Input),
  nonvar(Input),
  process_state_input(S, Input, S2),
  (
    wants_input(Input, S2),
    get_user_input(UserInput),
    apply_input(S, UserInput, SFinal),
    game_loop(SFinal);
    \+(wants_input(Input, S2)), game_loop(S2)
  ).

play :-
    State = mainmenu,
    game_loop(State).
