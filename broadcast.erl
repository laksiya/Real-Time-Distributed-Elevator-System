%Functions for Control Server MODULE
-module(broadcast).
-export([call_global_order/2, get_all_states/0, delete_global_order/1]).
-define(TIMEOUT, 1000).

get_all_states()->
  % Get all other elevators current FSM states
  % Returns available nodes and according states
  {RemoteStates, _ResR}=rpc:multicall(nodes(), fsm_elevator, get_state, [],?TIMEOUT),
  {nodes(), RemoteStates}.

call_global_order({SelectedElevator, Ref, {Floor, Type}}, NodesAvailable) ->
  % Send a new order with assigned elevator to all other available elevators' global queue
  % also returns if the elevator which was assigned is disconnected before recieving order 
  {_ResL, NodesDown} = rpc:multicall(NodesAvailable, control_server, add_remote_order, [{SelectedElevator, Ref, {Floor, Type}}],?TIMEOUT),
  lists:keymember(SelectedElevator, 1, NodesDown).

delete_global_order(Ref) ->
  % Delete executed order with reference in every other elevators global queue
  rpc:eval_everywhere(nodes(), control_server, order_finished, [remote, Ref]).
