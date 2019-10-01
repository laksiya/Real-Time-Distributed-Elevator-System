-module(start).

-export([node/1, node/2]).

% Easy interface for the final user

node(Port) ->
  {ok, FsmNetworkPid} = fsm_network:start(Port),
  register(fsm_network_pid, FsmNetworkPid).

node(Port, first) ->
  {ok, FsmNetworkPid} = fsm_network:start(Port, first),
  register(fsm_network_pid, FsmNetworkPid).
