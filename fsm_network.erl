-module(fsm_network).

-behaviour(gen_fsm).
-record(network_info, {send_socket, listen_socket, prev_node, monitoring_node, next_node}).
-define(COOKIE, test).
-define(REFERENCE_PORT, 20000).
-define(RANGE, 5).
-define(BROADCAST_TIME, 1000).
-define(TICK_TIME, 1000).

-export([start/1, start/2, start_link/1, start_link/2, request_monitor_network/1, set_next_node/1, get_monitoring_node/1, get_info/0]).
-export([init/1, listen/2, broadcast/2, broadcast/3, handle_event/3, handle_sync_event/4, handle_info/3,code_change/4, terminate/3]).

% Program is started with following lines:
% {ok, FsmNetworkPid} = fsm_network:start(15657, first), register(fsm_network_pid, FsmNetworkPid).
% {ok, FsmNetworkPid} = fsm_network:start(15657), register(fsm_network_pid, FsmNetworkPid).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% MODULE API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start(ElevatorPort, first) ->
  gen_fsm:start(?MODULE, [{ElevatorPort, first}], []).

start(ElevatorPort) ->
  gen_fsm:start(?MODULE, [ElevatorPort], []).

start_link(ElevatorPort, first) ->
  gen_fsm:start_link(?MODULE, [ElevatorPort, first], []).

start_link(ElevatorPort) ->
  gen_fsm:start_link(?MODULE, [ElevatorPort], []).

request_monitor_network(FromNode) ->
  gen_fsm:sync_send_event(whereis(fsm_network_pid), {request_monitor_network, FromNode}).

set_next_node(FromNode) ->
  gen_fsm:sync_send_event(whereis(fsm_network_pid), {set_next_node, FromNode}).

get_monitoring_node(FromNode) ->
  gen_fsm:sync_send_event(whereis(fsm_network_pid), {get_monitoring_node, FromNode}).

get_info() ->
  gen_fsm:sync_send_all_state_event(whereis(fsm_network_pid), get_state_info).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CALLBACK FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

init([StartInfo]) ->
  % Node is initialized with name PORT@IP_ADDRESS and starts monitoring self,
  % until it's connected to another node. Then its start monitoring in a circle formation.

  os:cmd("epmd -daemon"),
  IPaddr = inet:ntoa(get_local_ip()),
  monitor_node(node()),

  % If you have been initialized as first elevator, then the node starts BROADCASTING.
  % Otherwise the node is initialized then starts LISTENING.

  case StartInfo of
    {ElevatorPort, first} ->
      % Node initialisation
      Num = integer_to_list(ElevatorPort),
      LName = Num ++ "@",
      NodeName = list_to_atom(LName ++ IPaddr),
      net_kernel:start([NodeName,longnames, ?TICK_TIME]),
      erlang:set_cookie(node(), ?COOKIE),
      %%%%%
      Socket = open_send_socket(?REFERENCE_PORT),
      {_SendSocket, SendPort}=Socket,
      io:format("Im BROADCASTING from PORT ~p~n", [SendPort]),
      init_elevator(ElevatorPort),
      send_signal(),
      {ok, broadcast, #network_info{send_socket=Socket, monitoring_node=node()}};

    ElevatorPort ->
      Num = integer_to_list(ElevatorPort),
      LName = Num ++ "@",
      NodeName = list_to_atom(LName ++ IPaddr),
      net_kernel:start([NodeName,longnames, ?TICK_TIME]),
      erlang:set_cookie(node(), ?COOKIE),
      %%%%%
      Socket = open_listen_socket(?REFERENCE_PORT),
      init_elevator(ElevatorPort),
      send_signal(),
      {ok, listen, #network_info{listen_socket=Socket, monitoring_node=node()}}

  end.


listen(send_signal, N=#network_info{listen_socket={ListenSocket, ListenPort}, monitoring_node=MonitoringNode}) ->
  % Start listening until a message with a new node's name is recieved. Wait until all you local orders are executed(idle)
  % until trying to connect.

  io:format("Im LISTENING from PORT ~p~n", [ListenPort]),
  {ok,{_FoundNodeIP,_FromPort,Node_name}} = gen_udp:recv(ListenSocket,0),
  FoundNode = list_to_atom(Node_name),
  wait_until_idle(),

  % Connect to the new node
    % CONNECTION PROTOCOL 0:
    %   - Get a copy of the new node's list and add it to my list
    %   - Check if any of those orders are assigned to my node.
    % CONNECTION PROTOCOL 1/3/5:
    %   - Stop listening and start monitoring the new node instead of previous.
    %   - In case the node you are monitoring disconnects, keep track of which it shall monitor next.
    % Once you're in the network, start BROADCASTING to fin other nodes

  case net_kernel:connect_node(FoundNode) of
    true->
      io:format("Done listening, I'M NEW IN THE TEAM~n", []),
      io:format("Stablished connection with ~p~n", [FoundNode]),
      %%%% CONNECTION PROTOCOL %%%%%%%%%%% 0 %%%%%%%%%%%%
      control_server:transfer_queue(FoundNode),
      control_server:check_my_orders(),
      %%%% CONNECTION PROTOCOL %%%%%%%%%%% 1 %%%%%%%%%%%%
      gen_udp:close(ListenSocket),
      demonitor_node(MonitoringNode),
      {PrevFoundNode, NewMonitoringNode} = rpc:call(FoundNode, fsm_network, request_monitor_network, [node()]),
      %%%% CONNECTION PROTOCOL %%%%%%%%%%% 3 %%%%%%%%%%%%
      if PrevFoundNode =/= node() ->
        rpc:call(PrevFoundNode, fsm_network, set_next_node, [node()])
      end,
      %%%% CONNECTION PROTOCOL %%%%%%%%%%% 5 %%%%%%%%%%%%
      io:format("Monitoring... ~p~n", [NewMonitoringNode]),
      monitor_node(NewMonitoringNode),
      NextNode = rpc:call(NewMonitoringNode, fsm_network, get_monitoring_node, [node()]),
      io:format("Next Monitoring... ~p~n", [NextNode]),
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      Socket = open_send_socket(?REFERENCE_PORT),
      {_SendSocket, SendPort}=Socket,
      io:format("Im BROADCASTING from PORT ~p~n", [SendPort]),
      send_signal(),
      {next_state, broadcast, N#network_info{send_socket=Socket, prev_node=FoundNode, monitoring_node=NewMonitoringNode, next_node=NextNode}};
    _->
      io:format("ERROR\n"),
      send_signal(),
      {next_state, listen, N}
  end.

%%%%%%%%%%%% CONNECTION PROTOCOL %%%%%%%% 2 %%%%%%%%
broadcast({request_monitor_network, FromNode}, _From, N=#network_info{prev_node=PrevNode, monitoring_node=MonitoringNode}) ->
  demonitor_node(MonitoringNode),
  io:format("Demonitoring... ~p~n", [MonitoringNode]),
  monitor_node(FromNode),
  io:format("Monitoring... ~p~n", [FromNode]),
  case node() of
    MonitoringNode ->
        {reply, {PrevNode, MonitoringNode}, broadcast, N#network_info{prev_node=FromNode, monitoring_node=FromNode, next_node=MonitoringNode}};
    _ ->
        {reply, {PrevNode, MonitoringNode}, broadcast, N#network_info{monitoring_node=FromNode, next_node=MonitoringNode}}
  end;
%%%%%%%%%%%% CONNECTION PROTOCOL %%%%%%%% 4 %%%%%%%%
broadcast({set_next_node, FromNode}, _From, N=#network_info{}) ->
  {next_state, broadcast, N#network_info{next_node=FromNode}};
%%%%%%%%%%%% CONNECTION PROTOCOL %%%%%%%% 6 %%%%%%%%
broadcast({get_monitoring_node, FromNode}, _From, N=#network_info{monitoring_node=MonitoringNode}) ->
  {reply, MonitoringNode, broadcast, N#network_info{prev_node=FromNode}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

broadcast(send_signal, N=#network_info{send_socket=Socket}) ->
  broadcast_in_range(Socket, init),
  send_signal(),
  {next_state, broadcast, N}.

handle_sync_event(get_state_info, _From, State, StateData) ->
  {reply, StateData, State, StateData}.

handle_info(Message, StateName, N=#network_info{prev_node=PrevNode, monitoring_node=MonitoringNode, next_node=NextNode}) ->
  case Message of
    {nodedown, MonitoringNode} ->
      case nodes() of
        [] ->
          io:format("I'm OUT OF THE TEAM ;(~n", []),
          %%%%%%%%%%%% DESCONNECTION PROTOCOL %%%%%% The one OUT of NET %%%%%%%%%%%%%%%%%%%%
          monitor_node(node()),
          control_server:redistribute_hall_orders(MonitoringNode, normal),
          Socket = open_listen_socket(?REFERENCE_PORT),
          {next_state, listen, N#network_info{listen_socket=Socket, monitoring_node=node()}};
        _ ->
          io:format("I lost connection with ~p~n", [MonitoringNode]),
          %%%%%%%%%%%% DESCONNECTION PROTOCOL %%%%%% The one IN the NET %%%%%%%%%%%%%%%%%%%
          control_server:redistribute_hall_orders(MonitoringNode, normal),
          io:format("Monitoring... ~p~n", [NextNode]),
          monitor_node(NextNode),
          NewNextNode = rpc:call(NextNode, fsm_network, get_monitoring_node, [node()]),
          rpc:call(PrevNode, fsm_network, set_next_node, [NextNode]),
          {next_state, broadcast, N#network_info{monitoring_node=NextNode, next_node=NewNextNode}}
      end;
    {udp, _, _, _, _} ->
      {next_state, StateName, N};
    Mes ->
      io:format("Message ~p~n", [Mes]),
      {next_state, StateName, N}
  end.

%%%%%%%%%% DEFAULT CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%
handle_event( _, State, StateData) ->
  {next_state, State, StateData}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

terminate(normal, _State, S) ->
  ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

get_local_ip() ->
  %Gets private IP address
  {ok, Addrs} = inet:getifaddrs(),
  hd([Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts, size(Addr) == 4, check_ip_v4(Addr), Addr =/= {127,0,0,1}]).

check_ip_v4({First, _, _, _})->
  %Checks its the IPV4 address which starts at 10
  case First of
    10 ->
      true;
    _ ->
      false
  end.

send_signal() ->
  % Broadcasts a message for BROADCAST_TIME time
  gen_fsm:send_event_after(?BROADCAST_TIME, send_signal).

open_listen_socket(Port) ->
  % If Port is busy, the function tries another port until listen_socket is opened.
  case gen_udp:open(Port,[list,{active,false}]) of
    {ok, ListenSocket} ->
      {ListenSocket, Port};
    _ ->
      open_listen_socket(Port+1)
  end.

open_send_socket(Port) ->
  % If Port is busy, the function tries another port until send_socket is opened.
  case gen_udp:open(Port, [list, {active,true}, {broadcast, true}]) of
    {ok, SendSocket} ->
      {SendSocket, Port};
    _ ->
      open_send_socket(Port+1)
  end.

init_elevator(Port)->
  %Initializes the elevator by starting a process for modules and spawns process
  %for read_sensor and all buttons.
  {ok, ControlServerPid} = control_server:start(),
  register(control_server_pid, ControlServerPid),
  {ok, DriverPid} = elevator_interface:start({127,0,0,1}, Port),
  register(driver_pid, DriverPid),
  {ok, FSMElevatorPid} = fsm_elevator:start(),
  register(fsm_elevator_pid, FSMElevatorPid),
  spawn(driver, read_sensor, [ready, self()]),
  driver:buttons_init().

monitor_node(Node) ->
  erlang:monitor_node(Node, true).

demonitor_node(Node) ->
  erlang:monitor_node(Node, false).

%%% Broadcast in range broadcast from send_socket
%%% to all ports within a RANGE to reach the correct listen_socket
broadcast_in_range({SendSocket, Port}, init) ->
  broadcast_in_range({SendSocket, Port-?RANGE div 2}, 1);

broadcast_in_range({SendSocket, Port}, Count) when Count =:= ?RANGE ->
  gen_udp:send(SendSocket,{255,255,255,255}, Port, atom_to_list(node()));

broadcast_in_range({SendSocket, Port}, Count) ->
  gen_udp:send(SendSocket,{255,255,255,255}, Port, atom_to_list(node())),
  broadcast_in_range({SendSocket, Port+1}, Count+1).

wait_until_idle() ->
  case control_server:queue_empty() of
    false ->
      timer:sleep(100),
      wait_until_idle();
    true ->
      ok
  end.
