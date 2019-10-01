-module(control_server).

-behaviour(gen_server).

-export([start/0, start_link/0, add_remote_order/1, add_local_order/2, order_finished/1, order_finished/2,
kill/0, transfer_queue/1, get_queue/0, queue_empty/0, check_my_orders/0, redistribute_hall_orders/2]).
-export([init/1, handle_cast/2, terminate/2, handle_call/3, handle_info/2]).
-export([print_list/1]).
-define(RE_TIME, 10000).
-define(NUM_FLOOR, 4-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% MODULE API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link([]).

add_local_order(Floor, Type) ->
    gen_server:cast(whereis(control_server_pid), {add_local_order, Floor, Type}).

add_remote_order(Order) ->
    gen_server:cast(whereis(control_server_pid), {add_remote_order, Order}).

redistribute_hall_orders(Node, SituationType) ->
    gen_server:cast(whereis(control_server_pid), {redistribute, Node, SituationType}).

transfer_queue(From) ->
    gen_server:cast(whereis(control_server_pid), {transfer_queue, From}).

check_my_orders() ->
  gen_server:call(whereis(control_server_pid), check_my_orders).

get_queue() ->
    gen_server:call(whereis(control_server_pid), get_queue).

queue_empty() ->
    gen_server:call(whereis(control_server_pid), queue_empty).

order_finished(Ref) ->
    gen_server:cast(whereis(control_server_pid), {order_finished, Ref}).

order_finished(remote, Ref) ->
    gen_server:cast(whereis(control_server_pid), {order_finished_remote, Ref}).

kill() ->
    gen_server:cast(whereis(control_server_pid), terminate).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CALLBACK FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
  % Initialize the elevator's global queue with an empty list.
  {ok, []}.

handle_cast({add_local_order, Floor, cab}, GlobalQueue) ->
  % Local cab order added to all global queues,local queue in FSM with unique reference and set light.
  % Only added if cab button is not already pushed.
  case cab_already_ordered(Floor, GlobalQueue) of
    true ->
      {noreply, GlobalQueue};
    false ->
      Ref=make_ref(),
      broadcast:call_global_order({node(), Ref, {Floor, cab}}, nodes()),
      fsm_elevator:new_task({Ref, {Floor, cab}}),
      elevator_interface:set_order_button_light(whereis(driver_pid), cab, Floor, on),
      {noreply, [{node(), Ref, {Floor, cab}}|GlobalQueue]}
  end;

handle_cast({add_local_order, Floor, Type}, GlobalQueue) ->
  % Hall order added to all global queues, with a unique reference and assigned elevator and set lights.
  % If assigned to self, also added to local queue in FSM.
  % Only added if the same order is not already assigned in the global queue.
    case lists:keymember({Floor, Type}, 3, GlobalQueue) of
      true ->
        {noreply, GlobalQueue};
      false ->
        Ref=make_ref(),
        SelectedElevator = assign_and_broadcast(Ref, Floor, Type, normal),
        io:format("Assigned elevator ~p to order ~p~n",[SelectedElevator, Ref]),
        case node() of
          SelectedElevator ->
            fsm_elevator:new_task({Ref, {Floor, Type}}),
            elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, on);
          _ ->
            elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, on)
        end,
        {noreply, [{SelectedElevator, Ref, {Floor, Type}}|GlobalQueue]}
    end;

handle_cast({add_remote_order, {Elevator, Ref, {Floor, Type}}}, GlobalQueue) ->
  % Remote order added to global queue. Lights are turned on, unless its a remote cab order.
  % If the order is assigned to this elevator, then order is added to local FSM queue_empty.
    case node() of
      Elevator ->
        fsm_elevator:new_task({Ref, {Floor, Type}}),
        elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, on),
        {noreply, [{Elevator, Ref, {Floor, Type}}|GlobalQueue]};
      _ ->
        case Type of
            cab ->
                {noreply, [{Elevator, Ref, {Floor, Type}}|GlobalQueue]};
            _ ->
                elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, on),
                {noreply, [{Elevator, Ref, {Floor, Type}}|GlobalQueue]}
        end
      end;

handle_cast({order_finished, RefDone}, GlobalQueue) ->
  % Delete a locally executed order from all global queues
  % Keep sending until all elevators have recieved the message
    case lists:keyfind(RefDone, 2, GlobalQueue) of
      {_Elevator, Ref, {Floor, Type}} ->
        broadcast:delete_global_order(Ref),
        elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, off),
        re_send_order_finished(Ref),
        {noreply, lists:keydelete(Ref, 2, GlobalQueue)};
      false ->
        {noreply, GlobalQueue}
  end;

handle_cast({order_finished_remote, RefDone}, GlobalQueue) ->
  % Delete remote executed order. Unless cab order, turn off light also.
  case lists:keyfind(RefDone, 2, GlobalQueue) of
    {_Elevator, Ref, {_Floor, cab}} ->
      {noreply, lists:keydelete(Ref, 2, GlobalQueue)};

    {_Elevator, Ref, {Floor, Type}} ->
      elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, off),
      {noreply, lists:keydelete(Ref, 2, GlobalQueue)};

    false ->
      {noreply, GlobalQueue}
  end;

handle_cast({transfer_queue, From_Node}, GlobalQueue) ->
  % Copy global queue from From_Node
    NewQueue = rpc:call(From_Node, control_server, get_queue, []),
    {noreply, NewQueue++GlobalQueue};

handle_cast({redistribute, Node, SituationType}, GlobalQueue) ->
  % Redistribute Node's orders based on situation type : normal(another node disconnected) or motor_loss(redistribute own orders)
    NewGlobalQueue = redistribute_hall_orders(Node, GlobalQueue, SituationType),
    {noreply, NewGlobalQueue};

handle_cast(terminate, GlobalQueue) ->
  {stop, normal, GlobalQueue}.

handle_call(check_my_orders, _From, GlobalQueue) ->
  % Retrieve local elevator's assigned orders from global queue
    check_order(GlobalQueue),
    {reply, ok, GlobalQueue};

handle_call(get_queue, _From, GlobalQueue) ->
  % Get current global queue
    {reply, GlobalQueue,GlobalQueue};

handle_call(queue_empty, _From, GlobalQueue) ->
  % Return if global queue is empty
    case GlobalQueue of
      [] ->
          {reply,true,GlobalQueue};
      _ ->
          {reply,false,GlobalQueue}
      end.

handle_info({re_order_finished, RefDone}, GlobalQueue) ->
  % To handle short disconnections. Message is sent again RE_TIME(10).
  broadcast:delete_global_order(RefDone),
  {noreply, GlobalQueue}.

terminate(normal, GlobalQueue) ->
    io:format("Could not finish~n"),
    print_list(GlobalQueue),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

print_list([{Ref, {_Type, _Floor}}|T]) ->
  io:format("Order : ~p~n", [Ref]),
  print_list(T);
print_list([]) -> ok.

cab_already_ordered(Floor, GlobalQueue) ->
  case lists:keyfind({Floor, cab}, 3, GlobalQueue) of
    {Node, NRef, {NFloor, NType}} ->
      if Node =:= node() ->
        true;
      true ->
        cab_already_ordered(Floor, lists:delete({Node, NRef, {NFloor, NType}}, GlobalQueue))
      end;
    false ->
      false
  end.

re_send_order_finished(Ref) ->
  % Resend order_finished message if no response within RE_TIME(10) seconds
  erlang:send_after(?RE_TIME, whereis(control_server_pid), {re_order_finished, Ref}).

check_order([{Elevator, Ref, {Floor,Type}}|T])->
 % Check recursivley if an order in global queue is assigned to local elevator
    case Type of
        cab ->
            case node() of
                Elevator ->
                    fsm_elevator:new_task({Ref, {Floor, cab}}),
                    elevator_interface:set_order_button_light(whereis(driver_pid), cab, Floor, on),
                    check_order(T);
                _->
                    check_order(T)
            end;
        _ ->
            case node() of
                Elevator ->
                    fsm_elevator:new_task({Ref, {Floor, cab}}),
                    elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, on),
                    check_order(T);
                _->
                    elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, on),
                    check_order(T)
            end
end;

check_order([])->
    ok.

redistribute_hall_orders(Node, [{Elevator, Ref, {Floor,Type}}|T], SituationType) ->
  % Redistributes only Node's hall orders in global queue and returns updated global queue.
  % Deletes the order in all global queues, then broadcasts it again with same reference.
  % If SituationType = motor_loss, then order is not redistributed to self.
  % If SituationType = normal (node disconnected), order is redistributed to available nodes.
    case Node of
        Elevator ->
            case Type of
                cab ->
                  [{Elevator, Ref, {Floor,Type}}|redistribute_hall_orders(Node, T, SituationType)];
                _ ->
                  broadcast:delete_global_order(Ref),
                  SelectedElevator = assign_and_broadcast(Ref, Floor, Type, SituationType),
                  case node() of
                    SelectedElevator ->
                      fsm_elevator:new_task({Ref, {Floor, Type}}),
                      elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, on);
                    _ ->
                      elevator_interface:set_order_button_light(whereis(driver_pid), Type, Floor, on)
                  end,
                  io:format("Order ~p ~p from ~p redistributed to  ~p~n",[Floor, Type, Elevator, SelectedElevator]),
                  [{SelectedElevator, Ref, {Floor,Type}}|redistribute_hall_orders(Node, T, SituationType)]
              end;
        _ ->
          [{Elevator, Ref, {Floor,Type}}|redistribute_hall_orders(Node, T, SituationType)]
      end;

redistribute_hall_orders(_Node,[], _)->
    [].

select_elevator([H|T], Floor, Type) ->
  % Assigns an elevator to order {Floor, Type}.
  % First checks if any elevator is already at that floor
  % If no elevator at floor, get the optimal elevator from find_optimum_cost(CostList, ?NUM_FLOOR+3).
  case any_elev_ONFLOOR([H|T], Floor, Type) of
     {Node,_ElevState,_ElevFloor,_ElevDir}->
       io:format("Found an elevator ~p ONFLOOR\n", [Node]),
       Node;
     _->
        CostList = evaluate_state([H|T], Floor, Type),
        SelectedElevator = find_optimum_cost(CostList, ?NUM_FLOOR+3),
        SelectedElevator
  end.

evaluate_state([{Node,ElevState,ElevFloor,ElevDir}|T], Floor, Type) ->
  % COST FUNCTION: Returns a tuple list {Elevator, Points}, where the elevator
  % with highest point is most suitable for the hall orders
  case ElevState of
   idle ->
       Cost = (?NUM_FLOOR + 3) - abs(ElevFloor - Floor),
      [{Node,Cost}|evaluate_state(T,Floor,Type)];
   _ ->
      case Type of
          ElevDir ->
              case ElevDir of
                  hall_up->
                      if ElevFloor < Floor ->
                       Cost = (?NUM_FLOOR + 3) - abs(ElevFloor - Floor),
                       [{Node,Cost}|evaluate_state(T,Floor,Type)];
                       true->
                           Cost = 1,
                           [{Node,Cost}|evaluate_state(T,Floor,Type)]
                      end;
                  hall_down->
                      if ElevFloor > Floor ->
                       Cost = (?NUM_FLOOR + 3) - abs(ElevFloor - Floor),
                       [{Node,Cost}|evaluate_state(T,Floor,Type)];
                       true->
                           Cost = 1,
                           [{Node,Cost}|evaluate_state(T,Floor,Type)]
                   end
               end;
          _ ->
              case ElevDir of
                  hall_up->
                      if ElevFloor < Floor ->
                       Cost = (?NUM_FLOOR + 1) - abs(ElevFloor - Floor),
                       [{Node,Cost}|evaluate_state(T,Floor,Type)];
                       true->
                           Cost = 1,
                           [{Node,Cost}|evaluate_state(T,Floor,Type)]
                      end;
                  hall_down->
                      if ElevFloor > Floor ->
                       Cost = (?NUM_FLOOR + 1) - abs(ElevFloor - Floor),
                       [{Node,Cost}|evaluate_state(T,Floor,Type)];
                       true->
                           Cost = 1,
                           [{Node,Cost}|evaluate_state(T,Floor,Type)]
                   end
               end
         end
     end;

evaluate_state([], _Floor, _Type)->
   [].

 find_optimum_cost(_CostList, 0) ->
   % Should not occur, but implemented for safety
   % If no optimal elevator found, assign self.
   io:format("Error in find_optimum_cost"),
   node();

find_optimum_cost(CostList, Value) ->
  % Recursivley checks for an elevator in CostList
  % with highest value, and returns this.
  case lists:keyfind(Value, 2, CostList) of
      false ->
        find_optimum_cost(CostList, Value-1);
      {Node,_Cost} ->
          Node
  end.

any_elev_ONFLOOR([H|T], Floor, Type) ->
  %Recursivley checks if any elevators are in arrived or idle state in ordered floor.
  {Node,ElevState,ElevFloor,ElevDir} = H,
  case ElevFloor =:= Floor of
    false ->
        any_elev_ONFLOOR(T, Floor, Type);
    true ->
        case ElevState of
            arrived->
                {Node,ElevState,ElevFloor,ElevDir};
            idle->
                {Node,ElevState,ElevFloor,ElevDir};
            _->
                any_elev_ONFLOOR(T, Floor, Type)
            end
end;

any_elev_ONFLOOR([], _Floor, _Type) ->
  false.

assign_and_broadcast(Ref, Floor, Type, normal) ->
  % In NORMAL mode (a node is disconnected): Assigns order to one of the available nodes including
  % self and broadcasts the updated assigned order.If an elevator does not respond (may be disconnected),
  % the order is assigned and broadcasted until success.
  {NodesAvailabe, RemoteStates} = broadcast:get_all_states(),
  AllStates=[fsm_elevator:get_state()|RemoteStates],
  SelectedElevator = select_elevator(AllStates, Floor, Type),
  %io:format("Elevator: ~p~n", SelectedElevator),
  case broadcast:call_global_order({SelectedElevator, Ref, {Floor, Type}}, NodesAvailabe) of
    true ->
      assign_and_broadcast(Ref, Floor, Type, normal);
    false ->
      SelectedElevator
end;

assign_and_broadcast(Ref, Floor, Type, motor_loss) ->
  % In MOTOR_LOSS mode: Assigns order to one of the available nodes excluding self
  % and broadcasts the updated assigned order.
  {NodesAvailabe, RemoteStates} = broadcast:get_all_states(),
  SelectedElevator = select_elevator(RemoteStates, Floor, Type),
  %io:format("Elevator: ~p~n", SelectedElevator),
  case broadcast:call_global_order({SelectedElevator, Ref, {Floor, Type}}, NodesAvailabe) of
    true ->
      assign_and_broadcast(Ref, Floor, Type, motor_loss);
    false ->
      SelectedElevator
end.
