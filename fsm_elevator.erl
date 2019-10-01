-module(fsm_elevator).
-behaviour(gen_fsm).


-export([start/0, start_link/0, new_task/1, get_state/0, set_current/1, kill/0]).
-export([init/1, wait/2, idle/2, busy/2, arrived/2, handle_event/3, handle_sync_event/4, handle_info/3, code_change/4, terminate/3]).
-record(info, {queue=[], state, current_floor, direction, ref_go_to_work_after, ref_watchdog_cant_move}).
-define(NOT_MOVING_TIME, 5000).
% Received orders have the structure {Reference, {Floor, Type}}
% State passed when get_state() has the structure {node(), State, CurrentFloor, Direction}

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% MODULE API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  gen_fsm:start(?MODULE, [], []).

start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

new_task(NewOrder) ->
  gen_fsm:send_event(whereis(fsm_elevator_pid), {new_task, NewOrder}).

get_state() ->
  gen_fsm:sync_send_all_state_event(whereis(fsm_elevator_pid), get_state).

set_current(NewFloor) ->
  gen_fsm:send_event(whereis(fsm_elevator_pid), {set_current_floor, NewFloor}).

kill() ->
  gen_fsm:sync_send_all_state_event(whereis(fsm_elevator_pid), terminate).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CALLBACK FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
  %Elevator is initialised going up
  elevator_interface:set_door_open_light(whereis(driver_pid), off),
  {ok, wait, go_up(#info{state=wait})}.

wait({set_current_floor, NewFloor}, S=#info{}) ->
  %waits until it reaches a new floor
  elevator_interface:set_floor_indicator(whereis(driver_pid), NewFloor),
  {next_state, idle, stop(S#info{current_floor=NewFloor, state=idle}, to_idle)}.


idle({new_task, {Ref, {NewGoal, Type}}}, S=#info{current_floor=CurrFloor}) ->
  % If the new task matches the current floor, go to arrive. Otherwhise go to the right direction
  if (NewGoal - CurrFloor) =:= 0 ->
      ok = control_server:order_finished(Ref),
      elevator_interface:set_door_open_light(whereis(driver_pid), on),
      {next_state, arrived, S#info{ref_go_to_work_after=work_again_after(), state=arrived}};
    (NewGoal - CurrFloor) > 0 ->
      {next_state, busy, go_up(S#info{queue=[{Ref, {NewGoal, Type}}], state=busy, ref_watchdog_cant_move=watchdog_cant_move()})};
    (NewGoal - CurrFloor) < 0 ->
      {next_state, busy, go_down(S#info{queue=[{Ref, {NewGoal, Type}}], state=busy, ref_watchdog_cant_move=watchdog_cant_move()})}
  end.

busy({set_current_floor, NewFloor}, S=#info{queue=Q, direction=Dir, ref_watchdog_cant_move=RefWATCHDOG}) ->
  %When new floor is received, cancel watchog_cant_move. If I have to stop, go to arrived. Otherwhise, keep driving.
  gen_fsm:cancel_timer(RefWATCHDOG),
  elevator_interface:set_floor_indicator(whereis(driver_pid), NewFloor),
  case local_queue:should_stop(Q, NewFloor, Dir) of
      true ->
        stop(S, to_arrived),
        elevator_interface:set_door_open_light(whereis(driver_pid), on),
        {next_state, arrived, S#info{queue=acknowledge_orders(NewFloor, up, Q), current_floor=NewFloor, ref_go_to_work_after=work_again_after(), state=arrived}};
     false ->
         {next_state, busy, S#info{current_floor=NewFloor, state=busy, ref_watchdog_cant_move=watchdog_cant_move()}}
  end;

busy({new_task, NewOrder}, S=#info{queue=Q}) ->
  %Add new task to the queue
    {next_state, busy, S#info{queue=[NewOrder|Q], state=busy}};

busy(cant_move, S=#info{queue=Q}) ->
  %Function triggered by watchdog_cant_move. Get rid of hall orders and keep cab orders
  control_server:redistribute_hall_orders(node(), motor_loss),
  NewQ = get_cab_orders(Q),
  {next_state, busy, S#info{queue=NewQ, ref_watchdog_cant_move=watchdog_cant_move()}}.

arrived({new_task, {Ref, {NewGoal, Type}}}, S=#info{queue=Q, current_floor=CurrFloor, ref_go_to_work_after=RefTimer}) ->
    if (NewGoal - CurrFloor) =:= 0 ->
            gen_fsm:cancel_timer(RefTimer),
            control_server:order_finished(Ref),
            {next_state, arrived, S#info{ref_go_to_work_after=work_again_after(), state=arrived}};
        true ->
            {next_state, arrived, S#info{queue=[{Ref, {NewGoal, Type}}|Q], state=arrived}}
    end;

arrived(go_to_work, S=#info{queue=Q, current_floor=CurrFloor, direction=Dir}) ->
  %Function triggered by the go_to_work internal signal. Compute next direction or go to idle if there is not any order left
    elevator_interface:set_door_open_light(whereis(driver_pid), off),
    case local_queue:next_direction(Q, CurrFloor, Dir) of
        hall_up ->
            {next_state, busy, go_up(S#info{state=busy, ref_watchdog_cant_move=watchdog_cant_move()})};
        hall_down ->
            {next_state, busy, go_down(S#info{state=busy, ref_watchdog_cant_move=watchdog_cant_move()})};
        idle ->
            {next_state, idle, stop(S#info{state=idle}, to_idle)}
    end.

handle_sync_event(get_state, _From, State, S=#info{}) ->
  {reply, {node(), S#info.state, S#info.current_floor, S#info.direction}, State, S};

handle_sync_event(terminate, _From, State, S=#info{}) ->
  {stop, normal, {ok, S}, State}.

%handle_info(timeout, _, S=#info{}) ->
%  go_up(S),
%  {next_state, wait, S};

%%%%%%%%%% DEFAULT CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%
handle_event( _, State, StateData) ->
  {next_state, State, StateData}.

handle_info(Msg, State, S=#info{}) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {next_state, State, S}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

terminate(normal, _State, S) ->
  ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Private functions
go_up(S=#info{}) ->
  elevator_interface:set_motor_direction(whereis(driver_pid), up),
  S#info{direction=hall_up}.

go_down(S=#info{}) ->
  elevator_interface:set_motor_direction(whereis(driver_pid), down),
  S#info{direction=hall_down}.

stop(S=#info{}, to_idle) ->
  elevator_interface:set_motor_direction(whereis(driver_pid), stop),
  S#info{direction=hall_up}; %Default direcction. Used by next_direction in case the elevator goes from idle to arrived.
stop(S=#info{}, to_arrived) ->
  elevator_interface:set_motor_direction(whereis(driver_pid), stop),
  S. %The diference with the function avobe is that we don't want to lose the direction we were driving

work_again_after() ->
  %work_again_after signal sent to arrived
    gen_fsm:send_event_after(5000, go_to_work).

watchdog_cant_move() ->
  gen_fsm:send_event_after(?NOT_MOVING_TIME, cant_move).

get_cab_orders([]) ->
  [];
get_cab_orders([{Reference, {Floor, Type}}|T]) ->
  %Take cab orders from the list
  case Type of
    cab ->
      [{Reference, {Floor, Type}}|get_cab_orders(T)];
    _ ->
      get_cab_orders(T)
  end.

acknowledge_orders(CurrFloor, up, Q) ->
  %Used to acknowledge several orders within the same floor
    case lists:keysearch({CurrFloor, hall_up}, 2, Q) of
        false ->
            acknowledge_orders(CurrFloor, down, Q);
        {value, {Ref, {Floor, Type}}} ->
            control_server:order_finished(Ref),
            acknowledge_orders(CurrFloor, up, lists:delete({Ref, {Floor, Type}}, Q))
    end;
acknowledge_orders(CurrFloor, down, Q) ->
    case lists:keysearch({CurrFloor, hall_down}, 2, Q) of
        false ->
            acknowledge_orders(CurrFloor, cab, Q);
        {value, {Ref, {Floor, Type}}} ->
            control_server:order_finished(Ref),
            acknowledge_orders(CurrFloor, down, lists:delete({Ref, {Floor, Type}}, Q))
    end;
acknowledge_orders(CurrFloor, cab, Q) ->
    case lists:keysearch({CurrFloor, cab}, 2, Q) of
        false ->
            Q;
        {value, {Ref, {Floor, Type}}} ->
            control_server:order_finished(Ref),
            acknowledge_orders(CurrFloor, cab, lists:delete({Ref, {Floor, Type}}, Q))
    end.
