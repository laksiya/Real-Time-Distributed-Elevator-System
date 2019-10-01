
-module(driver).

-export([buttons_init/0, read_sensor/2]).

buttons_init() ->
  %Spawn one process for every button
  [spawn(fun() -> check_button(N, Type) end) || N <- lists:seq(1,2), Type <- [hall_up, hall_down]],
  [spawn(fun() -> check_button(N, cab) end) || N <- lists:seq(0,3)],
  spawn(fun() -> check_button(3, hall_down) end),
  spawn(fun() -> check_button(0, hall_up) end).

check_button(N, Type) ->
  %Initialize the button by turning it off, then recursivley check the button's state
    elevator_interface:set_order_button_light(whereis(driver_pid), Type, N, off),
    check_button(N, Type, 0).

check_button(N, Type, State) ->
  %Check button state every 200 ms. Adds local order on rising edge.
    timer:sleep(200),
    case elevator_interface:get_order_button_state(whereis(driver_pid), N, Type) of
  0 ->
    check_button(N, Type, 0);
  1 ->
      case State of %To not send the same message more than once in case it is pushed for a long time
    0 ->
      control_server:add_local_order(N, Type),
      check_button(N, Type, 1);
    1 ->
      check_button(N, Type, 1)
    end
  end.

read_sensor(ready, Pid) ->
  %Checks sensor every 20 ms. Sets current floor light if NewFloor registered.
  timer:sleep(20),
  case elevator_interface:get_floor_sensor_state(whereis(driver_pid)) of
    between_floors ->
      read_sensor(ready, Pid);
    NewFloor ->
      fsm_elevator:set_current(NewFloor),
      read_sensor(wait, Pid)
  end;

read_sensor(wait, Pid) ->
  %When sensor registers between_floors, start reading sensor again.
  timer:sleep(20),
  case elevator_interface:get_floor_sensor_state(whereis(driver_pid)) of
    between_floors ->
      read_sensor(ready, Pid);
    _CurrFloor ->
      read_sensor(wait, Pid)
end.
