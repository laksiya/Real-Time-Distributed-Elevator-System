FSM_ELEVATOR
============
![fsm_elevator](https://user-images.githubusercontent.com/42868962/55439020-8d4a5580-55a3-11e9-9372-e3b6926b6b97.png)
As the figure depicts, the FSM present three main states, `IDLE`, `BUSY` and `ARRIVED`. After starting, the elevator is sent up and goes to
the `WAIT` state. As soon as it reaches a new floor, it goes to IDLE. This is basically to allocate the cabin in one of the floors.
The number of floor is set by the API `fsm_elevator:set_current(NewFloor)` which connects the FSM with the elevator interface through the
function `driver:read_sensor()` in `driver.erl`. 

`fsm_elevator:new_task(NewOrder)` is the function with which Control Server will send new tasks to the FSM. This tasks will be added to the
queue defined in the record `#info{}` along with some other information discussed below.

When the elevator is in IDLE state, a new order that matches the current floor will send the elevator to ARRIVED. In this state, the
elevator opens its doors and computes the new direction to go if there are tasks in the queue. The time the elevator remains in such state
is controlled by `work_again_after()` which sends an internal signal when the time is up. It has a reference which is defined in the record
mentioned before. In case a new button is pushed at the same floor when the elevator is on ARRIVED, the timer is reset.

From IDLE and pushing a button not at the same floor, the elevator will move either downwards or upwards. After reaching a new floor, the
callback function `busy({set_current_floor, NewFloor})` will call `local_queue:should_stop()`. In case there is an order at that floor, and
matches with the direction of the elevator, the system will stop, acknowledge all the possible orders, and jump to ARRIVED.
After receiving the signal `go_to_work` the FSM will call the function `local_queue:next_direction`. The result will either be “you should
go up” (hall_up), “you should go down” (hall_down), or “go to idle” (idle).

Finally, it is important to ensure that everything is working as expected and the elevator goes from one floor to another within an
adequate period of time. Every time the elevator goes to BUSY from any of the two possible states (IDLE or ARRIVED) a counter is set. Note
that this time is defined on the top by `NOT_MOVING_TIME`. When the elevator reaches a new floor, the counter is reset by the sequence
`gen_fsm:cancel_timer(RefWATCHDOG)`, `watchdog_cant_move()`. If the time is up, an internal message `cant_move` is sent. Then, the FSM will
call the function mentioned in README_control_server `control_server:redistribute_hall_orders(node(), motor_loss)` with the only difference
that the function will not take into account the state of the local elevator and thus any task will be reassigned to it again. Only the cab
orders will be kept in the queue and the watchdog will be set again. It might be mentioned that during this time, the elevator keeps taking
new orders coming from both the outside and locally. However, once the time it gets will of all the orders that are not cab. When the
problem is solved, the elevator will start working as usual and finish its cab orders.  

