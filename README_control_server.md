CONTROL_SERVER
==============
The Control Server is the brain of the elevator, it constantly stores the Queue for the orders that have not been executed yet. To 

![control_server](https://user-images.githubusercontent.com/42868962/55439016-89b6ce80-55a3-11e9-821d-8257cd42e6ea.png)

Basic operation (Local view)
----------------------------
when a button is pushed a signal arrrives in the control server from one of the spawned `check_button` in `driver.erl`. The function it uses for such request is `control_server:add_order(Floor, Type)`. The order is ignored if it was already in the queue. If not, the message receives a reference and is sent to the private function `assign_and_broadcast()`. This function will compute the best suitable elevator to answer the order. It proceeds as follows:
* It asks about the states of the elevators within the network, including itself. Function `broadcast:get_states()` returns both the States and Nodes which where available while sampling.
* Then, the cost function computes the most suitable Elevator to perform the task.
* Finally, the message is sent by `broadcast:call_global_order()`using as the parameter the list of nodes availabes in step one. In case the assign node doesn't respond to the call, the whole process sarts again from the beginning. In this way, no order is lost even though a network failture has occurred while the process started.
To acknowledge a message from  FMS_elevator, the function `control_server:order_finished(Ref)` is called. The orders is automatically deleted locally and remotely via `broadcast:delete_global_order(Ref)`. As it may be noticed, this is a `cast` function since it is not needed a response from the outside. In the worst case, the order will be executed twice. As an extra feature, a timer is started from the time the acknowledgment is finished. The callback function `handle_info()` will send a second acknowledgment after `RE_TIME` has pased and the signal has been send from the private function `re_send_order_finished()`. This feature is useful when the network is not reliable and sporadic and quick disconnections occur.

Basic operation (Remote view)
----------------------------
It is basically the same behaviour as descrive above. New tasks and acknowledgments arrive through `control_server:add_remote_orders()` and `control_server:order_finished(remote)`.

Connection Protocol
-------------------
As described in `README_fsm_network.md`, fsm_network call some functions from control server while connecting to the network.
The fist funtion in use is `control_server:queue_empty()`. As the name suggests, it returns whether the Queue is empty or not. It is included in a recursive private funcion contained in fsm_network , `wait_until_idle` which waits until all the tasks have been executed to actually connect to the net. Due to the fact that the elevator is completely functional from the beginning, we must somehow make sure that the list is empty before taking the Global Queue.

To take the Global Queue `control_server:transfer_queu(FromNode)` is called by fsm_network. At the same time, the Control Server from the new node will contact with the Control Server from the node in the network (FromNode) via `control_server:get_queue()`. The latter node will return the Global Queue and `control_server:check_my_orders()` will be in charge of sending the orders assigned to itself to the fsm_elevator. Note that this is for not losing the cab orders.  

Dissconection Protocol
----------------------
This is the second process fsm_network refers to. When one of the nodes is missing, it is required to redistribute all its assigned hall orders, while the cab orders are kept in Global Queue until the missing node wakes up again.`control_server:redistribute_hall_orders(FromNode, normal)` will be called by fsm_network in case FromNode is missing and such fsm_network was monitoring it. All the orders from such node are redistributed and sent with the same Reference. Thus, every order must be overwritten. The sequence `broadcast:delete_global_order(Ref)` -> `assign_and_broadcast` is used for such purpose.   
