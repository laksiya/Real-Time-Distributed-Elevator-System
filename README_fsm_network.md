FSM_NETWORK
===============
The FSM will start running with either a tuple or a single variable “Port”
depending on whether or not the first node -the one which creates the network-
is required. Port is used to initialise `elevator_interface`.

In the `INIT` state, all the processes will be started. This means that the
elevator is completely functional by this time. If the node was started as the
first one, it will create a UDP broadcast socket and go into `BROADCAST` state.
In the other case, a UDP socket for listening will be created, and the process
will jump into `LISTEN`. In BROADCAST, the node will broadcast its own name
every period of time defined by `BROADCAST_TIME`.

In LISTEN state the node will fall into an infinite loop receiving information
from the outside. Once it has received the required information, it will try to
connect to the node the data came from. If succeed, the elevator will empty its
list of orders with `wait_until_idle()` and start the connection protocol.
![fsm_network](https://user-images.githubusercontent.com/42868962/55423202-ba840d00-557d-11e9-960f-c34d040aead6.png)


Network topology
----------------
Since every node has the same priority once it has been added to the network, some means are needed to handle the changes that could arise. When a new node wants to be part of the network, who pass him the current global queue? When a connection fails with one of the elevators, who is the one in charge of redistributing the orders?
In this case the solution is simple. Every node monitors another one while is being monitored by a previous elevator. The resulting topology is depicted in the figure:
![topology](https://user-images.githubusercontent.com/42868962/55423367-08991080-557e-11e9-8070-94a2524e0299.png)
Monitoring will mainly work as an alarm when something is wrong with the node pointed. If the node goes down, it will be instantly noticed by the previous elevator. Then the Disconnection Protocol takes place. In contrast, when the computer is disconnected from internet, it takes some time until the monitoring process fails. This time can be easily controlled by `TICK_TIME`.

Connection Protocol
-------------------
![Connection protocol](https://user-images.githubusercontent.com/42868962/55423375-0b940100-557e-11e9-8eb2-c6a564aa930f.png)
Suppose the network is already created. As we mentioned before, all its nodes are in BROADCAST mode. When a new node wakes up, it will start listening for all the incoming messages. Once the connection with the network has been stabilised `net_kernel:connect_node()` and the new node’s queue emptied, such elevator transfers the Global Queue calling the Control Server with `control_server:transfer_queue()`. Moreover, the Control Server is required to check which of the orders belong to the New Node `control_server:check_my_orders()`. Thanks to this functionality, no cab order is lost even though the elevator has been powered of, all the information is stored in the cloud. Then, the connecting node sends a request to the network to start monitoring and being monitored `request_monitor_network`. After doing some other rearrangements – `fsm_network:set_next_node` and  `fsm_network:get_monitoring_node` – the protocol is finished and the network established. During the process, `prev_node, monitoring_node and next_node` refresh their value
![connect](https://user-images.githubusercontent.com/42868962/55423383-10f14b80-557e-11e9-9fb5-de3164ba48be.png)
