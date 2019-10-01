Elevator Project
================

![](https://raw.github.com/klasbo/TTK4145/master/Project/ElevatorHardware.jpg)

Summary
-------
Create software for controlling `n` elevators working in parallel across `m` floors.


Main requirements
-----------------
* No orders are lost

* Multiple elevators should be more efficient than one

* An individual elevator should behave sensibly and efficiently

* The lights and buttons should function as expected

Further description of the project assignment may be found in `assignment.md`.

How to start the Real Time Elevator system
---------------------

Download and compile all files in Erlang shell. The first node should be initialized with `start:node(Port, First)`. All other elevators after this, also all reinitializations after termination, may be started with
`start:node(Port)`

If used with simulator, the port `15657` should be set to the port of the simulator.

It is recommended to run this program on a Linux OS.

Additional resources
--------------------

Information about every module is found in the folder `README_modules`. It is recommended to read through such files before or while the code review. Pictures and brief explanations for the most important functions and behaviours have been included.

Go to [the project resources repository](https://github.com/TTK4145/Project-resources) to find more resources for doing the project. This information is not required for the project, and is therefore maintained separately.
