-module(elevator_interface).

-behaviour(gen_server).

-export([start/0, start/2, stop/1]).
-export([set_motor_direction/2, set_order_button_light/4, set_floor_indicator/2, set_door_open_light/2, set_stop_button_light/2, get_order_button_state/3, get_floor_sensor_state/1, get_stop_button_state/1, get_obstruction_switch_state/1]).
-export([init/1, handle_cast/2, handle_call/3]).

-define(CALL_TIMEOUT, 1000).

start() ->
    gen_server:start(?MODULE, [{127,0,0,1}, 15657], []).


start(Address, Port) ->
    gen_server:start(?MODULE, [Address, Port], []).

stop(Pid) ->
    gen_server:stop(Pid).

init([Address, Port]) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, [{active, false}]),
    {ok, Socket}.

% Direction can be up/down/stop
set_motor_direction(Pid, Direction) ->
    gen_server:cast(Pid, {set_motor_direction, Direction}).

% ButtonType can be hall_up/hall_down/cab
% State can be on/off
set_order_button_light(Pid, ButtonType, Floor, State) ->
    gen_server:cast(Pid, {set_order_button_light, ButtonType, Floor, State}).

set_floor_indicator(Pid, Floor) ->
    gen_server:cast(Pid, {set_floor_indicator, Floor}).

% State can be on/off
set_door_open_light(Pid, State) ->
    gen_server:cast(Pid, {set_door_open_light, State}).

% State can be on/off
set_stop_button_light(Pid, State) ->
    gen_server:cast(Pid, {set_stop_button_light, State}).

get_order_button_state(Pid, Floor, ButtonType) ->
    gen_server:call(Pid, {get_order_button_state, Floor, ButtonType}).

get_floor_sensor_state(Pid) ->
    gen_server:call(Pid, get_floor_sensor_state).

get_stop_button_state(Pid) ->
    gen_server:call(Pid, get_stop_button_state).

get_obstruction_switch_state(Pid) ->
    gen_server:call(Pid, get_obstruction_switch_state).



%% Callback implementation

handle_cast({set_motor_direction, up}, Socket) ->
    gen_tcp:send(Socket, [1, 1, 0, 0]),
    {noreply, Socket};
handle_cast({set_motor_direction, down}, Socket) ->
    gen_tcp:send(Socket, [1, 255, 0, 0]),
    {noreply, Socket};
handle_cast({set_motor_direction, stop}, Socket) ->
    gen_tcp:send(Socket, [1, 0, 0, 0]),
    {noreply, Socket};

handle_cast({set_order_button_light, hall_up, Floor, on}, Socket) ->
    gen_tcp:send(Socket, [2, 0, Floor, 1]),
    {noreply, Socket};
handle_cast({set_order_button_light, hall_up, Floor, off}, Socket) ->
    gen_tcp:send(Socket, [2, 0, Floor, 0]),
    {noreply, Socket};
handle_cast({set_order_button_light, hall_down, Floor, on}, Socket) ->
    gen_tcp:send(Socket, [2, 1, Floor, 1]),
    {noreply, Socket};
handle_cast({set_order_button_light, hall_down, Floor, off}, Socket) ->
    gen_tcp:send(Socket, [2, 1, Floor, 0]),
    {noreply, Socket};
handle_cast({set_order_button_light, cab, Floor, on}, Socket) ->
    gen_tcp:send(Socket, [2, 2, Floor, 1]),
    {noreply, Socket};
handle_cast({set_order_button_light, cab, Floor, off}, Socket) ->
    gen_tcp:send(Socket, [2, 2, Floor, 0]),
    {noreply, Socket};

handle_cast({set_floor_indicator, Floor}, Socket) ->
    gen_tcp:send(Socket, [3, Floor, 0, 0]),
    {noreply, Socket};

handle_cast({set_door_open_light, on}, Socket) ->
    gen_tcp:send(Socket, [4, 1, 0, 0]),
    {noreply, Socket};
handle_cast({set_door_open_light, off}, Socket) ->
    gen_tcp:send(Socket, [4, 0, 0, 0]),
    {noreply, Socket};

handle_cast({set_stop_button_light, on}, Socket) ->
    gen_tcp:send(Socket, [5, 1, 0, 0]),
    {noreply, Socket};
handle_cast({set_stop_button_light, off}, Socket) ->
    gen_tcp:send(Socket, [5, 0, 0, 0]),
    {noreply, Socket}.


handle_call({get_order_button_state, Floor, hall_up}, _From, Socket) ->
    gen_tcp:send(Socket, [6, 0, Floor, 0]),
    {ok, [6, State, 0, 0]} = gen_tcp:recv(Socket, 4, ?CALL_TIMEOUT),
    {reply, State, Socket};
handle_call({get_order_button_state, Floor, hall_down}, _From, Socket) ->
    gen_tcp:send(Socket, [6, 1, Floor, 0]),
    {ok, [6, State, 0, 0]} = gen_tcp:recv(Socket, 4, ?CALL_TIMEOUT),
    {reply, State, Socket};
handle_call({get_order_button_state, Floor, cab}, _From, Socket) ->
    gen_tcp:send(Socket, [6, 2, Floor, 0]),
    {ok, [6, State, 0, 0]} = gen_tcp:recv(Socket, 4, ?CALL_TIMEOUT),
    {reply, State, Socket};

handle_call(get_floor_sensor_state, _From, Socket) ->
    gen_tcp:send(Socket, [7, 0, 0, 0]),
    State = case gen_tcp:recv(Socket, 4, ?CALL_TIMEOUT) of
			   {ok, [7, 0, _, 0]} -> between_floors;
			   {ok, [7, 1, Floor, 0]} -> Floor
		       end,
    {reply, State, Socket};

handle_call(get_stop_button_state, _From, Socket) ->
    gen_tcp:send(Socket, [8, 0, 0, 0]),
    StopButtonState = case gen_tcp:recv(Socket, 4, ?CALL_TIMEOUT) of
			   {ok, [8, 0, 0, 0]} -> inactive;
			   {ok, [8, 1, 0, 0]} -> active
			end,
    {reply, StopButtonState, Socket};

handle_call(get_obstruction_switch_state, _From, Socket) ->
    gen_tcp:send(Socket, [9, 0, 0, 0]),
    ObstructionState = case gen_tcp:recv(Socket, 4, ?CALL_TIMEOUT) of
			   {ok, [9, 0, 0, 0]} -> inactive;
			   {ok, [9, 1, 0, 0]} -> active
			end,
    {reply, ObstructionState, Socket}.
