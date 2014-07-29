-module(robot_fsm).
-behaviour(gen_fsm).

%%API
-export([start_link/0, start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% timer
-export([handle_tick/0]).

%% states
-export([on_line/2, scanning/2, finish_turning/2, lost/2]).

-include("brickpi.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 50).
-define(SCANS_UNTIL_ON_LINE, 7).
-define(LEFT_MOTOR, motor_c).
-define(RIGHT_MOTOR, motor_b).
-define(LIGHT_SENSOR, port_1).
-define(LIGHT_SENSOR_TYPE, ?SENSOR_COLOR_RED).
-define(MAX_SCANS, 40).
-define(MOTOR_TIMEOUT, 250).

-define(MOTOR_RUN_SPEED, 100).
-define(MOTOR_SCAN_SPEED, 100).
-define(LEAVE_BLACK_THRESHOLD, 400).
-define(ENTER_BLACK_THRESHOLD, 380).

-record(state, {interval, timer, scan_direction, scan_length, scan_count}).

start_link() ->
    start_link([?DEFAULT_INTERVAL]).

start_link(Args) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Args, []).

init([Interval]) ->
    brickpi:enable_peripherals(#robot_settings{?LEFT_MOTOR=true, ?RIGHT_MOTOR=true, ?LIGHT_SENSOR=?LIGHT_SENSOR_TYPE}),
    brickpi:set_motor_speeds(#robot_motor_speeds{?LEFT_MOTOR=-?MOTOR_RUN_SPEED, ?RIGHT_MOTOR=-?MOTOR_RUN_SPEED, timeout=?MOTOR_TIMEOUT}),
    {ok, Timer} = timer:apply_interval(Interval, ?MODULE, handle_tick, []),
    {ok, on_line, #state{interval=Interval, timer=Timer}}.

handle_info(_Request, _StateName, State) ->
    {noreply, State}.

handle_event(stop, _StateName, State) ->
    timer:cancel(State#state.timer),
    {stop,normal,State}.

handle_sync_event(stop, _From, _StateName, State) ->
    timer:cancel(State#state.timer),
    {stop,normal,State}.

terminate(_Reason, _StateName, State) ->
    timer:cancel(State#state.timer),
    ok.

code_change(_OldVsn, _StateName, State, _Extra) ->
    timer:cancel(State#state.timer),
    {ok, Timer} = timer:apply_interval(State#state.interval, ?MODULE, handle_tick, []),
    {ok, State#state{timer=Timer}}.

on_line({data,Data}, State) ->
    if 
        Data#robot_data.?LIGHT_SENSOR#sensor_data.value > ?LEAVE_BLACK_THRESHOLD ->
            turn_left(),
            {next_state, scanning, State#state{scan_direction=left, scan_length=1, scan_count=1}};
        true ->
            go_forward(),
            {next_state, on_line, State}
    end.

scanning({data,Data}, State) ->
    if 
        Data#robot_data.?LIGHT_SENSOR#sensor_data.value < ?ENTER_BLACK_THRESHOLD ->
            {next_state, finish_turning, continue_turn(State)};

        State#state.scan_count >= ?MAX_SCANS ->
            stop_moving(),
            {next_state, lost, State};

        State#state.scan_count >= State#state.scan_length ->
            {next_state, scanning, reverse_turn(State)};
        true ->
            {next_state, scanning, continue_turn(State)}
    end.

finish_turning({data,Data}, State) ->
    Scan_Count = State#state.scan_count + 1,
    if 
        Data#robot_data.?LIGHT_SENSOR#sensor_data.value > ?LEAVE_BLACK_THRESHOLD ->
            turn_left(),
            {next_state, scanning, State#state{scan_direction=left, scan_length=1, scan_count=1}};

        Scan_Count >= ?SCANS_UNTIL_ON_LINE ->
            go_forward(),
            {next_state, on_line, State};

        true ->
            continue_turn(State),
            {next_state, finish_turning, continue_turn(State)}
    end.

lost({data,Data}, State) ->
    if 
        Data#robot_data.?LIGHT_SENSOR#sensor_data.value < ?ENTER_BLACK_THRESHOLD ->
            go_forward(),
            {next_state, on_line, State};
        true ->
            stop_moving(),
            {next_state, lost, State}
    end.

handle_tick() ->
    {ok, Data} = brickpi:get_data(),
    gen_fsm:send_event(?MODULE, {data,Data}).

opposite_dir(left) -> right;
opposite_dir(right) -> left.

continue_turn(State) ->
    case State#state.scan_direction of
        left ->
            turn_left();
        right ->
            turn_right()
    end,
    State#state{scan_count=State#state.scan_count+1}.

reverse_turn(State) ->
    case State#state.scan_direction of
        left ->
            turn_right();
        right ->
            turn_left()
    end,
    State#state{scan_direction=opposite_dir(State#state.scan_direction),
        scan_length=State#state.scan_length+3, scan_count=0}.

go_forward() ->
    brickpi:set_motor_speeds(#robot_motor_speeds{?LEFT_MOTOR=-?MOTOR_RUN_SPEED, ?RIGHT_MOTOR=-?MOTOR_RUN_SPEED, timeout=?MOTOR_TIMEOUT}).

turn_left() ->
    brickpi:set_motor_speeds(#robot_motor_speeds{?LEFT_MOTOR=-?MOTOR_RUN_SPEED, ?RIGHT_MOTOR=?MOTOR_RUN_SPEED, timeout=?MOTOR_TIMEOUT}).

turn_right() ->
    brickpi:set_motor_speeds(#robot_motor_speeds{?LEFT_MOTOR=?MOTOR_RUN_SPEED, ?RIGHT_MOTOR=-?MOTOR_RUN_SPEED, timeout=?MOTOR_TIMEOUT}).

stop_moving() ->
    brickpi:set_motor_speeds(#robot_motor_speeds{?LEFT_MOTOR=0, ?RIGHT_MOTOR=0}).
