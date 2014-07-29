-module(brickpi).
-behaviour(gen_server).
%%API
-export([start_link/0, start_link/1, enable_peripherals/1, set_motor_speeds/1, get_data/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_EXTPROG, "brickpi_erlport").

-define(ENABLE_PERIPHERALS, 1).
-define(SET_MOTOR_SPEEDS, 2).
-define(GET_DATA, 3).

-include("brickpi.hrl").
-record(state, {port}).

start_link(ExtProg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ExtProg], []).

start_link() ->
    start_link(?DEFAULT_EXTPROG).

enable_peripherals(Settings) ->
    gen_server:cast(?SERVER, {enable_peripherals, Settings}).

set_motor_speeds(Settings) ->
    gen_server:cast(?SERVER, {set_motor_speeds, Settings}).

get_data() ->
    gen_server:call(?SERVER, get_data).

init([ExtProg]) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtProg}, [{packet, 2}]),
    {ok, #state{port = Port}}.

handle_call(get_data, _From, State) ->
    {reply, {ok, retrieve_data(State#state.port)}, State}.

handle_cast({enable_peripherals, Settings}, State) ->
    set_peripherals(State#state.port, Settings),
    {noreply,State};
handle_cast({set_motor_speeds, Settings}, State) ->
    set_speeds(State#state.port, Settings),
    {noreply,State}.

handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {port_terminated, Reason}, State};
handle_info(_Request, State) ->
    {noreply, State}.

terminate({port_terminated, _Reason}, State) ->
    port_close(State#state.port),
    ok;
terminate(_Reason, State) ->
    port_close(State#state.port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


retrieve_data(Port) ->
    Port ! { self(), {command, <<?GET_DATA>>}},
    receive
        {Port, {data, Robot_Data}} ->
            decode_robot_data(binary:list_to_bin(Robot_Data))
    end.

set_peripherals(Port, Settings) ->
    Port ! { self(), {command, encode_set_peripherals(Settings)}}.

set_speeds(Port, Settings) ->
    Port ! { self(), {command, encode_set_speeds(Settings)}}.

encode_set_peripherals(Settings) -> 
    MotorA = encode_boolean(Settings#robot_settings.motor_a),
    MotorB = encode_boolean(Settings#robot_settings.motor_b),
    MotorC = encode_boolean(Settings#robot_settings.motor_c),
    MotorD = encode_boolean(Settings#robot_settings.motor_d),
    Port1 = encode_number(Settings#robot_settings.port_1),
    Port2 = encode_number(Settings#robot_settings.port_2),
    Port3 = encode_number(Settings#robot_settings.port_3),
    Port4 = encode_number(Settings#robot_settings.port_4),
    <<?ENABLE_PERIPHERALS, MotorA:8, MotorB:8, MotorC:8, MotorD:8, Port1:8, Port2:8, Port3:8, Port4:8>>.

encode_set_speeds(Settings) ->
    MotorA = encode_number(Settings#robot_motor_speeds.motor_a),
    MotorB = encode_number(Settings#robot_motor_speeds.motor_b),
    MotorC = encode_number(Settings#robot_motor_speeds.motor_c),
    MotorD = encode_number(Settings#robot_motor_speeds.motor_d),
    Timeout = encode_number(Settings#robot_motor_speeds.timeout),
    <<?SET_MOTOR_SPEEDS, MotorA:16, MotorB:16, MotorC:16, MotorD:16, Timeout:32>>.

decode_robot_data(<<
    Port1_Value:32/big-signed-integer,Port1_Extra1:32/big-signed-integer,Port1_Extra2:32/big-signed-integer,Port1_Extra3:32/big-signed-integer,Port1_Extra4:32/big-signed-integer,
    Port2_Value:32/big-signed-integer,Port2_Extra1:32/big-signed-integer,Port2_Extra2:32/big-signed-integer,Port2_Extra3:32/big-signed-integer,Port2_Extra4:32/big-signed-integer,
    Port3_Value:32/big-signed-integer,Port3_Extra1:32/big-signed-integer,Port3_Extra2:32/big-signed-integer,Port3_Extra3:32/big-signed-integer,Port3_Extra4:32/big-signed-integer,
    Port4_Value:32/big-signed-integer,Port4_Extra1:32/big-signed-integer,Port4_Extra2:32/big-signed-integer,Port4_Extra3:32/big-signed-integer,Port4_Extra4:32/big-signed-integer,
    MotorA_Pos:32/big-signed-integer, MotorB_Pos:32/big-signed-integer, MotorC_Pos:32/big-signed-integer, MotorD_Pos:32/big-signed-integer>>) ->
    #robot_data{motor_a=MotorA_Pos, motor_b=MotorB_Pos, motor_c=MotorC_Pos, motor_d=MotorD_Pos,
        port_1=#sensor_data{value=Port1_Value, extra=[Port1_Extra1, Port1_Extra2, Port1_Extra3, Port1_Extra4]},
        port_2=#sensor_data{value=Port2_Value, extra=[Port2_Extra1, Port2_Extra2, Port2_Extra3, Port2_Extra4]},
        port_3=#sensor_data{value=Port3_Value, extra=[Port3_Extra1, Port3_Extra2, Port3_Extra3, Port3_Extra4]},
        port_4=#sensor_data{value=Port4_Value, extra=[Port4_Extra1, Port4_Extra2, Port4_Extra3, Port4_Extra4]}}.

encode_boolean(true) ->
    1;
encode_boolean(false) ->
    0;
encode_boolean(undefined) ->
    0.

encode_number(undefined) ->
    0;
encode_number(Value) ->
    Value.
