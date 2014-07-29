-module(robot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    BrickPiService = { brickpi, {brickpi, start_link, []},
        permanent, 2000, worker, [brickpi]},
    Robot_FSM = { robot_fsm, {robot_fsm, start_link, []},
        permanent, 2000, worker, [robot_fsm, brickpi]},
    Children = [ BrickPiService, Robot_FSM],
    Restart_Strategy = {rest_for_one, 1, 2},
    {ok, {Restart_Strategy, Children}}.
