{application, robot,
    [{description, "A Lego robot that optically follows a track"},
     {vsn, "0.1.0"},
     {modules, [robot_app, robot_sup, robot_fsm, brickpi]},
     {registered, [robot_sup, robot_fsm, brickpi]},
     {applications, [kernel, stdlib]},
     {mod, {robot_app, []}}
    ]
}.
