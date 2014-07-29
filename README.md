# Erlang+BrickPi Robot Application

## Overview
This is an application that controls a Lego robot powered by a Raspberry Pi
with the [BrickPi add-on board](http://www.dexterindustries.com/BrickPi/).


## Erlang BrickPi Interface
The brickpi process uses the Erlang port mechanism to communicate with the
Brick Pi. To use this process, you start it like this:
```erlang
brickpi:start_link().
```
The default start\_link function assumes that the brickpi\_erlport executable
is somewhere in the path. You can also specify the path to the program
as an argument to start\_link:
```erlang
brickpi:start_link("/home/mark/brickpi/brickpi_erlport").
```

The brickpi process uses several data structures and constants that are
defined in the brickpi.hrl file, make sure you include it with:
```erlang
-include("brickpi.hrl").
```

### Initializing peripherals
Before you can read sensors or run motors you need to specify which motors
should be active and what type of sensor is connected to each port. To do
this, call the brickpi:enable\_peripherals function with a \#robot\_settings
structure. For example, to enable motors in ports C and D, and use a red light
sensor in port 1, do:
```erlang
brickpi:enable_peripherals(#robot_settings(motor_b=true, motor_c=true, port_1=?SENSOR_COLOR_RED).
```

### Motors
To set the speeds of the motors, call brickpi:set\_motor\_speeds with a
\#robot\_motor\_speeds structure. You are allowed to set a timeout for the
motors and I have found it very helpful to always set a timeout so I don't
have a runaway robot. The timeout is specified in milliseconds.
```erlang
brickpi:set_motor_speeds(#robot_motor_speeds(motor_b=100, motor_c=-100, timeout=250).
```

### Sensors
To read the values of the sensors and also the motor rotations, call
brickpi:get\_data. It returns a \#robot\_data structure where
motor\_a, motor\_b, motor\_c, and motor\_d contain the rotation sensor values
of each motor, and port\_1, port\_2, port\_3, and port\_4 contain \#sensor\_data
structures for each port.

A \#sensor\_data structure contains two fields - value, which is the main integer value
of the sensor, and extra, which is a list of four integers that contain any
extra sensor data. When using the full color sensor, for example, the extra
list contains the RGB values.

## Example program
The example program consists of the brickpi module, as well as a robot\_fsm
module that contains the robot logic, a robot\_sup containing a supervisor
module that starts the brickpi and robot\_fsm processes and makes sure they
stay running, and the robot\_app application that starts the supervisor.

## Example Robot
![Lego Robot with Brick Pi controller](http://www.wutka.com/lego_robot.jpg)

The robot that this example program drives has two motors and a red light
sensor (I had some trouble getting the full light sensor to work). Because
of the way I built the robot, the motors are backwards, so you have to use
a negative motor speed to go forwards.

The light sensor thresholds were numbers I came up with after experimenting
with the printed test track that came with my Lego NXT set. They may not
work well under all lighting conditions.

Here's the robot in action:
[![Robot video](https://img.youtube.com/watch?v=Q0VFYvG5bTc/0.jpg)(https://www.youtube.com/watch?v=Q0VFYvG5bTc)]

