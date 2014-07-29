#!/bin/sh
mkdir ebin
erlc -o ebin *.erl
cp robot.app ebin
