PI_GCC_PREFIX=arm-unknown-linux-gnueabi
CC = $(PI_GCC_PREFIX)-gcc
CFLAGS = -g -I. -I/usr/local/include -L/usr/local/lib

brickpi_erlport:	brickpi_erlport.o erl_comm.o
	$(CC) $(CFLAGS) -o brickpi_erlport brickpi_erlport.o erl_comm.o -lwiringPi -lrt

clean:
	rm *.o brickpi_erlport
