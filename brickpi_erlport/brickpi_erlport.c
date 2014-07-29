#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <wiringPi.h>
#include <tick.h>
#include <BrickPi.h>

typedef unsigned char byte;

#define ENABLE_PERIPHERALS  1
#define SET_MOTOR_SPEEDS    2
#define GET_DATA            3

int motor_ports[] = { PORT_A, PORT_B, PORT_C, PORT_D };
int sensor_ports[] = { PORT_1, PORT_2, PORT_3, PORT_4 };

void write_long(long l, byte *buff, int *pos) {
    buff[*pos] = (l >> 24) & 255;
    buff[(*pos) + 1] = (l >> 16) & 255;
    buff[(*pos) + 2] = (l >> 8) & 255;
    buff[(*pos) + 3] = l & 255;
    *pos = *pos + 4;
}

int main(int argc, char *argv[])
{
    byte buff[1024];
    int len, i, j, result, pos;
    short speed;
    long timeout;
    byte temp;
    FILE *outfile;

    if ((outfile = fopen("brickpi_erlport.log", "w")) == NULL) {
        exit(2);
    }

    result = BrickPiSetup();
    if (result) {
        fprintf(outfile, "Error initializing BrickPi: %d\n", result);
        fclose(outfile);
        exit(1);
    }

    BrickPi.Address[0] = 1;
    BrickPi.Address[1] = 2;

    while ((len = read_cmd(buff)) > 0) {
        switch (buff[0]) {
            case ENABLE_PERIPHERALS:
                for (i=0; i < 4; i++) {
                    BrickPi.MotorEnable[motor_ports[i]] = buff[1+i];
                    BrickPi.SensorType[sensor_ports[i]] = buff[5+i];
                }
                BrickPiSetupSensors();
                usleep(10000);
                BrickPiUpdateValues();
                usleep(10000);
                break;

            case SET_MOTOR_SPEEDS:
                for (i=0; i < 4; i++) {
                    speed = (buff[1+i*2] << 8) + buff[1+i*2+1];
                    if (speed > 255) speed = -speed;
                    if (speed < -255) speed = -255;
                    BrickPi.MotorSpeed[motor_ports[i]] = speed;
                }

                BrickPiUpdateValues();
                BrickPi.Timeout = (buff[9] << 24) + (buff[10] << 16) +
                    (buff[11] << 8) + buff[12];
                BrickPiSetTimeout();
                break;

            case GET_DATA:
                BrickPiUpdateValues();
                pos = 0;
                for (i=0; i < 4; i++) {
                    write_long(BrickPi.Sensor[sensor_ports[i]], buff, &pos);
                    for (j=0; j < 4; j++) {
                        write_long(BrickPi.SensorArray[sensor_ports[i]][j], buff, &pos);
                    }
                }
                for (i=0; i < 4; i++) {
                    write_long(BrickPi.Encoder[motor_ports[i]], buff, &pos);
                }
                write_cmd(buff, pos);
                break;               
        }
    }
}
