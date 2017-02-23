#include "elev.h"
#include "erl_comm.h"
#include "elev_port.h"

/* NOT FINISHED */

typedef unsigned char byte;

int main() {
  int res = 0;
  byte buf[100];

  while (read_cmd(buf) > 0) {
    command = buf[0];
    
    switch(command){
      case(INIT_COMMAND):
        elev_init();
        break;
      case(MOTOR_DIRECTION_COMMAND):
        elev_set_motor_direction(buf[1]);
        break;
      case(BUTTON_LAMP_COMMAND):
        elev_set_button_lamp()
        break
      case(FLOOR_INDICATOR_COMMAND):
        break
      case(DOOR_OPEN_COMMAND):
        break
      case(BUTTON_SIGNAL_COMMAND):
        break
      case(FLOOR_SENSOR_SIGNAL_COMMAND):
        break
    }


    buf[0] = res;
    write_cmd(buf, 1);
  }
}