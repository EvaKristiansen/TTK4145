#include "elev.h"
#include "erl_comm.h"
#include "elev_port.h"

/* Should be functional now(while might not do the trick), option: look over the nested cases.*/
/* Switches to fit with the erlang encoding in driver.erl*/

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
        elev_button_type_t button;
        switch(buf[1]){
          case(-1):
            button = BUTTON_CALL_DOWN;
            break;
          case(0):
            button = BUTTON_COMMAND;
          case(1):
            button = BUTTON_CALL_UP;
        }
        res[0] = elev_set_button_lamp(button,buf[2],buf[3]);
        break;

      case(FLOOR_INDICATOR_COMMAND):
        elev_set_floor_indicator(buf[1]);
        break;

      case(DOOR_OPEN_COMMAND):
        elev_set_door_open_lamp(buf[1]);
        break;

      case(BUTTON_SIGNAL_COMMAND):
        elev_button_type_t button;
        switch(buf[1]){
          case(-1):
            button = BUTTON_CALL_DOWN;
            break;
          case(0):
            button = BUTTON_COMMAND;
          case(1):
            button = BUTTON_CALL_UP;
        }
        res[0] = elev_get_button_signal(button,buf[2]);
        break;

      case(FLOOR_SENSOR_SIGNAL_COMMAND):
        res[0] = elev_get_floor_sensor_signal();
        break;
    }
    buf[0] = res;
    write_cmd(buf, 1);
  }
}