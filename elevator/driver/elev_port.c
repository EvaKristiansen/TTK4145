#include "elev.h"
#include "erl_comm.h"
#include "elev_port.h"
#include <stdio.h>

/* Should be functional now(while might not do the trick), option: look over the nested cases.*/
/* Switches to fit with the erlang encoding in driver.erl*/

int main() {
  int res = 0, command;
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
        elev_set_button_lamp(buf[1],buf[2],buf[3]);
        break;

      case(FLOOR_INDICATOR_COMMAND):
        elev_set_floor_indicator(buf[1]);
        break;

      case(DOOR_OPEN_COMMAND):
        elev_set_door_open_lamp(buf[1]);
        break;

      case(BUTTON_SIGNAL_COMMAND):
        res = elev_get_button_signal(buf[1],buf[2]);
        break;

      case(FLOOR_SENSOR_SIGNAL_COMMAND):
        res = elev_get_floor_sensor_signal();
        break;

      case(RESET_ORDER_LIGHTS_COMMAND):
        elev_reset_order_lights(buf[1]);
        break;

      case(TURN_ALL_THE_LIGHTS_OFF_COMMAND):
        elev_turn_all_the_lights_off();
        break;
    }
    buf[0] = res; // DEBUG: Should be equal res
    write_cmd(buf, 1);
  }
  return 0;
}
