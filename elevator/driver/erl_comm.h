/* From http://erlang.org/doc/tutorial/c_port.html */

#ifndef ERL_COMM
#define ERL_COMM

typedef unsigned char byte;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

#endif