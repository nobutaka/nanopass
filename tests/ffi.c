#include <stdbool.h>

int twelve() { return 12; }
int a_minus_b(int a, int b) { return a-b; }
bool tobe_or_nottobe(bool answer) { return !answer; }
int get_byte(char* data, int k) { return data[k]; }
extern int call_closure();
int call_call_closure() { return call_closure(); }
