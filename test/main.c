#include "test.h"

void call_me_from_lisp(void) {
  print_factorial(add(2,3));
}
