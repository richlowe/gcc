// { dg-do run }
// { dg-options "" }
// { dg-additional-sources "interface1-a.cc" }

#pragma implementation "interface1.h"

#include "interface1.h"

extern void g();

int main () {
  g();
}
