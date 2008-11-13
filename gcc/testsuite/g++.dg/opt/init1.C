// PR c++/23171
// { dg-options "" }

int *p = (int*)(int[1]){0};
