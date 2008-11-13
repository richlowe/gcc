/* { dg-do compile } */
/* { dg-options "" } */

template<int N> void foo()
{
    double d = (N ? 0.0 : 0) + 1;
}

