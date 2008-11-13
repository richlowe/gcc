/* { dg-do compile } */
/* { dg-options "" } */

template <class T>
struct Bar
{
        int get() { return static_cast<T*>(this)->get2(); }
};
struct Foo : public Bar<Foo>
{
        int get2() { return x; }
        int x;
};

int foo(Foo& f)
{
        return f.get();
}

