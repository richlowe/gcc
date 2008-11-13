// Created by: Dodji Seketeli <dseketel@redhat.com>
// { dg-do compile }
// { dg-options "-fprofile-arcs" }
// Origin: PR C++/36767

struct A { A (); ~A (); };
A a[2];


