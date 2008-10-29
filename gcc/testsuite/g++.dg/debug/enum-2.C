/* Verify that used enums are output.  */
/* { dg-do compile } */
/* { dg-final { scan-assembler "JTI_MAX" { xfail sparc*-*-* } } } */

int var;

enum java_tree_index
{
  JTI_MAX
};

template<int X>
void tmpl (void)
{
  var = JTI_MAX + X;
}
 
void
function (void)
{
  tmpl<2>();
}
