/* { dg-do compile } */
/* { dg-options "" } */
struct JArray
{
  int data[1];
};
void *copyIntoByteArray (struct JArray *dest, __SIZE_TYPE__ offset)
{
  void *pdest = dest->data + offset;
  return pdest;
}
