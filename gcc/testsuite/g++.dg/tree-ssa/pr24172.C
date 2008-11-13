// { dg-options "" }
void IOException( char);
inline int* dummy( const char* const mode )
{
  IOException(*mode+*mode);
}

void prepare_inpaint( )
{
  dummy ("rb");
}
