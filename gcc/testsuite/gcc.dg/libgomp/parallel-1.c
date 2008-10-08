/* Trivial test of thread startup.  */

#include <omp.h>
#include <string.h>
#include <assert.h>


static int nthr;
static int saw[4];

static void function(void *dummy)
{
  int iam = omp_get_thread_num ();

  if (iam == 0)
    nthr = omp_get_num_threads ();

  saw[iam] = 1;
}

int main()
{
  omp_set_dynamic (0);

  #pragma omp parallel num_threads(2)
    function (NULL);

  assert (nthr == 2);
  assert (saw[0] != 0);
  assert (saw[1] != 0);
  assert (saw[2] == 0);

  memset (saw, 0, sizeof (saw));
  
  #pragma omp parallel num_threads(3)
    function (NULL);

  assert (nthr == 3);
  assert (saw[0] != 0);
  assert (saw[1] != 0);
  assert (saw[2] != 0);
  assert (saw[3] == 0);

  return 0;
}
