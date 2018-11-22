#include "unconstrained_optimization_method.h"
#include <stdio.h>
#include <errno.h>

double f007(double *x, int dim)
{
  if (dim != 2) {perror("f007: dimension is not 2!\n");}
  return 10*SQUARE(x[0] + x[1] - 5) + SQUARE(x[0] - x[1]);
}
int main(int argc, char *argv[])
{
  info_t info = {
    .dimension = 2,
    .x0 = NULL,
    .precision = 0.0000001
  };
  answer_t ans = PowellAlgo(f007, info);
  printf("x0 = %lf, x1 = %lf, f(x) = %lf\n",
      ans.minimum[0],
      ans.minimum[1],
      ans.value);
  return 0;
}
