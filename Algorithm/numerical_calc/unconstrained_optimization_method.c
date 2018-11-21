#include <stdio.h>
#include <memory.h>
#include <malloc.h>
#include <errno.h>
#include <math.h>
#include "unconstrained_optimization_method.h"
// 注意：被优化的函数应该写成如下形式

// 函数假定会求极小值

const double *g_xi_1;
const double *g_d_i;
function_t g_f;
info_t g_info;

answer_t PowellAlgo(function_t f,info_t info)
{
#define FOR(i, x) for (i = 0; i < dim; i++) { x }
  g_f = f; g_info = info; g_info.dimension = 1;
  int i, j, k;
  
  double f_i_1;
  double f_i;
  double delta_i, delta_max, m;

  double F0, F2, F3;
  answer_t alpha_ans; double alpha;
  answer_t ret;

  const int dim = info.dimension;
  const double precision = info.precision;

  double *x0 = malloc(sizeof(double) * dim);
  double *xi = malloc(sizeof(double) * dim);
  double **d = malloc(sizeof(double *) * dim);
  double *dn1 = malloc(sizeof(double) * dim);
  double *xn1 = malloc(sizeof(double) * dim);

  // 初始化数据
  memset(x0, 0, dim);
  FOR(i,
      d[i] = malloc(sizeof(double) * dim);
      memset(d[i], 0, dim);
      d[i][i] = 1;
     );
  if (info.x0 != NULL) {
    FOR(i,
        x0[i] = info.x0[i];
       );
  }


  // 进入算法
  k = 0;
  do {
    delta_max = 0;
    // 这时xi还是xi-1
    FOR(j, xi[j] = x0[j];);
    FOR(i,
        // 计算f_i-1
        f_i_1 = f(xi, dim);

        // fun1
        // 通过全局变量传递参数
        g_xi_1 = xi; g_d_i = d[i];
        alpha_ans = gold_cut(fun1, g_info);
        f_i = alpha_ans.value;
        alpha = *alpha_ans.minimum;

        // 计算xi
        FOR(j, xi[j] = xi[j] + alpha * d[i][j];)

        delta_i = f_i_1 - f_i;
        if (delta_i > delta_max) {delta_max = delta_i; m = i;}
       );

    FOR(j, dn1[j] = xi[j] - x0[j]; xn1[j] = 2*xi[j] - x0[j];);
    F0 = f(x0, dim); F2 = f(xi, dim); F3 = f(xn1, dim);

    if ( F3 < F0 &&
        (F0 - 2*F2 + F3) *
        SQUARE(F0 - F2 - delta_max)
        < 0.5 * delta_max * SQUARE(F0 - F3)) {
      // TODO
      for (j = m; j < dim; j++) {
        if ( j == dim -1 ) {
          // 最后一个用dn+1补充
          FOR(k, d[j][k] = dn1[k];);
        } else {
          // d[m] 被删除了，后面的向前移位
          FOR(k, d[j][k] = d[j+1][k];);
        }
      }
    } else {
      if ( F2 < F3 ) {
        FOR(j, x0[j] = xi[j];);
      } else {
        FOR(j, x0[j] = xn1[j];);
      }
    }

    k++;
  } while( distance(xi, x0, dim) >= precision );

  ret.minimum = x0;
  ret.value = F0;
  return ret;
#undef FOR
}

double fun1(double *alpha, int dim)
{
  double *tmp = malloc(sizeof(double) * g_info.dimension);
  int i;
  if (dim != 1) { perror("fun1: dimension is not 1!"); return 0; }
  for (i = 0; i < g_info.dimension; i++) {
    // 注意：这里修改了g_xi_1
    tmp[i] = g_xi_1[i] + (*alpha) * g_d_i[i];
  }
  return g_f(tmp, g_info.dimension);
}
answer_t gold_cut(function_t f, info_t info)
{
  answer_t ans;
  if (info.dimension != 1) {perror("gold_cut: dimension is not 1!");}
  double a, b; // [a, b] 构成搜索区间
  jintui(f, &a, &b, info);

  double a1 = b - 0.618 * (b - a);
  double a2 = a + 0.618 * (b - a);
  double y1 = f(&a1, 1);
  double y2 = f(&a2, 1);
  do {
    if (y1 >= y2) {
      a = a1;
      a1 = a2;
      y1 = y2;
      a2 = a + 0.618 * (b - a);
      y2 = f(&a2, 1);
    } else {
      b = a2;
      a2 = a1;
      y2 = y1;
      a1 = b - 0.618 * (b - a);
      y1 = f(&a1, 1);
    }
  } while( fabs( (b - a) / b ) >= info.precision || fabs( (y2 - y1) / y2 ) >= info.precision );
  *ans.minimum = ( a + b ) / 2;
  ans.value = f(ans.minimum, 1);
  return ans;
}

double distance(double *x1, double *x2, int dimension)
{
  double ret = 0;
  int i;
  for (i = 0; i < dimension; i++) {
    ret += SQUARE(x1[i] - x2[i]);
  }
  return sqrt(ret);
}

void jintui(function_t f, double *a, double *b, info_t info)
{
  double t0,t1,t,h,f0,f1;
  double alpha = 2;
  int k=0;
  t0 = *info.x0;
  printf("\n请输入初始步长h=");
  scanf("%lf",&h);
  f0=f(&t0, 1);
  t1=t0+h;
  f1=f(&t1, 1);
  while(1)
  {
    if(f1<f0)
    {
      h=alpha*h;
      t=t0;
      t0=t1;
      f0=f1;
      k++;
    }
    else
    {
      if(k==0)
      {h=-h;t=t1;}
      else
      {
        *a=t<t1?t:t1;
        *b=t>t1?t:t1;
        break;
      }
    }
    t1=t0+h;
    f1=f(&t1, 1);
  }
}
