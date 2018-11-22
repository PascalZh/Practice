#include <stdio.h>
#include <memory.h>
#include <malloc.h>
#include <errno.h>
#include <math.h>
#include "unconstrained_optimization_method.h"
// 注意：被优化的函数应该写成如下形式

// 函数假定会求极小值

double *g_xi_1;
double *g_d_i;
function_t g_f;
info_t g_info;

answer_t PowellAlgo(function_t f,info_t info)
{
#define FOR(i__, x__) for (i__ = 0; i__ < dim; i__++) { x__ }
  g_f = f; g_info = info;
  int i, j, k;
  
  double f_i_1;
  double f_i;
  double delta_i, delta_max, m;

  double F0, F2, F3;
  answer_t alpha_ans; double alpha;
  answer_t ret;

  const int dim = info.dimension;
  alpha_ans.minimum = malloc(sizeof(double) * dim);
  const double precision = info.precision;

  double *x0 = malloc(sizeof(double) * dim);
  double *xi = malloc(sizeof(double) * dim);
  double *x0_old = malloc(sizeof(double) * dim);
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
    FOR(i, x0[i] = info.x0[i];);
  } else {
    info.x0 = malloc(sizeof(double) * dim);
    g_info.x0 = info.x0;
    memset(info.x0, 0, dim);
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
        alpha_ans = gold_cut(fun1, &info);
        f_i = alpha_ans.value;
        alpha = *alpha_ans.minimum;

        // 计算xi
        FOR(j, xi[j] = xi[j] + alpha * d[i][j];)

        delta_i = f_i_1 - f_i;
        if (delta_i > delta_max) {delta_max = delta_i; m = i;}
       );
    FOR(j, x0_old[j] = x0[j];);

    FOR(j, dn1[j] = xi[j] - x0[j]; xn1[j] = 2*xi[j] - x0[j];);
    F0 = f(x0, dim); F2 = f(xi, dim); F3 = f(xn1, dim);

    if ( F3 < F0 &&
        (F0 - 2*F2 + F3) *
        SQUARE(F0 - F2 - delta_max)
        < 0.5 * delta_max * SQUARE(F0 - F3)) {

      // 此时xi已经算完，变成了xn
      g_xi_1 = xi; g_d_i = dn1;
      alpha_ans = gold_cut(fun1, &info);
      FOR(j, x0[j] = xi[j] + (alpha_ans.minimum)[0] * dn1[j];);

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
  } while( distance(xi, x0_old, dim) >= precision );

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
answer_t gold_cut(function_t f, info_t *info)
{
  answer_t ans;
  ans.minimum = malloc(sizeof(double) * (*info).dimension);
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
  } while( fabs( (b - a) / b ) >= (*info).precision && fabs( (y2 - y1) / y2 ) >= (* info ).precision );
  *ans.minimum = ( a + b ) / 2;
  ans.value = f(ans.minimum, 1);
  printf("gold cut result: (x, f(x)) = (%lf, %lf)\n", *ans.minimum, ans.value);
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

void jintui(function_t f, double *a, double *b, info_t *info)
{
  double h = 10;
  double a1 = 0; double a2 = a1 + h; double a3;
  double y1 = f(&a1, 1);
  double y2 = f(&a2, 1);
  double y3;
  if ( y2 > y1 ) {
    h = -h;
    a3 = a1; y3 = y1;
a:
    a1 = a2; y1 = y2; a2 = a3; y2 = y3;
  }
  a3 = a2 + h; y3 = f(&a3, 1);
  if (y3 < y2) {
    h = 2*h;
    goto a;
  } else {
    *a = a1 > a3?a3:a1;
    *b = a1 > a3?a1:a3;
  }
  printf("jintuifa result: [%lf, %lf]\n", *a, *b);
}
