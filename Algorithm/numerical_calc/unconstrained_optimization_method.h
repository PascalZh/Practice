#define SQUARE(x) ((x) * (x))
typedef double (* function_t)(double *, int);
typedef struct answer_t_ {
  int dimension;
  // 极小点
  double *minimum;
  // 极小点所对应的值
  double value;
} answer_t;
// 传入算法的附带信息
typedef struct info_t_ {
  int dimension;
  double *x0;
  double precision;
  void *other;
} info_t;

answer_t PowellAlgo(function_t, info_t);
answer_t gold_cut(function_t, info_t *);
double fun1(double *alpha, int dim);
double distance(double *x1, double *x2, int dimension);
void jintui(function_t f, double *a, double *b, info_t *info);
