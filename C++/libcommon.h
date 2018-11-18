#ifndef __LIBCOMMON_H__
#define __LIBCOMMON_H__

#ifdef USING_COMMON
#error "USING_COMMON has been defined"
#else

#define PASCAL_USING_IOSTREAM
#ifdef _GLIBCXX_IOSTREAM
#undef PASCAL_USING_IOSTREAM
#define PASCAL_USING_IOSTREAM using std::cout; using std::cin; using std::endl;
#endif

#define USING_COMMON PASCAL_USING_IOSTREAM

#endif /* #ifdef USING_COMMN */

#endif /* __LIBCOMMON_H__ */
