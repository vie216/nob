#ifndef LOG_H
#define LOG_H

#include <stdio.h>

#ifdef NDEBUG

#define ERROR(...) fprintf(stderr, "\033[31;1m[ERROR]\033[0m " __VA_ARGS__)
#define WARN(...) fprintf(stderr, "\033[33;1m[WARN]\033[0m " __VA_ARGS__)
#define INFO(...) printf("\033[1m[INFO]\033[0m " __VA_ARGS__)

#else

#define PERROR(prefix, ...) do {                              \
    fprintf(stderr, __FILE__ ":%d: ", __LINE__);              \
    fprintf(stderr, prefix "\033[31;1m[ERROR]\033[0m " __VA_ARGS__);  \
  } while (0)
#define PWARN(prefix, ...) do {                                         \
    fprintf(stderr, __FILE__ ":%d:  ", __LINE__);                       \
    fprintf(stderr, prefix ": \033[33;1m[WARN]\033[0m " __VA_ARGS__); \
  } while (0)
#define PINFO(prefix, ...) do {                   \
    printf(__FILE__ ":%d: ", __LINE__);           \
    printf(prefix "\033[1m[INFO]\033[0m " __VA_ARGS__); \
  } while(0)
#define ERROR(...) do {                                       \
    fprintf(stderr, __FILE__ ":%d: ", __LINE__);              \
    fprintf(stderr, "\033[31;1m[ERROR]\033[0m " __VA_ARGS__); \
  } while (0)
#define WARN(...) do {                                                  \
    fprintf(stderr, __FILE__ ":%d:  ", __LINE__);                       \
    fprintf(stderr, ": \033[33;1m[WARN]\033[0m " __VA_ARGS__); \
  } while (0)
#define INFO(...) do {                            \
    printf(__FILE__ ":%d: ", __LINE__);           \
    printf("\033[1m[INFO]\033[0m " __VA_ARGS__);  \
  } while(0)

#endif // DEBUG

#endif // LOG_H
