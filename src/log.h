#ifndef LOG_H
#define LOG_H

#include <stdio.h>

#ifdef NDEBUG

#define PERROR(prefix, ...) fprintf(stderr, prefix "\033[31;1m[ERROR]\033[0m " __VA_ARGS__)
#define PWARN(prefix, ...) fprintf(stderr, prefix "\033[33;1m[WARN]\033[0m " __VA_ARGS__)
#define PINFO(prefix, ...) printf(prefix "\033[1m[INFO]\033[0m " __VA_ARGS__)

#else

#define PERROR(prefix, ...)                                           \
  do {                                                                \
    fprintf(stderr, __FILE__ ":%d: ", __LINE__);                      \
    fprintf(stderr, prefix "\033[31;1m[ERROR]\033[0m " __VA_ARGS__);  \
  } while (0)
#define PWARN(prefix, ...)                                              \
  do {                                                                  \
    fprintf(stderr, __FILE__ ":%d:  ", __LINE__);                       \
    fprintf(stderr, prefix ": \033[33;1m[WARN]\033[0m " __VA_ARGS__);   \
  } while (0)
#define PINFO(prefix, ...)                              \
  do {                                                  \
    printf(__FILE__ ":%d: ", __LINE__);                 \
    printf(prefix "\033[1m[INFO]\033[0m " __VA_ARGS__); \
  } while(0)

#endif // DEBUG

#define ERROR(...) PERROR("", __VA_ARGS__)
#define WARN(...) PWARN("", __VA_ARGS__)
#define INFO(...) PINFO("", __VA_ARGS__)

#endif // LOG_H
