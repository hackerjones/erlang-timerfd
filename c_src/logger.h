#ifndef LOGGER_H
#define LOGGER_H

#include <stdio.h>

extern FILE *logger_fp;

void logger_open(const char *module_name, const char *path);
void logger_close(void);
void logger_print(const char *format, ...);

#if DEBUG > 0
#define LOGGER_OPEN(x,y)        logger_open(x,y)
#define LOGGER_CLOSE            logger_close
#define LOGGER_PRINT(...)       logger_print(__VA_ARGS__)
#else
#define LOGGER_OPEN(x,y)        
#define LOGGER_CLOSE()       
#define LOGGER_PRINT(...)
#endif

#endif
