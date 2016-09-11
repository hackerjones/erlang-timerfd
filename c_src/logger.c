#include <stdio.h>
#include <stdarg.h>
#include <erl_driver.h>
#include "logger.h"

FILE *logger_fp;
static const char *prefix;

void logger_open(const char *module_name, const char *path)
{
    logger_fp = fopen(path, "a+");
    if(logger_fp)
        prefix = module_name;
}

void logger_close(void)
{
    if(logger_fp)
    {
        fclose(logger_fp);
        logger_fp = NULL;
    }
}

void logger_print(const char *format, ...)
{
    va_list ap;

    if(!logger_fp)
        return;

    fprintf(logger_fp, "%s: ", prefix);
    va_start(ap, format);
    vfprintf(logger_fp, format, ap);
    va_end(ap);
    fprintf(logger_fp, "\n");
    fflush(logger_fp);
}

