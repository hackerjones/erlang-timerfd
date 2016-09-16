/*
 * Copyright (c) 2016, Mark Jones <markalanj@gmail.com>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * The names of its contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

