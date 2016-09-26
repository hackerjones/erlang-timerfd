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

#include <ei.h>
#include <erl_driver.h>
#include <string.h>
#include "ei_x_extras.h"

ErlDrvBinary *ei_x_to_new_binary(const ei_x_buff *x)
{
    ErlDrvBinary *bin = driver_alloc_binary(x->index);

    if(bin != NULL)
        memcpy(bin->orig_bytes, x->buff, x->index);

    return bin;
}

int ei_x_decode_version(ei_x_buff *x, int *version)
{
    return ei_decode_version(x->buff, &x->index, version);
}

int ei_x_decode_atom(ei_x_buff *x, char *atom)
{
    return ei_decode_atom(x->buff, &x->index, atom);
}

int ei_x_decode_term(ei_x_buff *x, void *term)
{
    return ei_decode_term(x->buff, &x->index, term);
}

int ei_x_decode_tuple_header(ei_x_buff *x, int *arity)
{
    return ei_decode_tuple_header(x->buff, &x->index, arity);
}

int ei_x_decode_long(ei_x_buff *x, long *n)
{
    return ei_decode_long(x->buff, &x->index, n);
}

int ei_x_decode_longlong(ei_x_buff *x, long long *n)
{
    return ei_decode_longlong(x->buff, &x->index, n);
}

int ei_x_decode_ulong(ei_x_buff *x, unsigned long *n)
{
    return ei_decode_ulong(x->buff, &x->index, n);
}

int ei_x_decode_ulonglong(ei_x_buff *x, unsigned long long *n)
{
    return ei_decode_ulonglong(x->buff, &x->index, n);
}

