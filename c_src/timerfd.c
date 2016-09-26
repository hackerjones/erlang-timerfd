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

#include <erl_driver.h>
#include <ei.h>
#include <linux/time.h>
#include <sys/timerfd.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
#include "logger.h"
#include "ei_x_extras.h"

#define MODULE          "timerfd"
#define LOGFILE         "timerfd.log"

#define ATOM_TRUE               "true"
#define ATOM_FALSE              "false"
#define ATOM_OK                 "ok"
#define ATOM_ERROR              "error"
#define ATOM_ENOMEM             "enomem"
#define ATOM_EWOULDBLOCK        "ewouldblock"
#define ATOM_CLOCK_MONOTONIC    "clock_monotonic"
#define ATOM_CLOCK_REALTIME     "clock_realtime"

#define EVENT2FD(x) (long)(x)
#define FD2EVENT(x) (ErlDrvEvent)(long)(x)

typedef struct
{
    ErlDrvPort port;
    int fd;
} timer_data;

enum
{
    CREATE  = 0,
    SETTIME = 1,
    GETTIME = 2,
    READ = 3
};

static ErlDrvSSizeT encode_error(ei_x_buff *x, const char *str)
{
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_atom(x, ATOM_ERROR);
    ei_x_encode_string(x, str);
    return x->index;
}

static ErlDrvSSizeT encode_ok(ei_x_buff *x)
{
    ei_x_encode_atom(x, ATOM_OK);
    return x->index;
}

static ErlDrvSSizeT create_timer(timer_data *data, ei_x_buff *in_x_buff,
                                 ei_x_buff *out_x_buff)
{
    char atom[MAXATOMLEN];
    int clockid = -1;

    if(ei_x_decode_atom(in_x_buff, atom) == 0)
    {
        if(strcmp(atom, ATOM_CLOCK_MONOTONIC) == 0)
            clockid = CLOCK_MONOTONIC;
        else if(strcmp(atom, ATOM_CLOCK_REALTIME) == 0)
            clockid = CLOCK_REALTIME;

        if(clockid != -1)
        {
            data->fd = timerfd_create(clockid, TFD_NONBLOCK | TFD_CLOEXEC);
            if(data->fd < 0)
            {
                LOGGER_PRINT("timerfd_create() failed");
                encode_error(out_x_buff, "timerfd_create failed");
            }
            else
            {
                LOGGER_PRINT("timerfd_create() success");
                driver_select(data->port, FD2EVENT(data->fd),
                              ERL_DRV_READ | ERL_DRV_USE, 1);
                encode_ok(out_x_buff);
            }
        }
        else
        {
            LOGGER_PRINT("%s is bad clockid", atom);
            return -1; /* badarg */
        }
    }

    return out_x_buff->index;
}

static ErlDrvSSizeT settime(timer_data *data, ei_x_buff *in_x_buff,
                            ei_x_buff *out_x_buff)
{
    struct itimerspec new_value, old_value;
    int arity = 0, flags = 0;
    char atom[MAXATOMLEN];

    ei_x_decode_tuple_header(in_x_buff, &arity);
    ei_x_decode_tuple_header(in_x_buff, &arity);
    ei_x_decode_tuple_header(in_x_buff, &arity);
    ei_x_decode_long(in_x_buff, &new_value.it_interval.tv_sec);
    ei_x_decode_long(in_x_buff, &new_value.it_interval.tv_nsec);
    ei_x_decode_tuple_header(in_x_buff, &arity);
    ei_x_decode_long(in_x_buff, &new_value.it_value.tv_sec);
    ei_x_decode_long(in_x_buff, &new_value.it_value.tv_nsec);
    ei_x_decode_atom(in_x_buff, atom);

    if(strcmp(atom, ATOM_TRUE) == 0)
        flags = TFD_TIMER_ABSTIME;

    if(timerfd_settime(data->fd, flags, &new_value, &old_value) == 0)
    {
        LOGGER_PRINT("timerfd_settime sucessful");
        ei_x_format_wo_ver(out_x_buff, "{~a,{{~i,~i},{~i,~i}}}",
                           ATOM_OK,
                           old_value.it_interval.tv_sec,
                           old_value.it_interval.tv_nsec,
                           old_value.it_value.tv_sec,
                           old_value.it_value.tv_nsec);
    }
    return out_x_buff->index;
}

static ErlDrvSSizeT gettime(timer_data *data, ei_x_buff *in_x_buff,
                            ei_x_buff *out_x_buff)
{
    struct itimerspec curr_value;

    if(timerfd_gettime(data->fd, &curr_value) == 0)
    {
        ei_x_format_wo_ver(out_x_buff, "{ok,{{~i,~i},{~i,~i}}}",
                           curr_value.it_interval.tv_sec,
                           curr_value.it_interval.tv_nsec,
                           curr_value.it_value.tv_sec,
                           curr_value.it_value.tv_nsec);
    }

    return out_x_buff->index;
}

static ErlDrvSSizeT read_timer(timer_data *data, ei_x_buff *in_x_buff,
                               ei_x_buff *out_x_buff)
{
    uint64_t expirations;

    if(read(data->fd, &expirations, sizeof(expirations)) < 0)
    {
        switch(errno)
        {
        case EAGAIN:
            ei_x_format_wo_ver(out_x_buff, "{~a,~a}", ATOM_ERROR,
                               ATOM_EWOULDBLOCK);
            break;
        default:
            ei_x_format_wo_ver(out_x_buff, "{~a,~i}", ATOM_ERROR, errno);
            break;
        }
    }
    else
    {
        ei_x_format_wo_ver(out_x_buff, "{~a,~i}", ATOM_OK, expirations);
    }

    driver_select(data->port, FD2EVENT(data->fd),
                  ERL_DRV_READ | ERL_DRV_USE, 1);
    return out_x_buff->index;
}

static int init(void)
{
    LOGGER_OPEN(MODULE, LOGFILE);
    LOGGER_PRINT("driver loaded");
    return 0;
}

static void finish(void)
{
    LOGGER_PRINT("driver unloaded");
    LOGGER_CLOSE();
}

static ErlDrvData start(ErlDrvPort port, char *cmd)
{
    timer_data *data = (timer_data *)driver_alloc(sizeof(timer_data));
    if(data)
    {
        set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
        data->port = port;
        data->fd = -1;
        LOGGER_PRINT("port opened");
    }
    else
    {
        LOGGER_PRINT("failed to allocate port data");
        driver_failure_atom(port, ATOM_ENOMEM);
    }
    return (ErlDrvData)data;
}

static void stop(ErlDrvData handle)
{
    timer_data *data = (timer_data *)handle;

    if(data->fd)
    {
        driver_select(data->port, FD2EVENT(data->fd), ERL_DRV_READ, 0);
        close(data->fd);
    }

    driver_free(data);
    LOGGER_PRINT("port closed");
}

static ErlDrvSSizeT control(ErlDrvData handle,
                            unsigned int command,
                            char *buf, ErlDrvSizeT len,
                            char **rbuf, ErlDrvSizeT rlen)
{
    timer_data *data = (timer_data *)handle;
    ei_x_buff in_x_buff = {buf, len, 0};
    ei_x_buff out_x_buff;
    int version;
    ErlDrvSSizeT tmp;

    ei_x_decode_version(&in_x_buff, &version);

    if(ei_x_new_with_version(&out_x_buff) != 0)
    {
        driver_failure_atom(data->port, ATOM_ENOMEM);
        return 0;
    }

    switch(command)
    {
    case CREATE:
        tmp = create_timer(data, &in_x_buff, &out_x_buff);
        break;

    case SETTIME:
        tmp = settime(data, &in_x_buff, &out_x_buff);
        break;

    case GETTIME:
        tmp = gettime(data, &in_x_buff, &out_x_buff);
        break;

    case READ:
        tmp = read_timer(data, &in_x_buff, &out_x_buff);
        break;

    default:
        tmp = -1; /* badarg */
        break;
    }

    /* Small optimization. If the VM supplied buffer is large enough to hold
     * our data use it else allocate a new binary */
    if(out_x_buff.index <= rlen)
        memcpy(*rbuf, out_x_buff.buff, out_x_buff.index);
    else
        *rbuf = (char *)ei_x_to_new_binary(&out_x_buff);

    ei_x_free(&out_x_buff);
    return tmp;
}

static void ready_input(ErlDrvData handle, ErlDrvEvent event)
{
    timer_data *data = (timer_data *)handle;
    ei_x_buff x;

    if(EVENT2FD(event) == data->fd)
    {
        LOGGER_PRINT("ready");
        driver_select(data->port, FD2EVENT(data->fd),
                      ERL_DRV_READ | ERL_DRV_USE, 0);
        ei_x_new_with_version(&x);
        ei_x_encode_tuple_header(&x, 2);
        ei_x_encode_atom(&x, "timerfd");
        ei_x_encode_atom(&x, "ready");
        driver_output(data->port, x.buff, x.index);
    }
    else
    {
        LOGGER_PRINT("ready_input with bad fd");
    }
}

static void stop_select(ErlDrvEvent event, void *reserved)
{
}

ErlDrvEntry timerfd_entry =
{
    init,                           /* init */
    start,                          /* start */
    stop,                           /* stop */
    NULL,                           /* output */
    ready_input,                    /* ready_input */
    NULL,                           /* ready_output */
    MODULE,                         /* driver name */
    finish,                         /* finish */
    NULL,                           /* VM reserved */
    control,                        /* control */
    NULL,                           /* timeout */
    NULL,                           /* outputv */
    NULL,                           /* ready_async */
    NULL,                           /* flush */
    NULL,                           /* call */
    NULL,                           /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING,  /* flags */
    NULL,                           /* VM reserved */
    NULL,                           /* process exit */
    stop_select                     /* stop_select */
};

DRIVER_INIT(timerfd)
{
    return &timerfd_entry;
}

