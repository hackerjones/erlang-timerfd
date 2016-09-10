#include "erl_driver.h"

typedef struct {
	ErlDrvPort port;
}etimerfd;

static ErlDrvData 
etimerfd_start(ErlDrvPort port, char *cmd)
{
	etimerfd *d;

   	d = (etimerfd*)driver_alloc(sizeof(etimerfd));
	d->port = port;
	return (ErlDrvData)d;
}	

static void
etimerfd_stop(ErlDrvData d)
{
	driver_free((char*)d);
}

static void 
etimerfd_output(ErlDrvData d, char *buf, ErlDrvSizeT len)
{
}

static ErlDrvSSizeT
etimerfd_control(ErlDrvData d, unsigned int cmd, char *buf, ErlDrvSizeT len,
		char **rbuf, ErlDrvSizeT rlen)
{
	return 0;
}

ErlDrvEntry etimerfd_entry = {
	NULL, 							/* init */
	etimerfd_start, 				/* start */
	etimerfd_stop,					/* stop */
	etimerfd_output,				/* output */
	NULL,							/* ready_input */
	NULL,							/* ready_output */
	"etimerfd",						/* driver name */
	NULL,							/* finish */
	NULL,							/* VM reserved */
	etimerfd_control,				/* control */
	NULL,							/* timeout */
	NULL,							/* outputv */
	NULL,							/* ready_async */
	NULL,							/* flush */
	NULL,							/* call */
	NULL,							/* event */
	ERL_DRV_EXTENDED_MARKER,
	ERL_DRV_EXTENDED_MAJOR_VERSION,
	ERL_DRV_EXTENDED_MINOR_VERSION,
	0,								/* flags */
	NULL,							/* VM reserved */
	NULL							/* stop_select */
};

DRIVER_INIT(etimerfd)
{
	return &etimerfd_entry;
}

