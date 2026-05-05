#include "vm-config.h"

#define COAP_OBSERVE_CLIENT 1

/* Enable TCP support so the VM can offer stream sockets. Contiki-NG's
   default build disables TCP to save memory; the VM's UDP-only path
   stays unchanged when the user explicitly overrides this back to 0. */
#ifndef UIP_CONF_TCP
#define UIP_CONF_TCP 1
#endif

#ifndef UIP_CONF_TCP_CONNS
#define UIP_CONF_TCP_CONNS 4
#endif

#if CONTIKI_TARGET_ZOUL
#undef LPM_CONF_MAX_PM
#define LPM_CONF_MAX_PM 1

#define UIP_CONF_ND6_SEND_NA 0
#define UIP_CONF_ND6_SEND_NS 0
#define UIP_CONF_BUFFER_SIZE 256
#define SICSLOWPAN_CONF_FRAG 0
#define QUEUEBUF_CONF_NUM 1
#define NBR_TABLE_CONF_MAX_NEIGHBORS 2

#undef RPL_CONF_INSERT_HBH_OPTION
#define RPL_CONF_INSERT_HBH_OPTION 0
#endif

#define ENERGEST_CONF_ON 1
