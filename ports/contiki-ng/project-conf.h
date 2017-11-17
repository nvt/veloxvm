#include "vm-config.h"

#define COAP_OBSERVE_CLIENT 1

#if CONTIKI_TARGET_ZOUL
#undef LPM_CONF_MAX_PM
#define LPM_CONF_MAX_PM 1
#endif

#define ENERGEST_CONF_ON 1
#define UIP_CONF_ND6_SEND_NA 0
#define UIP_CONF_ND6_SEND_NS 0
#define UIP_CONF_BUFFER_SIZE 256
#define SICSLOWPAN_CONF_FRAG 0
#define QUEUEBUF_CONF_NUM 1
#define NBR_TABLE_CONF_MAX_NEIGHBORS 2
#define NETSTACK_CONF_MAC nullmac_driver

#undef HEAPMEM_CONF_ARENA_SIZE
#define HEAPMEM_CONF_ARENA_SIZE VM_HEAP_SIZE        + \
                                VM_OBJECT_POOL_SIZE + \
                                VM_FRAME_POOL_SIZE

#define HEAPMEM_CONF_REALLOC 1

#undef MEMPOOL_CONF_ALLOC
#define MEMPOOL_CONF_ALLOC heapmem_alloc
#undef MEMPOOL_CONF_FREE
#define MEMPOOL_CONF_FREE heapmem_free

#undef RPL_CONF_INSERT_HBH_OPTION
#define RPL_CONF_INSERT_HBH_OPTION 0
