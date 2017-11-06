#include "vm-config.h"

#if CONTIKI_TARGET_ZOUL
#undef LPM_CONF_MAX_PM
#define LPM_CONF_MAX_PM 1
#endif

#undef ENERGEST_CONF_ON
#define ENERGEST_CONF_ON 1

#undef UIP_CONF_ND6_SEND_NA
#define UIP_CONF_ND6_SEND_NA 0

#undef UIP_CONF_ND6_SEND_NS
#define UIP_CONF_ND6_SEND_NS 0

#undef UIP_CONF_BUFFER_SIZE
#define UIP_CONF_BUFFER_SIZE 256

#undef SICSLOWPAN_CONF_FRAG
#define SICSLOWPAN_CONF_FRAG 0

#undef QUEUEBUF_CONF_NUM
#define QUEUEBUF_CONF_NUM 1

#if 1
/* This value is set temporarily for a paper experiment. */
#define RESOLV_CONF_MAX_RETRIES 1
#endif

#undef NBR_TABLE_CONF_MAX_NEIGHBORS
#define NBR_TABLE_CONF_MAX_NEIGHBORS 2

#undef NETSTACK_CONF_RDC
#define NETSTACK_CONF_RDC contikimac_driver

#undef NETSTACK_CONF_MAC
#define NETSTACK_CONF_MAC csma_driver

#undef HEAPMEM_CONF_ARENA_SIZE
#define HEAPMEM_CONF_ARENA_SIZE VM_HEAP_SIZE        + \
                                VM_OBJECT_POOL_SIZE + \
                                VM_FRAME_POOL_SIZE

#undef HEAPMEM_CONF_REALLOC
#define HEAPMEM_CONF_REALLOC 1

#undef MEMPOOL_CONF_ALLOC
#define MEMPOOL_CONF_ALLOC heapmem_alloc
#undef MEMPOOL_CONF_FREE
#define MEMPOOL_CONF_FREE heapmem_free

#undef RPL_CONF_INSERT_HBH_OPTION
#define RPL_CONF_INSERT_HBH_OPTION 0
