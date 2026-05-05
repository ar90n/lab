#ifndef IOCTLVAL_UAPI_H
#define IOCTLVAL_UAPI_H

#include <linux/ioctl.h>
#include <linux/types.h>

#define IOCTLVAL_IOC_MAGIC 'v'

#define IOCTLVAL_SET_VAL _IOW(IOCTLVAL_IOC_MAGIC, 1, int32_t)
#define IOCTLVAL_GET_VAL _IOR(IOCTLVAL_IOC_MAGIC, 2, int32_t)

#endif
