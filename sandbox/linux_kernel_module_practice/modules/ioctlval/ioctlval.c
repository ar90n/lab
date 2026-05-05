#include <linux/cdev.h>
#include <linux/cred.h>
#include <linux/device.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/uaccess.h>

#include "ioctlval_uapi.h"

#define DEVICE_NAME "ioctlval"
#define CLASS_NAME  "ioctlval"
#define MINOR_BASE 0
#define MINOR_COUNT 1

struct ioctlval_device {
    struct cdev cdev;
    struct mutex lock;
    int32_t val;
};

static dev_t devno;
static struct class *ioctlval_class;
static struct device *ioctlval_device_node;
static struct ioctlval_device ioctlval_dev;

static int ioctlval_open(struct inode *inode, struct file *file)
{
    struct ioctlval_device *dev;

    dev = container_of(inode->i_cdev, struct ioctlval_device, cdev);
    file->private_data = dev;

    pr_info("ioctlval: open major=%d minor=%d\n",
            imajor(inode), iminor(inode));

    return 0;
}

static int ioctlval_release(struct inode *inode, struct file *file)
{
    pr_info("ioctlval: release major=%d minor=%d\n",
            imajor(inode), iminor(inode));

    return 0;
}

static ssize_t ioctlval_read(struct file *file,
                             char __user *buf,
                             size_t count,
                             loff_t *ppos)
{
    struct ioctlval_device *dev = file->private_data;
    char kbuf[32];
    int32_t val;
    int len;

    if (mutex_lock_interruptible(&dev->lock))
        return -ERESTARTSYS;

    val = dev->val;

    mutex_unlock(&dev->lock);

    len = scnprintf(kbuf, sizeof(kbuf), "%d\n", val);

    return simple_read_from_buffer(buf, count, ppos, kbuf, len);
}

static long ioctlval_ioctl(struct file *file,
                           unsigned int cmd,
                           unsigned long arg)
{
    struct ioctlval_device *dev = file->private_data;
    int32_t val;

    switch (cmd) {
    case IOCTLVAL_SET_VAL:
        if (!capable(CAP_SYS_ADMIN)) {
            pr_warn("ioctlval: SET_VAL denied for non-root process\n");
            return -EPERM;
        }

        if (copy_from_user(&val, (const void __user *)arg, sizeof(val)))
            return -EFAULT;

        if (mutex_lock_interruptible(&dev->lock))
            return -ERESTARTSYS;

        dev->val = val;

        mutex_unlock(&dev->lock);

        pr_info("ioctlval: SET_VAL val=%d\n", val);

        return 0;

    case IOCTLVAL_GET_VAL:
        if (mutex_lock_interruptible(&dev->lock))
            return -ERESTARTSYS;

        val = dev->val;

        mutex_unlock(&dev->lock);

        if (copy_to_user((void __user *)arg, &val, sizeof(val)))
            return -EFAULT;

        pr_info("ioctlval: GET_VAL val=%d\n", val);

        return 0;

    default:
        return -ENOTTY;
    }
}

static const struct file_operations ioctlval_fops = {
    .owner = THIS_MODULE,
    .open = ioctlval_open,
    .release = ioctlval_release,
    .read = ioctlval_read,
    .unlocked_ioctl = ioctlval_ioctl,
};

static int __init ioctlval_init(void)
{
    int ret;

    ret = alloc_chrdev_region(&devno,
                              MINOR_BASE,
                              MINOR_COUNT,
                              DEVICE_NAME);
    if (ret < 0) {
        pr_err("ioctlval: alloc_chrdev_region failed: %d\n", ret);
        return ret;
    }

    mutex_init(&ioctlval_dev.lock);
    ioctlval_dev.val = 0;

    cdev_init(&ioctlval_dev.cdev, &ioctlval_fops);
    ioctlval_dev.cdev.owner = THIS_MODULE;

    ret = cdev_add(&ioctlval_dev.cdev, devno, MINOR_COUNT);
    if (ret < 0) {
        pr_err("ioctlval: cdev_add failed: %d\n", ret);
        goto err_unregister_chrdev;
    }

    ioctlval_class = class_create(CLASS_NAME);
    if (IS_ERR(ioctlval_class)) {
        ret = PTR_ERR(ioctlval_class);
        pr_err("ioctlval: class_create failed: %d\n", ret);
        goto err_cdev_del;
    }

    ioctlval_device_node = device_create(ioctlval_class,
                                         NULL,
                                         devno,
                                         NULL,
                                         DEVICE_NAME "%d",
                                         MINOR(devno));
    if (IS_ERR(ioctlval_device_node)) {
        ret = PTR_ERR(ioctlval_device_node);
        pr_err("ioctlval: device_create failed: %d\n", ret);
        goto err_class_destroy;
    }

    pr_info("ioctlval: loaded major=%d minor=%d\n",
            MAJOR(devno), MINOR(devno));
    pr_info("ioctlval: device node should be /dev/%s0\n", DEVICE_NAME);

    return 0;

err_class_destroy:
    class_destroy(ioctlval_class);

err_cdev_del:
    cdev_del(&ioctlval_dev.cdev);

err_unregister_chrdev:
    unregister_chrdev_region(devno, MINOR_COUNT);

    return ret;
}

static void __exit ioctlval_exit(void)
{
    device_destroy(ioctlval_class, devno);
    class_destroy(ioctlval_class);
    cdev_del(&ioctlval_dev.cdev);
    unregister_chrdev_region(devno, MINOR_COUNT);

    pr_info("ioctlval: unloaded\n");
}

module_init(ioctlval_init);
module_exit(ioctlval_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("ioctl sample module with root-only SET_VAL and readable value");
