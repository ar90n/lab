#include <linux/cdev.h>
#include <linux/device.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

#define DEVICE_NAME "udevcheck"
#define CLASS_NAME  "udevcheck"

static dev_t devno;
static struct class *udevcheck_class;
static struct device *udevcheck_device;
static struct cdev udevcheck_cdev;

static int udevcheck_open(struct inode *inode, struct file *file)
{
    pr_info("udevcheck: open major=%d minor=%d\n",
            imajor(inode), iminor(inode));
    return 0;
}

static int udevcheck_release(struct inode *inode, struct file *file)
{
    pr_info("udevcheck: release major=%d minor=%d\n",
            imajor(inode), iminor(inode));
    return 0;
}

static const struct file_operations udevcheck_fops = {
    .owner = THIS_MODULE,
    .open = udevcheck_open,
    .release = udevcheck_release,
};

static int __init udevcheck_init(void)
{
    int ret;

    ret = alloc_chrdev_region(&devno, 0, 1, DEVICE_NAME);
    if (ret < 0) {
        pr_err("udevcheck: alloc_chrdev_region failed: %d\n", ret);
        return ret;
    }

    cdev_init(&udevcheck_cdev, &udevcheck_fops);
    udevcheck_cdev.owner = THIS_MODULE;

    ret = cdev_add(&udevcheck_cdev, devno, 1);
    if (ret < 0) {
        pr_err("udevcheck: cdev_add failed: %d\n", ret);
        goto err_unregister_region;
    }

    udevcheck_class = class_create(CLASS_NAME);
    if (IS_ERR(udevcheck_class)) {
        ret = PTR_ERR(udevcheck_class);
        pr_err("udevcheck: class_create failed: %d\n", ret);
        goto err_cdev_del;
    }

    udevcheck_device = device_create(udevcheck_class,
                                     NULL,
                                     devno,
                                     NULL,
                                     DEVICE_NAME "%d",
                                     MINOR(devno));
    if (IS_ERR(udevcheck_device)) {
        ret = PTR_ERR(udevcheck_device);
        pr_err("udevcheck: device_create failed: %d\n", ret);
        goto err_class_destroy;
    }

    pr_info("udevcheck: loaded major=%d minor=%d\n",
            MAJOR(devno), MINOR(devno));
    pr_info("udevcheck: check /dev/%s0 and /sys/class/%s/%s0\n",
            DEVICE_NAME, CLASS_NAME, DEVICE_NAME);

    return 0;

err_class_destroy:
    class_destroy(udevcheck_class);

err_cdev_del:
    cdev_del(&udevcheck_cdev);

err_unregister_region:
    unregister_chrdev_region(devno, 1);

    return ret;
}

static void __exit udevcheck_exit(void)
{
    device_destroy(udevcheck_class, devno);
    class_destroy(udevcheck_class);
    cdev_del(&udevcheck_cdev);
    unregister_chrdev_region(devno, 1);

    pr_info("udevcheck: unloaded\n");
}

module_init(udevcheck_init);
module_exit(udevcheck_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("Minimal module to check udev device node creation");
