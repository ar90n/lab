#include <linux/cdev.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

#define DEVICE_NAME "mychardev"
#define MINOR_BASE 0
#define MINOR_COUNT 1

static dev_t devno;
static struct cdev my_cdev;

static int my_open(struct inode *inode, struct file *file)
{
    pr_info("mychardev: open major=%d minor=%d\n",
            imajor(inode), iminor(inode));
    return 0;
}

static int my_release(struct inode *inode, struct file *file)
{
    pr_info("mychardev: release major=%d minor=%d\n",
            imajor(inode), iminor(inode));
    return 0;
}

static const struct file_operations my_fops = {
    .owner = THIS_MODULE,
    .open = my_open,
    .release = my_release,
};

static int __init my_init(void)
{
    int ret;
    int major;
    int minor;

    ret = alloc_chrdev_region(&devno, MINOR_BASE, MINOR_COUNT, DEVICE_NAME);
    if (ret < 0) {
        pr_err("mychardev: alloc_chrdev_region failed: %d\n", ret);
        return ret;
    }

    major = MAJOR(devno);
    minor = MINOR(devno);

    pr_info("mychardev: allocated major=%d minor=%d\n", major, minor);

    cdev_init(&my_cdev, &my_fops);
    my_cdev.owner = THIS_MODULE;

    ret = cdev_add(&my_cdev, devno, MINOR_COUNT);
    if (ret < 0) {
        pr_err("mychardev: cdev_add failed: %d\n", ret);
        unregister_chrdev_region(devno, MINOR_COUNT);
        return ret;
    }

    pr_info("mychardev: loaded. create node with:\n");
    pr_info("mychardev: sudo mknod /dev/%s c %d %d\n",
            DEVICE_NAME, major, minor);

    return 0;
}

static void __exit my_exit(void)
{
    cdev_del(&my_cdev);
    unregister_chrdev_region(devno, MINOR_COUNT);

    pr_info("mychardev: unloaded major=%d minor=%d\n",
            MAJOR(devno), MINOR(devno));
}

module_init(my_init);
module_exit(my_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("Minimal char device with dynamic major/minor allocation");
