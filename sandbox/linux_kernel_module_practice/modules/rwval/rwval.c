#include <linux/cdev.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/string.h>
#include <linux/uaccess.h>

#define DEVICE_NAME "rwval"
#define MINOR_BASE 0
#define MINOR_COUNT 4
#define VAL_SIZE 256

struct rwval_device {
    struct cdev cdev;
    struct mutex lock;
    unsigned int minor;
    char val[VAL_SIZE];
    size_t len;
};

static dev_t base_devno;
static struct rwval_device devices[MINOR_COUNT];

static int rwval_open(struct inode *inode, struct file *file)
{
    struct rwval_device *dev;

    dev = container_of(inode->i_cdev, struct rwval_device, cdev);
    file->private_data = dev;

    pr_info("rwval: open minor=%u\n", dev->minor);

    return 0;
}

static int rwval_release(struct inode *inode, struct file *file)
{
    struct rwval_device *dev = file->private_data;

    pr_info("rwval: release minor=%u\n", dev->minor);

    return 0;
}

static ssize_t rwval_read(struct file *file,
                          char __user *buf,
                          size_t count,
                          loff_t *ppos)
{
    struct rwval_device *dev = file->private_data;
    char snapshot[VAL_SIZE];
    size_t snapshot_len;
    size_t n;

    if (*ppos < 0)
        return -EINVAL;

    if (mutex_lock_interruptible(&dev->lock))
        return -ERESTARTSYS;

    snapshot_len = dev->len;
    memcpy(snapshot, dev->val, snapshot_len);

    mutex_unlock(&dev->lock);

    if ((size_t)*ppos >= snapshot_len)
        return 0;

    n = min(count, snapshot_len - (size_t)*ppos);

    if (copy_to_user(buf, snapshot + *ppos, n))
        return -EFAULT;

    *ppos += n;

    return n;
}

static ssize_t rwval_write(struct file *file,
                           const char __user *buf,
                           size_t count,
                           loff_t *ppos)
{
    struct rwval_device *dev = file->private_data;
    char tmp[VAL_SIZE];
    size_t n;

    n = min(count, (size_t)(VAL_SIZE - 1));

    if (copy_from_user(tmp, buf, n))
        return -EFAULT;

    tmp[n] = '\0';

    if (mutex_lock_interruptible(&dev->lock))
        return -ERESTARTSYS;

    memcpy(dev->val, tmp, n + 1);
    dev->len = n;

    mutex_unlock(&dev->lock);

    pr_info("rwval: write minor=%u len=%zu\n", dev->minor, n);

    return n;
}

static const struct file_operations rwval_fops = {
    .owner = THIS_MODULE,
    .open = rwval_open,
    .release = rwval_release,
    .read = rwval_read,
    .write = rwval_write,
};

static int __init rwval_init(void)
{
    int ret;
    int i;
    int added = 0;
    int major;

    ret = alloc_chrdev_region(&base_devno,
                              MINOR_BASE,
                              MINOR_COUNT,
                              DEVICE_NAME);
    if (ret < 0) {
        pr_err("rwval: alloc_chrdev_region failed: %d\n", ret);
        return ret;
    }

    major = MAJOR(base_devno);

    for (i = 0; i < MINOR_COUNT; i++) {
        dev_t devno = MKDEV(major, MINOR_BASE + i);

        devices[i].minor = MINOR_BASE + i;
        devices[i].len = 0;
        devices[i].val[0] = '\0';
        mutex_init(&devices[i].lock);

        cdev_init(&devices[i].cdev, &rwval_fops);
        devices[i].cdev.owner = THIS_MODULE;

        ret = cdev_add(&devices[i].cdev, devno, 1);
        if (ret < 0) {
            pr_err("rwval: cdev_add failed minor=%d ret=%d\n", i, ret);
            goto err_cdev_del;
        }

        added++;
    }

    pr_info("rwval: loaded major=%d minor_base=%d count=%d\n",
            major, MINOR_BASE, MINOR_COUNT);

    for (i = 0; i < MINOR_COUNT; i++) {
        pr_info("rwval: sudo mknod /dev/rwval%d c %d %d\n",
                i, major, MINOR_BASE + i);
    }

    return 0;

err_cdev_del:
    while (added > 0) {
        added--;
        cdev_del(&devices[added].cdev);
    }

    unregister_chrdev_region(base_devno, MINOR_COUNT);

    return ret;
}

static void __exit rwval_exit(void)
{
    int i;

    for (i = 0; i < MINOR_COUNT; i++)
        cdev_del(&devices[i].cdev);

    unregister_chrdev_region(base_devno, MINOR_COUNT);

    pr_info("rwval: unloaded\n");
}

module_init(rwval_init);
module_exit(rwval_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("Multiple minor char devices with mutex-protected values");
