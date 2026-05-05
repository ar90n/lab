#include <linux/cdev.h>
#include <linux/device.h>
#include <linux/errno.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/uaccess.h>
#include <linux/wait.h>

#define DEVICE_NAME "waitval"
#define CLASS_NAME  "waitval"
#define BUF_SIZE 256

struct waitval_device {
    struct cdev cdev;
    struct mutex lock;
    wait_queue_head_t read_wq;

    char buf[BUF_SIZE];
    size_t len;
    bool data_ready;
};

static dev_t devno;
static struct class *waitval_class;
static struct device *waitval_device_node;
static struct waitval_device waitval_dev;

static int waitval_open(struct inode *inode, struct file *file)
{
    struct waitval_device *dev;

    dev = container_of(inode->i_cdev, struct waitval_device, cdev);
    file->private_data = dev;

    return 0;
}

static ssize_t waitval_read(struct file *file,
                            char __user *buf,
                            size_t count,
                            loff_t *ppos)
{
    struct waitval_device *dev = file->private_data;
    char snapshot[BUF_SIZE];
    size_t snapshot_len;
    size_t n;
    int ret;

    (void)ppos;

retry:
    if (!READ_ONCE(dev->data_ready)) {
        if (file->f_flags & O_NONBLOCK)
            return -EAGAIN;

        ret = wait_event_interruptible(
            dev->read_wq,
            READ_ONCE(dev->data_ready)
        );
        if (ret)
            return ret;
    }

    if (mutex_lock_interruptible(&dev->lock))
        return -ERESTARTSYS;

    /*
     * 複数 reader が起きた場合、別 reader が先に消費している可能性がある。
     * なので mutex を取った後に必ず再確認する。
     */
    if (!dev->data_ready) {
        mutex_unlock(&dev->lock);
        goto retry;
    }

    snapshot_len = dev->len;
    memcpy(snapshot, dev->buf, snapshot_len);

    dev->len = 0;
    dev->buf[0] = '\0';
    WRITE_ONCE(dev->data_ready, false);

    mutex_unlock(&dev->lock);

    n = min(count, snapshot_len);

    if (copy_to_user(buf, snapshot, n))
        return -EFAULT;

    return n;
}

static ssize_t waitval_write(struct file *file,
                             const char __user *buf,
                             size_t count,
                             loff_t *ppos)
{
    struct waitval_device *dev = file->private_data;
    char tmp[BUF_SIZE];
    size_t n;

    (void)ppos;

    n = min(count, (size_t)(BUF_SIZE - 1));

    if (copy_from_user(tmp, buf, n))
        return -EFAULT;

    tmp[n] = '\0';

    if (mutex_lock_interruptible(&dev->lock))
        return -ERESTARTSYS;

    memcpy(dev->buf, tmp, n + 1);
    dev->len = n;
    WRITE_ONCE(dev->data_ready, true);

    mutex_unlock(&dev->lock);

    wake_up_interruptible(&dev->read_wq);

    return n;
}

static const struct file_operations waitval_fops = {
    .owner = THIS_MODULE,
    .open = waitval_open,
    .read = waitval_read,
    .write = waitval_write,
};

static int __init waitval_init(void)
{
    int ret;

    ret = alloc_chrdev_region(&devno, 0, 1, DEVICE_NAME);
    if (ret < 0)
        return ret;

    mutex_init(&waitval_dev.lock);
    init_waitqueue_head(&waitval_dev.read_wq);
    waitval_dev.len = 0;
    waitval_dev.buf[0] = '\0';
    waitval_dev.data_ready = false;

    cdev_init(&waitval_dev.cdev, &waitval_fops);
    waitval_dev.cdev.owner = THIS_MODULE;

    ret = cdev_add(&waitval_dev.cdev, devno, 1);
    if (ret < 0)
        goto err_unregister;

    waitval_class = class_create(CLASS_NAME);
    if (IS_ERR(waitval_class)) {
        ret = PTR_ERR(waitval_class);
        goto err_cdev;
    }

    waitval_device_node = device_create(waitval_class,
                                        NULL,
                                        devno,
                                        NULL,
                                        DEVICE_NAME "%d",
                                        MINOR(devno));
    if (IS_ERR(waitval_device_node)) {
        ret = PTR_ERR(waitval_device_node);
        goto err_class;
    }

    pr_info("waitval: loaded /dev/%s0 major=%d minor=%d\n",
            DEVICE_NAME, MAJOR(devno), MINOR(devno));

    return 0;

err_class:
    class_destroy(waitval_class);

err_cdev:
    cdev_del(&waitval_dev.cdev);

err_unregister:
    unregister_chrdev_region(devno, 1);

    return ret;
}

static void __exit waitval_exit(void)
{
    device_destroy(waitval_class, devno);
    class_destroy(waitval_class);
    cdev_del(&waitval_dev.cdev);
    unregister_chrdev_region(devno, 1);

    pr_info("waitval: unloaded\n");
}

module_init(waitval_init);
module_exit(waitval_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("wait_event_interruptible sample char device");
