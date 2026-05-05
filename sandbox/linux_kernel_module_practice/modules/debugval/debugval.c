#include <linux/debugfs.h>
#include <linux/errno.h>
#include <linux/fs.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/seq_file.h>
#include <linux/uaccess.h>

#define DRIVER_NAME "debugval"
#define WRITE_BUF_SIZE 64
#define READ_BUF_SIZE 64

struct debugval_state {
    struct mutex lock;
    int value;
    unsigned long read_count;
    unsigned long write_count;

    u32 raw_u32;
    bool enabled;
};

static struct debugval_state state;
static struct dentry *debugval_dir;

static ssize_t debugval_value_read(struct file *file,
                                   char __user *buf,
                                   size_t count,
                                   loff_t *ppos)
{
    struct debugval_state *s = file->private_data;
    char kbuf[READ_BUF_SIZE];
    int value;
    int len;

    if (mutex_lock_interruptible(&s->lock))
        return -ERESTARTSYS;

    value = s->value;
    s->read_count++;

    mutex_unlock(&s->lock);

    len = scnprintf(kbuf, sizeof(kbuf), "%d\n", value);

    return simple_read_from_buffer(buf, count, ppos, kbuf, len);
}

static ssize_t debugval_value_write(struct file *file,
                                    const char __user *buf,
                                    size_t count,
                                    loff_t *ppos)
{
    struct debugval_state *s = file->private_data;
    char kbuf[WRITE_BUF_SIZE];
    long value;
    size_t n;
    int ret;

    (void)ppos;

    n = min(count, (size_t)(WRITE_BUF_SIZE - 1));

    if (copy_from_user(kbuf, buf, n))
        return -EFAULT;

    kbuf[n] = '\0';

    ret = kstrtol(kbuf, 0, &value);
    if (ret < 0)
        return ret;

    if (mutex_lock_interruptible(&s->lock))
        return -ERESTARTSYS;

    s->value = (int)value;
    s->write_count++;

    mutex_unlock(&s->lock);

    pr_info("debugval: value=%d\n", (int)value);

    return count;
}

static const struct file_operations debugval_value_fops = {
    .owner = THIS_MODULE,
    .open = simple_open,
    .read = debugval_value_read,
    .write = debugval_value_write,
    .llseek = default_llseek,
};

static int debugval_status_show(struct seq_file *m, void *v)
{
    struct debugval_state *s = m->private;
    int value;
    unsigned long read_count;
    unsigned long write_count;
    u32 raw_u32;
    bool enabled;

    (void)v;

    if (mutex_lock_interruptible(&s->lock))
        return -ERESTARTSYS;

    value = s->value;
    read_count = s->read_count;
    write_count = s->write_count;
    raw_u32 = s->raw_u32;
    enabled = s->enabled;

    mutex_unlock(&s->lock);

    seq_puts(m, "debugval status\n");
    seq_puts(m, "===============\n");
    seq_printf(m, "value:       %d\n", value);
    seq_printf(m, "read_count:  %lu\n", read_count);
    seq_printf(m, "write_count: %lu\n", write_count);
    seq_printf(m, "raw_u32:     %u\n", raw_u32);
    seq_printf(m, "enabled:     %s\n", enabled ? "true" : "false");

    return 0;
}

static int debugval_status_open(struct inode *inode, struct file *file)
{
    return single_open(file, debugval_status_show, inode->i_private);
}

static const struct file_operations debugval_status_fops = {
    .owner = THIS_MODULE,
    .open = debugval_status_open,
    .read = seq_read,
    .llseek = seq_lseek,
    .release = single_release,
};

static int __init debugval_init(void)
{
    struct dentry *d;

    mutex_init(&state.lock);
    state.value = 0;
    state.read_count = 0;
    state.write_count = 0;
    state.raw_u32 = 42;
    state.enabled = true;

    debugval_dir = debugfs_create_dir(DRIVER_NAME, NULL);
    if (IS_ERR(debugval_dir)) {
        pr_err("debugval: debugfs_create_dir failed: %ld\n",
               PTR_ERR(debugval_dir));
        return PTR_ERR(debugval_dir);
    }

    d = debugfs_create_file("value",
                            0644,
                            debugval_dir,
                            &state,
                            &debugval_value_fops);
    if (IS_ERR(d)) {
        pr_err("debugval: debugfs_create_file(value) failed: %ld\n",
               PTR_ERR(d));
        debugfs_remove_recursive(debugval_dir);
        return PTR_ERR(d);
    }

    d = debugfs_create_file("status",
                            0444,
                            debugval_dir,
                            &state,
                            &debugval_status_fops);
    if (IS_ERR(d)) {
        pr_err("debugval: debugfs_create_file(status) failed: %ld\n",
               PTR_ERR(d));
        debugfs_remove_recursive(debugval_dir);
        return PTR_ERR(d);
    }

    debugfs_create_u32("raw_u32", 0644, debugval_dir, &state.raw_u32);
    debugfs_create_bool("enabled", 0644, debugval_dir, &state.enabled);

    pr_info("debugval: loaded. see /sys/kernel/debug/%s\n", DRIVER_NAME);

    return 0;
}

static void __exit debugval_exit(void)
{
    debugfs_remove_recursive(debugval_dir);
    pr_info("debugval: unloaded\n");
}

module_init(debugval_init);
module_exit(debugval_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("debugfs sample module");
