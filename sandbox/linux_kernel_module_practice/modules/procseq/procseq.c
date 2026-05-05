#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/proc_fs.h>
#include <linux/seq_file.h>
#include <linux/uaccess.h>

#define PROC_NAME "procseq"
#define WRITE_BUF_SIZE 64

struct procseq_state {
    struct mutex lock;
    int val;
    unsigned long read_count;
    unsigned long write_count;
};

static struct procseq_state state;
static struct proc_dir_entry *proc_entry;

static int procseq_show(struct seq_file *m, void *v)
{
    int val;
    unsigned long read_count;
    unsigned long write_count;

    (void)v;

    if (mutex_lock_interruptible(&state.lock))
        return -ERESTARTSYS;

    state.read_count++;

    val = state.val;
    read_count = state.read_count;
    write_count = state.write_count;

    mutex_unlock(&state.lock);

    seq_puts(m, "procseq status\n");
    seq_puts(m, "==============\n");
    seq_printf(m, "val:         %d\n", val);
    seq_printf(m, "read_count:  %lu\n", read_count);
    seq_printf(m, "write_count: %lu\n", write_count);

    return 0;
}

static int procseq_open(struct inode *inode, struct file *file)
{
    return single_open(file, procseq_show, NULL);
}

static ssize_t procseq_write(struct file *file,
                             const char __user *buf,
                             size_t count,
                             loff_t *ppos)
{
    char kbuf[WRITE_BUF_SIZE];
    long val;
    size_t n;
    int ret;

    (void)file;
    (void)ppos;

    n = min(count, (size_t)(WRITE_BUF_SIZE - 1));

    if (copy_from_user(kbuf, buf, n))
        return -EFAULT;

    kbuf[n] = '\0';

    ret = kstrtol(kbuf, 0, &val);
    if (ret < 0)
        return ret;

    if (mutex_lock_interruptible(&state.lock))
        return -ERESTARTSYS;

    state.val = (int)val;
    state.write_count++;

    mutex_unlock(&state.lock);

    pr_info("procseq: write val=%d\n", (int)val);

    return count;
}

static const struct proc_ops procseq_ops = {
    .proc_open    = procseq_open,
    .proc_read    = seq_read,
    .proc_write   = procseq_write,
    .proc_lseek   = seq_lseek,
    .proc_release = single_release,
};

static int __init procseq_init(void)
{
    mutex_init(&state.lock);
    state.val = 0;
    state.read_count = 0;
    state.write_count = 0;

    proc_entry = proc_create(PROC_NAME, 0666, NULL, &procseq_ops);
    if (!proc_entry) {
        pr_err("procseq: proc_create failed\n");
        return -ENOMEM;
    }

    pr_info("procseq: loaded /proc/%s\n", PROC_NAME);

    return 0;
}

static void __exit procseq_exit(void)
{
    proc_remove(proc_entry);

    pr_info("procseq: unloaded\n");
}

module_init(procseq_init);
module_exit(procseq_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("procfs seq_file sample");
