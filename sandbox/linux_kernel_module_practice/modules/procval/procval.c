#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/proc_fs.h>
#include <linux/uaccess.h>

#define PROC_NAME "procval"
#define WRITE_BUF_SIZE 64
#define READ_BUF_SIZE 64

struct procval_state {
    struct mutex lock;
    int val;
};

static struct procval_state state;
static struct proc_dir_entry *proc_entry;

static ssize_t procval_read(struct file *file,
                            char __user *buf,
                            size_t count,
                            loff_t *ppos)
{
    char kbuf[READ_BUF_SIZE];
    int val;
    int len;

    if (mutex_lock_interruptible(&state.lock))
        return -ERESTARTSYS;

    val = state.val;

    mutex_unlock(&state.lock);

    len = scnprintf(kbuf, sizeof(kbuf), "%d\n", val);

    return simple_read_from_buffer(buf, count, ppos, kbuf, len);
}

static ssize_t procval_write(struct file *file,
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

    mutex_unlock(&state.lock);

    pr_info("procval: write val=%d\n", state.val);

    return count;
}

static const struct proc_ops procval_ops = {
    .proc_read = procval_read,
    .proc_write = procval_write,
};

static int __init procval_init(void)
{
    mutex_init(&state.lock);
    state.val = 0;

    proc_entry = proc_create(PROC_NAME, 0666, NULL, &procval_ops);
    if (!proc_entry) {
        pr_err("procval: proc_create failed\n");
        return -ENOMEM;
    }

    pr_info("procval: loaded /proc/%s\n", PROC_NAME);

    return 0;
}

static void __exit procval_exit(void)
{
    proc_remove(proc_entry);

    pr_info("procval: unloaded\n");
}

module_init(procval_init);
module_exit(procval_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("simple procfs read/write sample");
