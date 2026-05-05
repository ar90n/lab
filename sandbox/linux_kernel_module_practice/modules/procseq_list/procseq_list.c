#include <linux/errno.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/proc_fs.h>
#include <linux/seq_file.h>
#include <linux/uaccess.h>

#define PROC_NAME "procseq_list"
#define ITEM_COUNT 4
#define WRITE_BUF_SIZE 64

struct procseq_item {
    int val;
    unsigned long show_count;
    unsigned long write_count;
};

struct procseq_state {
    struct mutex lock;
    struct procseq_item items[ITEM_COUNT];
};

static struct procseq_state state;
static struct proc_dir_entry *proc_entry;

static void *procseq_start(struct seq_file *m, loff_t *pos)
{
    struct procseq_state *s = m->private;

    mutex_lock(&s->lock);

    if (*pos >= ITEM_COUNT) {
        mutex_unlock(&s->lock);
        return NULL;
    }

    return &s->items[*pos];
}

static void *procseq_next(struct seq_file *m, void *v, loff_t *pos)
{
    struct procseq_state *s = m->private;

    (void)v;

    (*pos)++;

    if (*pos >= ITEM_COUNT)
        return NULL;

    return &s->items[*pos];
}

static void procseq_stop(struct seq_file *m, void *v)
{
    struct procseq_state *s = m->private;

    (void)v;

    mutex_unlock(&s->lock);
}

static int procseq_show(struct seq_file *m, void *v)
{
    struct procseq_state *s = m->private;
    struct procseq_item *item = v;
    unsigned long index;

    index = item - s->items;

    item->show_count++;

    seq_printf(m,
               "%lu: val=%d show_count=%lu write_count=%lu\n",
               index,
               item->val,
               item->show_count,
               item->write_count);

    return 0;
}

static const struct seq_operations procseq_seq_ops = {
    .start = procseq_start,
    .next  = procseq_next,
    .stop  = procseq_stop,
    .show  = procseq_show,
};

static int procseq_open(struct inode *inode, struct file *file)
{
    struct seq_file *seq;
    int ret;

    (void)inode;

    ret = seq_open(file, &procseq_seq_ops);
    if (ret)
        return ret;

    seq = file->private_data;
    seq->private = &state;

    return 0;
}

static ssize_t procseq_write(struct file *file,
                             const char __user *buf,
                             size_t count,
                             loff_t *ppos)
{
    char kbuf[WRITE_BUF_SIZE];
    unsigned int index;
    long val;
    size_t n;
    int scanned;

    (void)file;
    (void)ppos;

    n = min(count, (size_t)(WRITE_BUF_SIZE - 1));

    if (copy_from_user(kbuf, buf, n))
        return -EFAULT;

    kbuf[n] = '\0';

    scanned = sscanf(kbuf, "%u %ld", &index, &val);
    if (scanned != 2)
        return -EINVAL;

    if (index >= ITEM_COUNT)
        return -ERANGE;

    if (mutex_lock_interruptible(&state.lock))
        return -ERESTARTSYS;

    state.items[index].val = (int)val;
    state.items[index].write_count++;

    mutex_unlock(&state.lock);

    pr_info("procseq_list: write index=%u val=%ld\n", index, val);

    return count;
}

static const struct proc_ops procseq_proc_ops = {
    .proc_open    = procseq_open,
    .proc_read    = seq_read,
    .proc_write   = procseq_write,
    .proc_lseek   = seq_lseek,
    .proc_release = seq_release,
};

static int __init procseq_init(void)
{
    int i;

    mutex_init(&state.lock);

    for (i = 0; i < ITEM_COUNT; i++) {
        state.items[i].val = 0;
        state.items[i].show_count = 0;
        state.items[i].write_count = 0;
    }

    proc_entry = proc_create(PROC_NAME, 0666, NULL, &procseq_proc_ops);
    if (!proc_entry) {
        pr_err("procseq_list: proc_create failed\n");
        return -ENOMEM;
    }

    pr_info("procseq_list: loaded /proc/%s\n", PROC_NAME);

    return 0;
}

static void __exit procseq_exit(void)
{
    proc_remove(proc_entry);

    pr_info("procseq_list: unloaded\n");
}

module_init(procseq_init);
module_exit(procseq_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("procfs seq_file iterator sample");
