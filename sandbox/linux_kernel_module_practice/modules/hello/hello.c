// SPDX-License-Identifier: GPL-2.0
/*
 * hello.c - minimal kernel module for learning
 *
 *   insmod hello.ko            # → "Hello, world!" in dmesg
 *   insmod hello.ko name=foo   # → "Hello, foo!"
 *   rmmod hello                # → "Goodbye, ..."
 */

#include <linux/module.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/moduleparam.h>

static char *name = "world";
module_param(name, charp, 0444);
MODULE_PARM_DESC(name, "Greeting target (default: world)");

static int __init hello_init(void)
{
	pr_info("hello: Hello, %s!\n", name);
	return 0;   /* 0 = success; non-zero aborts insmod */
}

static void __exit hello_exit(void)
{
	pr_info("hello: Goodbye, %s!\n", name);
}

module_init(hello_init);
module_exit(hello_exit);

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Masahiro Wada");
MODULE_DESCRIPTION("Minimal kernel module for learning");
MODULE_VERSION("0.1");
