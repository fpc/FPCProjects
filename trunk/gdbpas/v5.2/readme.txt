Apply simply this patch to
gdb-5.2 official source by executing
patch -p 0 < gdbpas52diffs
at gdb-5.2 directory level


Main diffs to official release source are:

- correct some watchpoint related bugs for i386

- change id name to all uppercase if not found as typed
(Free pascal specific)

+ try to find field names on the fly in pascal parser.

+ hardware register support for win32-nat target
which enables hardware watchpoints
(note that this has usual i386 debug register size
limitation contrary to my older hardware support
that was based on page memory protection,
but that also had several problems).

WARNING: this is experimental and possibly buggy:
+ SSE support for win32 by compiling with -DHAVE_CONTEXT_EXTENDED_REGISTERS
This should enable to see the xmm regs, but will possibly result
in problems if you try to modify the FPU registers
(because these registers appear at two locations in CONTEXT)

Instructions to update the packages/gdbint/libgdb/target directory.

 after compilation of the patched gdb,
 go to build/gdb dir

 ar -rs libgdb.a cli*.o mi*.o

and copy all libraries to the packages/gdbint/libgdb/$target directory.

 On unix systems this can be made easily by
going back to buildd directory and executing

  cp `ls */lib*.a` /some/fpc/src/base/dir/packages/gdbint/libgdb/$target
