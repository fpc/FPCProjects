Apply simply this patch to
gdb-5.3 official source by executing
patch -p 0 < gdb53.dif
at gdb-5.3 directory level


Main diffs to official release source are:

- change id name to all uppercase if not found as typed
(Free pascal specific)

* avoid i386 problems with 'pushl %esi'
that results in being unable to step into
the first called method inside a method.

+ try to find field names on the fly in pascal parser.


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
