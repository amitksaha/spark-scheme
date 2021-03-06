#define LIBFFI_ASM
#include <fficonfig.h>
#include <ffi.h>
#include <powerpc/asm.h>

	.file   "ppc_closure.S"

#ifndef __powerpc64__

ENTRY(ffi_closure_SYSV)
.LFB1:
	stwu %r1,-144(%r1)
.LCFI0:
	mflr %r0
.LCFI1:
	stw %r0,148(%r1)

# we want to build up an areas for the parameters passed
# in registers (both floating point and integer)

	# so first save gpr 3 to gpr 10 (aligned to 4)
	stw   %r3, 16(%r1)
	stw   %r4, 20(%r1)
	stw   %r5, 24(%r1)
	stw   %r6, 28(%r1)
	stw   %r7, 32(%r1)
	stw   %r8, 36(%r1)
	stw   %r9, 40(%r1)
	stw   %r10,44(%r1)

	# next save fpr 1 to fpr 8 (aligned to 8)
	stfd  %f1, 48(%r1)
	stfd  %f2, 56(%r1)
	stfd  %f3, 64(%r1)
	stfd  %f4, 72(%r1)
	stfd  %f5, 80(%r1)
	stfd  %f6, 88(%r1)
	stfd  %f7, 96(%r1)
	stfd  %f8, 104(%r1)

	# set up registers for the routine that actually does the work
	# get the context pointer from the trampoline
	mr %r3,%r11

	# now load up the pointer to the result storage
	addi %r4,%r1,112

	# now load up the pointer to the saved gpr registers
	addi %r5,%r1,16

	# now load up the pointer to the saved fpr registers */
	addi %r6,%r1,48

	# now load up the pointer to the outgoing parameter
	# stack in the previous frame
	# i.e. the previous frame pointer + 8
	addi %r7,%r1,152

	# make the call
	bl ffi_closure_helper_SYSV@local

	# now r3 contains the return type
	# so use it to look up in a table
	# so we know how to deal with each type

	# Extract the size of the return type for small structures.
	# Then calculate (4 - size) and multiply the result by 8.
	# This gives the value needed for the shift operation below.
	# This part is only needed for FFI_SYSV and small structures.
	addi	%r5,%r3,-(FFI_SYSV_TYPE_SMALL_STRUCT)
	cmpwi	cr0,%r5,4
	ble	cr0,.Lnext
	addi	%r5,%r5,-4
.Lnext:
	addi	%r5,%r5,-4
	neg	%r5,%r5
	slwi	%r5,%r5,3

	# look up the proper starting point in table
	# by using return type as offset
	addi %r6,%r1,112   # get pointer to results area
	bl .Lget_ret_type0_addr # get pointer to .Lret_type0 into LR
	mflr %r4           # move to r4
	slwi %r3,%r3,4     # now multiply return type by 16
	add %r3,%r3,%r4    # add contents of table to table address
	mtctr %r3
	bctr               # jump to it
.LFE1:

# Each of the ret_typeX code fragments has to be exactly 16 bytes long
# (4 instructions). For cache effectiveness we align to a 16 byte boundary
# first.
	.align 4

	nop
	nop
	nop
.Lget_ret_type0_addr:
	blrl

# case FFI_TYPE_VOID
.Lret_type0:
	b .Lfinish
	nop
	nop
	nop

# case FFI_TYPE_INT
.Lret_type1:
	lwz %r3,0(%r6)
	b .Lfinish
	nop
	nop

# case FFI_TYPE_FLOAT
.Lret_type2:
	lfs %f1,0(%r6)
	b .Lfinish
	nop
	nop

# case FFI_TYPE_DOUBLE
.Lret_type3:
	lfd %f1,0(%r6)
	b .Lfinish
	nop
	nop

# case FFI_TYPE_LONGDOUBLE
.Lret_type4:
	lfd %f1,0(%r6)
	b .Lfinish
	nop
	nop

# case FFI_TYPE_UINT8
.Lret_type5:
	lbz %r3,3(%r6)
	b .Lfinish
	nop
	nop

# case FFI_TYPE_SINT8
.Lret_type6:
	lbz %r3,3(%r6)
	extsb %r3,%r3
	b .Lfinish
	nop

# case FFI_TYPE_UINT16
.Lret_type7:
	lhz %r3,2(%r6)
	b .Lfinish
	nop
	nop

# case FFI_TYPE_SINT16
.Lret_type8:
	lha %r3,2(%r6)
	b .Lfinish
	nop
	nop

# case FFI_TYPE_UINT32
.Lret_type9:
	lwz %r3,0(%r6)
	b .Lfinish
	nop
	nop

# case FFI_TYPE_SINT32
.Lret_type10:
	lwz %r3,0(%r6)
	b .Lfinish
	nop
	nop

# case FFI_TYPE_UINT64
.Lret_type11:
	lwz %r3,0(%r6)
	lwz %r4,4(%r6)
	b .Lfinish
	nop

# case FFI_TYPE_SINT64
.Lret_type12:
	lwz %r3,0(%r6)
	lwz %r4,4(%r6)
	b .Lfinish
	nop

# case FFI_TYPE_STRUCT
.Lret_type13:
	b .Lfinish
	nop
	nop
	nop

# case FFI_TYPE_POINTER
.Lret_type14:
	lwz %r3,0(%r6)
	b .Lfinish
	nop
	nop

# The return types below are only used when the ABI type is FFI_SYSV.
# case FFI_SYSV_TYPE_SMALL_STRUCT + 1. One byte struct.
.Lret_type15:
# fall through.
	nop
	nop
	nop
	nop

# case FFI_SYSV_TYPE_SMALL_STRUCT + 2. Two byte struct.
.Lret_type16:
# fall through.
	nop
	nop
	nop
	nop

# case FFI_SYSV_TYPE_SMALL_STRUCT + 3. Three byte struct.
.Lret_type17:
# fall through.
	nop
	nop
	nop
	nop

# case FFI_SYSV_TYPE_SMALL_STRUCT + 4. Four byte struct.
.Lret_type18:
# this one handles the structs from above too.
	lwz %r3,0(%r6)
	srw %r3,%r3,%r5
	b .Lfinish
	nop

# case FFI_SYSV_TYPE_SMALL_STRUCT + 5. Five byte struct.
.Lret_type19:
# fall through.
	nop
	nop
	nop
	nop

# case FFI_SYSV_TYPE_SMALL_STRUCT + 6. Six byte struct.
.Lret_type20:
# fall through.
	nop
	nop
	nop
	nop

# case FFI_SYSV_TYPE_SMALL_STRUCT + 7. Seven byte struct.
.Lret_type21:
# fall through.
	nop
	nop
	nop
	nop

# case FFI_SYSV_TYPE_SMALL_STRUCT + 8. Eight byte struct.
.Lret_type22:
# this one handles the above unhandled structs.
	lwz %r3,0(%r6)
	lwz %r4,4(%r6)
	bl __lshrdi3	# libgcc function to shift r3/r4, shift value in r5.
	b .Lfinish

# case done
.Lfinish:

	lwz %r0,148(%r1)
	mtlr %r0
	addi %r1,%r1,144
	blr
END(ffi_closure_SYSV)

	.section	".eh_frame",EH_FRAME_FLAGS,@progbits
.Lframe1:
	.4byte	.LECIE1-.LSCIE1	 # Length of Common Information Entry
.LSCIE1:
	.4byte	0x0	 # CIE Identifier Tag
	.byte	0x1	 # CIE Version
#if defined _RELOCATABLE || defined __PIC__
	.ascii "zR\0"	 # CIE Augmentation
#else
	.ascii "\0"	 # CIE Augmentation
#endif
	.uleb128 0x1	 # CIE Code Alignment Factor
	.sleb128 -4	 # CIE Data Alignment Factor
	.byte	0x41	 # CIE RA Column
#if defined _RELOCATABLE || defined __PIC__
	.uleb128 0x1	 # Augmentation size
	.byte	0x1b	 # FDE Encoding (pcrel sdata4)
#endif
	.byte	0xc	 # DW_CFA_def_cfa
	.uleb128 0x1
	.uleb128 0x0
	.align 2
.LECIE1:
.LSFDE1:
	.4byte	.LEFDE1-.LASFDE1	 # FDE Length
.LASFDE1:
	.4byte	.LASFDE1-.Lframe1	 # FDE CIE offset
#if defined _RELOCATABLE || defined __PIC__
	.4byte	.LFB1-.	 # FDE initial location
#else
	.4byte	.LFB1	 # FDE initial location
#endif
	.4byte	.LFE1-.LFB1	 # FDE address range
#if defined _RELOCATABLE || defined __PIC__
	.uleb128 0x0	 # Augmentation size
#endif
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	.LCFI0-.LFB1
	.byte	0xe	 # DW_CFA_def_cfa_offset
	.uleb128 144
	.byte	0x4	 # DW_CFA_advance_loc4
	.4byte	.LCFI1-.LCFI0
	.byte	0x11	 # DW_CFA_offset_extended_sf
	.uleb128 0x41
	.sleb128 -1
	.align 2
.LEFDE1:

#endif
