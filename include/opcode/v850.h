/* v850.h -- Header file for NEC V850 opcode table
   Copyright 1996 Free Software Foundation, Inc.
   Written by J.T. Conklin, Cygnus Support

This file is part of GDB, GAS, and the GNU binutils.

GDB, GAS, and the GNU binutils are free software; you can redistribute
them and/or modify them under the terms of the GNU General Public
License as published by the Free Software Foundation; either version
1, or (at your option) any later version.

GDB, GAS, and the GNU binutils are distributed in the hope that they
will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this file; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef V850_H
#define V850_H

/* The opcode table is an array of struct v850_opcode.  */

struct v850_opcode
{
  /* The opcode name.  */
  const char *name;

  /* The opcode itself.  Those bits which will be filled in with
     operands are zeroes.  */
  unsigned long opcode;

  /* The opcode mask.  This is used by the disassembler.  This is a
     mask containing ones indicating those bits which must match the
     opcode field, and zeroes indicating those bits which need not
     match (and are presumably filled in by operands).  */
  unsigned long mask;

  /* An array of operand codes.  Each code is an index into the
     operand table.  They appear in the order which the operands must
     appear in assembly code, and are terminated by a zero.  */
  unsigned char operands[8];
};

/* The table itself is sorted by major opcode number, and is otherwise
   in the order in which the disassembler should consider
   instructions.  */
extern const struct v850_opcode v850_opcodes[];
extern const int v850_num_opcodes;


/* The operands table is an array of struct powerpc_operand.  */

struct v850_operand
{
  /* The number of bits in the operand.  */
  int bits;

  /* How far the operand is left shifted in the instruction.  */
  int shift;
};

/* Elements in the table are retrieved by indexing with values from
   the operands field of the v850_opcodes table.  */

extern const struct v850_operand v850_operands[];


#endif /* V850_H */
