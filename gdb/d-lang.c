/* D language support routines for GDB, the GNU debugger.

   Copyright (C) 2005-2019 Free Software Foundation, Inc.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include "defs.h"
#include "symtab.h"
#include "language.h"
#include "varobj.h"
#include "d-lang.h"
#include "c-lang.h"
#include "demangle.h"
#include "cp-support.h"
#include "gdbarch.h"

/* The name of the symbol to use to get the name of the main subprogram.  */
static const char D_MAIN[] = "D main";

/* Function returning the special symbol name used by D for the main
   procedure in the main program if it is found in minimal symbol list.
   This function tries to find minimal symbols so that it finds them even
   if the program was compiled without debugging information.  */

const char *
d_main_name (void)
{
  struct bound_minimal_symbol msym;

  msym = lookup_minimal_symbol (D_MAIN, NULL, NULL);
  if (msym.minsym != NULL)
    return D_MAIN;

  /* No known entry procedure found, the main program is probably not D.  */
  return NULL;
}

/* Implements the la_demangle language_defn routine for language D.  */

char *
d_demangle (const char *symbol, int options)
{
  return gdb_demangle (symbol, options | DMGL_DLANG);
}

/* la_sniff_from_mangled_name implementation for D.  */

static int
d_sniff_from_mangled_name (const char *mangled, char **demangled)
{
  *demangled = d_demangle (mangled, 0);
  return *demangled != NULL;
}

/* Table mapping opcodes into strings for printing operators
   and precedences of the operators.  */
static const struct op_print d_op_print_tab[] =
{
  {",", BINOP_COMMA, PREC_COMMA, 0},
  {"=", BINOP_ASSIGN, PREC_ASSIGN, 1},
  {"||", BINOP_LOGICAL_OR, PREC_LOGICAL_OR, 0},
  {"&&", BINOP_LOGICAL_AND, PREC_LOGICAL_AND, 0},
  {"|", BINOP_BITWISE_IOR, PREC_BITWISE_IOR, 0},
  {"^", BINOP_BITWISE_XOR, PREC_BITWISE_XOR, 0},
  {"&", BINOP_BITWISE_AND, PREC_BITWISE_AND, 0},
  {"==", BINOP_EQUAL, PREC_ORDER, 0},
  {"!=", BINOP_NOTEQUAL, PREC_ORDER, 0},
  {"<=", BINOP_LEQ, PREC_ORDER, 0},
  {">=", BINOP_GEQ, PREC_ORDER, 0},
  {">", BINOP_GTR, PREC_ORDER, 0},
  {"<", BINOP_LESS, PREC_ORDER, 0},
  {">>", BINOP_RSH, PREC_SHIFT, 0},
  {"<<", BINOP_LSH, PREC_SHIFT, 0},
  {"+", BINOP_ADD, PREC_ADD, 0},
  {"-", BINOP_SUB, PREC_ADD, 0},
  {"~", BINOP_CONCAT, PREC_ADD, 0},
  {"*", BINOP_MUL, PREC_MUL, 0},
  {"/", BINOP_DIV, PREC_MUL, 0},
  {"%", BINOP_REM, PREC_MUL, 0},
  {"^^", BINOP_EXP, PREC_REPEAT, 0},
  {"@", BINOP_REPEAT, PREC_REPEAT, 0},
  {"-", UNOP_NEG, PREC_PREFIX, 0},
  {"!", UNOP_LOGICAL_NOT, PREC_PREFIX, 0},
  {"~", UNOP_COMPLEMENT, PREC_PREFIX, 0},
  {"*", UNOP_IND, PREC_PREFIX, 0},
  {"&", UNOP_ADDR, PREC_PREFIX, 0},
  {"sizeof ", UNOP_SIZEOF, PREC_PREFIX, 0},
  {"++", UNOP_PREINCREMENT, PREC_PREFIX, 0},
  {"--", UNOP_PREDECREMENT, PREC_PREFIX, 0},
  {NULL, OP_NULL, PREC_PREFIX, 0}
};

/* Mapping of all D basic data types into the language vector.  */

enum d_primitive_types {
  d_primitive_type_void,
  d_primitive_type_bool,
  d_primitive_type_byte,
  d_primitive_type_ubyte,
  d_primitive_type_short,
  d_primitive_type_ushort,
  d_primitive_type_int,
  d_primitive_type_uint,
  d_primitive_type_long,
  d_primitive_type_ulong,
  d_primitive_type_cent,    /* Signed 128 bit integer.  */
  d_primitive_type_ucent,   /* Unsigned 128 bit integer.  */
  d_primitive_type_float,
  d_primitive_type_double,
  d_primitive_type_real,
  d_primitive_type_ifloat,  /* Imaginary float types.  */
  d_primitive_type_idouble,
  d_primitive_type_ireal,
  d_primitive_type_cfloat,  /* Complex number of two float values.  */
  d_primitive_type_cdouble,
  d_primitive_type_creal,
  d_primitive_type_char,    /* Unsigned character types.  */
  d_primitive_type_wchar,
  d_primitive_type_dchar,
  nr_d_primitive_types
};

/* Implements the la_language_arch_info language_defn routine
   for language D.  */

static void
d_language_arch_info (struct gdbarch *gdbarch,
		      struct language_arch_info *lai)
{
  const struct builtin_d_type *builtin = builtin_d_type (gdbarch);

  lai->string_char_type = builtin->builtin_char;
  lai->primitive_type_vector
    = GDBARCH_OBSTACK_CALLOC (gdbarch, nr_d_primitive_types + 1,
			      struct type *);

  lai->primitive_type_vector [d_primitive_type_void]
    = builtin->builtin_void;
  lai->primitive_type_vector [d_primitive_type_bool]
    = builtin->builtin_bool;
  lai->primitive_type_vector [d_primitive_type_byte]
    = builtin->builtin_byte;
  lai->primitive_type_vector [d_primitive_type_ubyte]
    = builtin->builtin_ubyte;
  lai->primitive_type_vector [d_primitive_type_short]
    = builtin->builtin_short;
  lai->primitive_type_vector [d_primitive_type_ushort]
    = builtin->builtin_ushort;
  lai->primitive_type_vector [d_primitive_type_int]
    = builtin->builtin_int;
  lai->primitive_type_vector [d_primitive_type_uint]
    = builtin->builtin_uint;
  lai->primitive_type_vector [d_primitive_type_long]
    = builtin->builtin_long;
  lai->primitive_type_vector [d_primitive_type_ulong]
    = builtin->builtin_ulong;
  lai->primitive_type_vector [d_primitive_type_cent]
    = builtin->builtin_cent;
  lai->primitive_type_vector [d_primitive_type_ucent]
    = builtin->builtin_ucent;
  lai->primitive_type_vector [d_primitive_type_float]
    = builtin->builtin_float;
  lai->primitive_type_vector [d_primitive_type_double]
    = builtin->builtin_double;
  lai->primitive_type_vector [d_primitive_type_real]
    = builtin->builtin_real;
  lai->primitive_type_vector [d_primitive_type_ifloat]
    = builtin->builtin_ifloat;
  lai->primitive_type_vector [d_primitive_type_idouble]
    = builtin->builtin_idouble;
  lai->primitive_type_vector [d_primitive_type_ireal]
    = builtin->builtin_ireal;
  lai->primitive_type_vector [d_primitive_type_cfloat]
    = builtin->builtin_cfloat;
  lai->primitive_type_vector [d_primitive_type_cdouble]
    = builtin->builtin_cdouble;
  lai->primitive_type_vector [d_primitive_type_creal]
    = builtin->builtin_creal;
  lai->primitive_type_vector [d_primitive_type_char]
    = builtin->builtin_char;
  lai->primitive_type_vector [d_primitive_type_wchar]
    = builtin->builtin_wchar;
  lai->primitive_type_vector [d_primitive_type_dchar]
    = builtin->builtin_dchar;

  lai->bool_type_symbol = "bool";
  lai->bool_type_default = builtin->builtin_bool;
}

/* Return non-zero if TYPE is a dynamic array.
   We assume CHECK_TYPEDEF has already been done.  */

static int
dynamic_array_p (struct type *type)
{
  /* D dynamic arrays don't necessarily have a name we can use.  */
  if (TYPE_CODE (type) == TYPE_CODE_STRUCT
      && TYPE_NFIELDS (type) == 2)
    {
      struct type *type0 = TYPE_FIELD_TYPE (type, 0);
      struct type *type1 = TYPE_FIELD_TYPE (type, 1);

      type0 = check_typedef (type0);
      type1 = check_typedef (type1);

      if (TYPE_CODE (type0) == TYPE_CODE_INT
	  && strcmp (TYPE_FIELD_NAME (type, 0), "length") == 0
	  && TYPE_CODE (type1) == TYPE_CODE_PTR
	  && strcmp (TYPE_FIELD_NAME (type, 1), "ptr") == 0)
	return 1;
    }

  return 0;
}

/* Given that PTR is a pointer or the ptr component of a dynamic array,
   returns the D slice of HIGH-LOW elements starting at index LOW.  */
static struct value *
d_value_slice_ptr (struct value *ptr, struct type *index_type,
		   LONGEST low, LONGEST high)
{
  struct type *base_type;
  struct type *range_type;
  struct type *slice_type;
  CORE_ADDR base;

  base_type = TYPE_TARGET_TYPE (value_type (ptr));
  range_type = create_static_range_type (NULL, index_type,
					 low, high - 1);
  slice_type = create_array_type (NULL, base_type, range_type);

  base = value_as_address (ptr)
    + (longest_to_int (low) * TYPE_LENGTH (base_type));

  return value_at_lazy (slice_type, base);
}

/* Expression evaluator for the D language family.  Most operations
   are delegated to evaluate_subexp_standard; see that function for a
   description of the arguments.  */

static struct value *
evaluate_subexp_d (struct type *expect_type, struct expression *exp,
		   int *pos, enum noside noside)
{
  enum exp_opcode op;
  int pc = *pos;

  *pos += 1;
  op = exp->elts[pc].opcode;

  switch (op)
    {
    case BINOP_SUBSCRIPT:
      {
	struct value *array = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	struct value *index_val =
	  evaluate_subexp (NULL_TYPE, exp, pos, noside);
	struct type *type;

	if (noside == EVAL_SKIP)
	  goto nosideret;

	type = value_type (array);
	type = check_typedef (type);

	if (dynamic_array_p (type))
	  {
	    /* Indexing a dynamic array.  Extract the underlying length
	       and pointer fields.  Checks array bounds as per D rules.  */
	    struct value *ptr = value_struct_elt (&array, NULL, "ptr", NULL,
						  _("D dynamic array"));
	    struct value *length = value_struct_elt (&array, NULL,
						     "length", NULL,
						     _("D dynamic array"));
	    LONGEST upper_bound = value_as_long (length);
	    LONGEST index = value_as_long (index_val);

	    if (index < 0 || upper_bound <= index)
	      error (_("index out of range"));

	    return value_ind (value_ptradd (ptr, index));
	  }

	*pos = pc;
	return evaluate_subexp_c (expect_type, exp, pos, noside);
      }

    case TERNOP_SLICE:
      {
	/* Slicing a dynamic array, static array or pointer, return the
	   D slice of HIGH-LOW elements starting at index LOW.  */
	struct value *array = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	struct value *low_bound_val =
	  evaluate_subexp (NULL_TYPE, exp, pos, noside);
	struct value *high_bound_val =
	  evaluate_subexp (NULL_TYPE, exp, pos, noside);
	LONGEST low_bound;
	LONGEST high_bound;
	struct type *type;

	if (noside == EVAL_SKIP)
	  goto nosideret;

	low_bound = value_as_long (low_bound_val);
	high_bound = value_as_long (high_bound_val);

	type = value_type (array);
	type = check_typedef (type);

	if (TYPE_CODE (type) == TYPE_CODE_PTR)
	  {
	    /* Slicing a pointer, check array bounds as per D rules.  */
	    if (high_bound < low_bound)
	      error (_("slice out of range"));

	    return d_value_slice_ptr (array,
				      builtin_d_type (exp->gdbarch)->builtin_uint,
				      low_bound, high_bound);
	  }
	else if (dynamic_array_p (type))
	  {
	    /* Slicing a dynamic array.  Extract the underlying length
	       and pointer fields.  Checks array bounds as per D rules.  */
	    struct value *ptr = value_struct_elt (&array, NULL, "ptr", NULL,
						  _("D dynamic array"));
	    struct value *length = value_struct_elt (&array, NULL,
						     "length", NULL,
						     _("D dynamic array"));
	    LONGEST upper_bound = value_as_long (length);

	    if (low_bound < 0
		|| high_bound < low_bound
		|| upper_bound < high_bound)
	      error (_("slice out of range"));

	    return d_value_slice_ptr (ptr, value_type (length),
				      low_bound, high_bound);
	  }
	else
	  return value_slice (array, low_bound, high_bound - low_bound);
      }

    default:
      *pos -= 1;
      return evaluate_subexp_c (expect_type, exp, pos, noside);
    }

nosideret:
  return value_from_longest (builtin_type (exp->gdbarch)->builtin_int, 1);
}


static const struct exp_descriptor exp_descriptor_d =
{
  print_subexp_standard,
  operator_length_standard,
  operator_check_standard,
  op_name_standard,
  dump_subexp_body_standard,
  evaluate_subexp_d
};

static const char *d_extensions[] =
{
  ".d", NULL
};

extern const struct language_defn d_language_defn =
{
  "d",
  "D",
  language_d,
  range_check_off,
  case_sensitive_on,
  array_row_major,
  macro_expansion_no,
  d_extensions,
  &exp_descriptor_d,
  d_parse,
  null_post_parser,
  c_printchar,			/* Print a character constant.  */
  c_printstr,			/* Function to print string constant.  */
  c_emit_char,			/* Print a single char.  */
  d_print_type,			/* Print a type using appropriate syntax.  */
  c_print_typedef,		/* Print a typedef using appropriate
				   syntax.  */
  d_val_print,			/* Print a value using appropriate syntax.  */
  c_value_print,		/* Print a top-level value.  */
  default_read_var_value,	/* la_read_var_value */
  NULL,				/* Language specific skip_trampoline.  */
  "this",
  false,			/* la_store_sym_names_in_linkage_form_p */
  d_lookup_symbol_nonlocal,
  basic_lookup_transparent_type,
  d_demangle,			/* Language specific symbol demangler.  */
  d_sniff_from_mangled_name,
  NULL,				/* Language specific
				   class_name_from_physname.  */
  d_op_print_tab,		/* Expression operators for printing.  */
  1,				/* C-style arrays.  */
  0,				/* String lower bound.  */
  default_word_break_characters,
  default_collect_symbol_completion_matches,
  d_language_arch_info,
  default_print_array_index,
  default_pass_by_reference,
  c_get_string,
  c_watch_location_expression,
  NULL,				/* la_get_symbol_name_matcher */
  iterate_over_symbols,
  default_search_name_hash,
  &default_varobj_ops,
  NULL,
  NULL,
  c_is_string_type_p,
  "{...}"			/* la_struct_too_deep_ellipsis */
};

/* Build all D language types for the specified architecture.  */

static void *
build_d_types (struct gdbarch *gdbarch)
{
  struct builtin_d_type *builtin_d_type
    = GDBARCH_OBSTACK_ZALLOC (gdbarch, struct builtin_d_type);

  /* Basic types.  */
  builtin_d_type->builtin_void
    = arch_type (gdbarch, TYPE_CODE_VOID, TARGET_CHAR_BIT, "void");
  builtin_d_type->builtin_bool
    = arch_boolean_type (gdbarch, 8, 1, "bool");
  builtin_d_type->builtin_byte
    = arch_integer_type (gdbarch, 8, 0, "byte");
  builtin_d_type->builtin_ubyte
    = arch_integer_type (gdbarch, 8, 1, "ubyte");
  builtin_d_type->builtin_short
    = arch_integer_type (gdbarch, 16, 0, "short");
  builtin_d_type->builtin_ushort
    = arch_integer_type (gdbarch, 16, 1, "ushort");
  builtin_d_type->builtin_int
    = arch_integer_type (gdbarch, 32, 0, "int");
  builtin_d_type->builtin_uint
    = arch_integer_type (gdbarch, 32, 1, "uint");
  builtin_d_type->builtin_long
    = arch_integer_type (gdbarch, 64, 0, "long");
  builtin_d_type->builtin_ulong
    = arch_integer_type (gdbarch, 64, 1, "ulong");
  builtin_d_type->builtin_cent
    = arch_integer_type (gdbarch, 128, 0, "cent");
  builtin_d_type->builtin_ucent
    = arch_integer_type (gdbarch, 128, 1, "ucent");
  builtin_d_type->builtin_float
    = arch_float_type (gdbarch, gdbarch_float_bit (gdbarch),
		       "float", gdbarch_float_format (gdbarch));
  builtin_d_type->builtin_double
    = arch_float_type (gdbarch, gdbarch_double_bit (gdbarch),
		       "double", gdbarch_double_format (gdbarch));
  builtin_d_type->builtin_real
    = arch_float_type (gdbarch, gdbarch_long_double_bit (gdbarch),
		       "real", gdbarch_long_double_format (gdbarch));

  TYPE_INSTANCE_FLAGS (builtin_d_type->builtin_byte)
    |= TYPE_INSTANCE_FLAG_NOTTEXT;
  TYPE_INSTANCE_FLAGS (builtin_d_type->builtin_ubyte)
    |= TYPE_INSTANCE_FLAG_NOTTEXT;

  /* Imaginary and complex types.  */
  builtin_d_type->builtin_ifloat
    = arch_float_type (gdbarch, gdbarch_float_bit (gdbarch),
		       "ifloat", gdbarch_float_format (gdbarch));
  builtin_d_type->builtin_idouble
    = arch_float_type (gdbarch, gdbarch_double_bit (gdbarch),
		       "idouble", gdbarch_double_format (gdbarch));
  builtin_d_type->builtin_ireal
    = arch_float_type (gdbarch, gdbarch_long_double_bit (gdbarch),
		       "ireal", gdbarch_long_double_format (gdbarch));
  builtin_d_type->builtin_cfloat
    = arch_complex_type (gdbarch, "cfloat",
			 builtin_d_type->builtin_float);
  builtin_d_type->builtin_cdouble
    = arch_complex_type (gdbarch, "cdouble",
			 builtin_d_type->builtin_double);
  builtin_d_type->builtin_creal
    = arch_complex_type (gdbarch, "creal",
			 builtin_d_type->builtin_real);

  /* Character types.  */
  builtin_d_type->builtin_char
    = arch_character_type (gdbarch, 8, 1, "char");
  builtin_d_type->builtin_wchar
    = arch_character_type (gdbarch, 16, 1, "wchar");
  builtin_d_type->builtin_dchar
    = arch_character_type (gdbarch, 32, 1, "dchar");

  return builtin_d_type;
}

static struct gdbarch_data *d_type_data;

/* Return the D type table for the specified architecture.  */

const struct builtin_d_type *
builtin_d_type (struct gdbarch *gdbarch)
{
  return (const struct builtin_d_type *) gdbarch_data (gdbarch, d_type_data);
}

void
_initialize_d_language (void)
{
  d_type_data = gdbarch_data_register_post_init (build_d_types);
}
