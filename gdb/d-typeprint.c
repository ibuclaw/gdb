/* Support for printing D types for GDB, the GNU debugger.
   Copyright (C) 2015 Free Software Foundation, Inc.

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
#include "gdbtypes.h"
#include "demangle.h"
#include "d-lang.h"
#include "typeprint.h"
#include "c-lang.h"
#include "cp-support.h"

/* Print the name of the type (or the ultimate pointer target,
   function value or array element), or the description of a
   structure or union.

   SHOW positive means print details about the type (e.g. enum
   values), and print structure elements passing SHOW - 1 for show.

   SHOW negative means just print the type name or struct tag if there
   is one.  If there is no name, print something sensible but concise
   like "struct {...}".

   SHOW zero means just print the type name or struct tag if there is
   one.  If there is no name, print something sensible but not as
   concise like "struct {int x; int y;}".

   LEVEL is the number of spaces to indent by.
   We increase it for some recursive calls.  */

static void
d_type_print_base (struct type *type, struct ui_file *stream, int show,
		   int level, const struct type_print_options *flags)
{
  QUIT;
  wrap_here ("    ");

  if (type == NULL)
    {
      fputs_filtered ("<type unknown>", stream);
      return;
    }

  /* When SHOW is zero or less, and there is a valid type name, then always
     just print the type name directly from the type.  */
  if ((show <= 0) && (TYPE_NAME (type) != NULL))
    {
      fputs_filtered (TYPE_NAME (type), stream);
      return;
    }

  if (TYPE_CODE (type) != TYPE_CODE_TYPEDEF)
    type = check_typedef (type);

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_TYPEDEF:
      d_type_print_base (TYPE_TARGET_TYPE (type), stream, 0, level, flags);
      break;

    case TYPE_CODE_ARRAY:
      {
	LONGEST low_bound, high_bound;
	int is_vector = TYPE_VECTOR (type);

	if (is_vector)
	  fputs_filtered ("__vector(", stream);

	d_type_print_base (TYPE_TARGET_TYPE (type), stream, 0, level, flags);
	fputs_filtered ("[", stream);
	/* Bounds are not yet resolved, print a bounds placeholder instead.  */
	if (TYPE_HIGH_BOUND_KIND (TYPE_INDEX_TYPE (type)) == PROP_LOCEXPR
	    || TYPE_HIGH_BOUND_KIND (TYPE_INDEX_TYPE (type)) == PROP_LOCLIST)
	fputs_filtered ("variable length", stream);
	else if (get_array_bounds (type, &low_bound, &high_bound))
	  fprintf_filtered (stream, "%s",
			    plongest (high_bound - low_bound + 1));
	fputs_filtered ("]", stream);

	if (is_vector)
	  fputs_filtered (")", stream);
      }
      break;

    case TYPE_CODE_PTR:
      {
	struct type *target = TYPE_TARGET_TYPE (type);
	int is_funcptr = (target != NULL
			  && TYPE_CODE (target) == TYPE_CODE_FUNC);
	int is_class = (target != NULL
			&& TYPE_CODE (target) == TYPE_CODE_STRUCT
			&& TYPE_DECLARED_CLASS (target));

	/* Want to show the contents of top-level class pointers.  */
	if (show > 0 && !is_class)
	  show = 0;

	d_type_print_base (target, stream, show, level, flags);

	/* Function pointers have their own syntax.  */
	if (is_funcptr)
	  {
	    int i;

	    fputs_filtered (" function(", stream);
	    for (i = 0; i < TYPE_NFIELDS (target); i++)
	      {
		struct field f = TYPE_FIELDS (target)[i];

		if (i > 0)
		  fputs_filtered (", ", stream);

		d_type_print_base (FIELD_TYPE (f), stream, 0, level, flags);
	      }
	    fputs_filtered (")", stream);
	  }
	else
	  {
	    /* Skip emitting the '*' for classes, as they are all represented
	       as pointer types in D.  */
	    if (!is_class)
	      fprintf_filtered (stream, "*");
	  }
      }
      break;

    case TYPE_CODE_REF:
      {
	fputs_filtered ("ref ", stream);
	d_type_print_base (TYPE_TARGET_TYPE (type), stream,
			   show, level, flags);
      }
      break;

    case TYPE_CODE_FUNC:
      d_type_print_base (TYPE_TARGET_TYPE (type), stream,
			 show, level, flags);
      break;

    case TYPE_CODE_STRUCT:
    case TYPE_CODE_UNION:
      {
	const char *tagname = TYPE_TAG_NAME (type);

	if (tagname != NULL)
	  {
	    char lastchar = tagname[strlen (tagname) - 1];

	    fputs_filtered (tagname, stream);
	    /* If it's a dynamic or associative array, or a delegate function,
	       don't print the underlying type structure.  */
	    if (lastchar == ']' || lastchar == ')')
	      break;

	    if (show > 0)
	      fputs_filtered (" ", stream);
	  }
	else
	  {
	    if (TYPE_CODE (type) == TYPE_CODE_UNION)
	      fputs_filtered ("union ", stream);
	    else if (TYPE_DECLARED_CLASS (type))
	      fputs_filtered ("class ", stream);
	    else
	      fputs_filtered ("struct ", stream);
	  }

	wrap_here ("    ");

	if (show < 0)
	  {
	    /* If we just printed a tag name, no need to print anything else.  */
	    if (tagname == NULL)
	      fprintf_filtered (stream, "{...}");
	  }
	else if (show > 0 || TYPE_TAG_NAME (type) == NULL)
	  {
	    int len;
	    int i, j;

	    // FIXME: print inheritance / interfaces

	    fputs_filtered ("{\n", stream);
	    if ((TYPE_NFIELDS (type) == 0) && (TYPE_NFN_FIELDS (type) == 0))
	      {
		if (TYPE_STUB (type))
		  fprintfi_filtered (level + 4, stream, "<incomplete type>\n");
		else
		  fprintfi_filtered (level + 4, stream, "<no data fields>\n");
	      }

	    QUIT;

	    /* If there is a base class for this type,
	       do not print the field that it occupies.  */
	    len = TYPE_NFIELDS (type);
	    for (i = TYPE_N_BASECLASSES (type); i < len; i++)
	      {
		QUIT;
		/* Don't print out virtual function table or monitor.
		   These are the first two fields placed in the class,
		   and should be marked as artificial.  */
		if (i < 2 && TYPE_DECLARED_CLASS (type))
		  {
		    if (TYPE_FIELD_ARTIFICIAL (type, i))
		      continue;
		    else if (startswith (TYPE_FIELD_NAME (type, i), "__vptr"))
		      continue;
		    else if (startswith (TYPE_FIELD_NAME (type, i), "__monitor"))
		      continue;
		  }

		print_spaces_filtered (level + 4, stream);

		if (TYPE_FIELD_PROTECTED (type, i))
		  fputs_filtered ("protected ", stream);
		else if (TYPE_FIELD_PRIVATE (type, i))
		  fputs_filtered ("private ", stream);

		if (field_is_static (&TYPE_FIELD (type, i)))
		  fputs_filtered ("static ", stream);

		d_print_type (TYPE_FIELD_TYPE (type, i),
			      TYPE_FIELD_NAME (type, i),
			      stream, show - 1, level + 4, flags);
		fputs_filtered (";\n", stream);
	      }

	    /* Print out the methods, artificial methods will be hidden.  */
	    len = TYPE_NFN_FIELDS (type);
	    if (!flags->print_methods)
	      len = 0;

	    for (i = 0; i < len; i++)
	      {
		for (j = 0; j < TYPE_FN_FIELDLIST_LENGTH (type, i); j++)
		  {
		    struct fn_field *fields = TYPE_FN_FIELDLIST1 (type, i);
		    const char *method_name;
		    char *mangled_name;
		    char *demangled_name;
		    struct type *target;

		    /* Do not print out artificial methods.  */
		    if (TYPE_FN_FIELD_ARTIFICIAL (fields, j))
		      continue;

		    method_name = TYPE_FN_FIELDLIST_NAME (type, i);

		    /* Build something we can demangle.  */
		    if (TYPE_FN_FIELD_STUB (fields, j))
		      mangled_name = gdb_mangle_name (type, i, j);
		    else
		      {
			const char *physname;
			int physname_len;

			physname = TYPE_FN_FIELD_PHYSNAME (fields, j);
			physname_len = strlen(physname);

			mangled_name = (char *) alloca (physname_len + 1);
			memcpy (mangled_name, physname, physname_len);
			mangled_name[physname_len] = '\0';
		      }

		    QUIT;
		    print_spaces_filtered (level + 4, stream);

		    if (TYPE_FN_FIELD_PROTECTED (fields, j))
		      fputs_filtered ("protected ", stream);
		    else if (TYPE_FN_FIELD_PRIVATE (fields, j))
		      fputs_filtered ("private ", stream);

		    if (TYPE_FN_FIELD_ABSTRACT (fields, j))
		      fputs_filtered ("abstract ", stream);
		    if (TYPE_FN_FIELD_STATIC (fields, j))
		      fputs_filtered ("static ", stream);
		    if (TYPE_FN_FIELD_FINAL (fields, j))
		      fputs_filtered ("final ", stream);
		    if (TYPE_FN_FIELD_SYNCHRONIZED (fields, j))
		      fputs_filtered ("synchronized ", stream);

		    target = TYPE_TARGET_TYPE (TYPE_FN_FIELD_TYPE (fields, j));
		    if (target == NULL)
		      {
			/* Keep GDB from crashing here.  */
			fprintf_filtered (stream, "<undefined type> %s;\n",
					  TYPE_FN_FIELD_PHYSNAME (fields, j));
			break;
		      }
		    else if (strcmp (method_name, "__ctor") != 0
			     && strcmp (method_name, "__dtor") != 0
			     && strcmp (method_name, "__postblit") != 0)
		      {
			d_print_type (target, "", stream, -1, 0, flags);
			fputs_filtered (" ", stream);
		      }

		    demangled_name = gdb_demangle (mangled_name, DMGL_DLANG);

		    if (demangled_name != NULL)
		      {
			/* Skip to the base name of the demangled symbol.  */
			char *p = strchr (demangled_name, '(');
			gdb_assert (p != NULL);
			while (p > demangled_name && p[-1] != '.')
			  p--;

			fputs_filtered (p, stream);
			xfree (demangled_name);
		      }
		    else
		      fputs_filtered (mangled_name, stream);

		    if (TYPE_FN_FIELD_STUB (fields, j))
		      xfree (mangled_name);

		    fputs_filtered (";\n", stream);
		  }
	      }

	    fprintfi_filtered (level, stream, "}");
	  }
      }
      break;

    case TYPE_CODE_MODULE:
      fprintf_filtered (stream, "module %s", TYPE_TAG_NAME (type));
      break;

    default:
      /* Handle types not explicitly handled by the other cases,
         such as fundamental types.  For these, just print whatever
         the type name is, as recorded in the type itself.  If there
         is no type name, then hand it down to c_type_print_base.  */
      if (TYPE_NAME (type) != NULL)
	fprintfi_filtered (level, stream, "%s", TYPE_NAME (type));
      else
	c_type_print_base (type, stream, show, level, flags);
      break;
    }
}

/* LEVEL is the depth to indent lines by.  */

void
d_print_type (struct type *type, const char *varstring,
	      struct ui_file *stream, int show, int level,
	      const struct type_print_options *flags)
{
  d_type_print_base (type, stream, show, level, flags);

  if (varstring != NULL && *varstring != '\0')
    {
      fputs_filtered (" ", stream);
      fputs_filtered (varstring, stream);
    }
}

