/*
  Modifications for FPC support
  $Id$
*/
/* Support for printing C and C++ types for GDB, the GNU debugger.
   Copyright 1986, 1988, 1989, 1991, 1993, 1994, 1995, 1996
   Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "defs.h"
#include "obstack.h"
#include "bfd.h"		/* Binary File Description */
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "value.h"
#include "gdbcore.h"
#include "target.h"
#include "command.h"
#include "gdbcmd.h"
#include "language.h"
#include "demangle.h"
#include "p-lang.h"
#include "typeprint.h"

#include "gdb_string.h"
#include <errno.h>
#include <ctype.h>

static void
pas_type_print_args PARAMS ((struct type *, GDB_FILE *));

static void
pas_type_print_varspec_suffix PARAMS ((struct type *, GDB_FILE *, int, int, int));

static void
pcpas_type_print_derivation_info PARAMS ((GDB_FILE *, struct type *));

void
pas_type_print_varspec_prefix PARAMS ((struct type *, GDB_FILE *, int, int));


/* LEVEL is the depth to indent lines by.  */

void
pas_print_type (type, varstring, stream, show, level)
     struct type *type;
     char *varstring;
     GDB_FILE *stream;
     int show;
     int level;
{
  register enum type_code code;
  int demangled_args;

  code = TYPE_CODE (type);

  if (show > 0)
    CHECK_TYPEDEF (type);

  if ((code == TYPE_CODE_FUNC ||
       code == TYPE_CODE_METHOD))
     {
      pas_type_print_varspec_prefix (type, stream, show, 0);
     }
  /* first the name */
  fputs_filtered (varstring, stream);

  if ((varstring != NULL && *varstring != '\0') &&
      !(code == TYPE_CODE_FUNC ||
        code == TYPE_CODE_METHOD))
    {
    fputs_filtered (" : ", stream);
    }

  if (! (code == TYPE_CODE_FUNC ||
        code == TYPE_CODE_METHOD))
     {
      pas_type_print_varspec_prefix (type, stream, show, 0);
     }

  pas_type_print_base (type, stream, show, level);
  /* For demangled function names, we have the arglist as part of the name,
     so don't print an additional pair of ()'s */

  demangled_args = varstring ? strchr(varstring, '(') != NULL : NULL;
  pas_type_print_varspec_suffix (type, stream, show, 0, demangled_args);

}
  
/* If TYPE is a derived type, then print out derivation information.
   Print only the actual base classes of this type, not the base classes
   of the base classes.  I.E.  for the derivation hierarchy:

	class A { int a; };
	class B : public A {int b; };
	class C : public B {int c; };

   Print the type of class C as:

   	class C : public B {
		int c;
	}

   Not as the following (like gdb used to), which is not legal C++ syntax for
   derived types and may be confused with the multiple inheritance form:

	class C : public B : public A {
		int c;
	}

   In general, gdb should try to print the types as closely as possible to
   the form that they appear in the source code. */

static void
pcpas_type_print_derivation_info (stream, type)
     GDB_FILE *stream;
     struct type *type;
{
  char *name;
  int i;

  for (i = 0; i < TYPE_N_BASECLASSES (type); i++)
    {
      fputs_filtered (i == 0 ? ": " : ", ", stream);
      fprintf_filtered (stream, "%s%s ",
			BASETYPE_VIA_PUBLIC (type, i) ? "public" : "private",
			BASETYPE_VIA_VIRTUAL(type, i) ? " virtual" : "");
      name = type_name_no_tag (TYPE_BASECLASS (type, i));
      fprintf_filtered (stream, "%s", name ? name : "(null)");
    }
  if (i > 0)
    {
      fputs_filtered (" ", stream);
    }
}

/* Print the FPC method arguments ARGS to the file STREAM.  */

void
pas_type_print_method_args (physname, methodname, stream)
     char *physname;
     char *methodname;
     GDB_FILE *stream;
{
  int is_constructor = STREQN (physname, "__ct__", 6);
  int is_destructor = STREQN (physname, "__dt__", 6);

  if (is_constructor || is_destructor)
    {
     physname+=6;
    }

  fputs_filtered (methodname, stream);

  if (physname && (*physname != 0))
    {
      int i = 0;
      int len = 0;
      char storec;
      char *argname;
      fputs_filtered (" (", stream);
      /* we must demangle this */
      while isdigit(physname[0])
        {
         while isdigit (physname[len])
           {
            len++;
           }
         i = strtol(physname,&argname,0);
         physname+=len;
         storec = physname[i];
         physname[i] = 0;
         fputs_filtered (physname, stream);
         physname[i] = storec;
         physname+=i;
         if (physname[0] != 0)
           {
            fputs_filtered (", ", stream);
           }
        }
      fputs_filtered (")", stream);
    }
}
  
/* Print any asterisks or open-parentheses needed before the
   variable name (to describe its type).

   On outermost call, pass 0 for PASSED_A_PTR.
   On outermost call, SHOW > 0 means should ignore
   any typename for TYPE and show its details.
   SHOW is always zero on recursive calls.  */

void
pas_type_print_varspec_prefix (type, stream, show, passed_a_ptr)
     struct type *type;
     GDB_FILE *stream;
     int show;
     int passed_a_ptr;
{
  char *name;
  if (type == 0)
    return;

  if (TYPE_NAME (type) && show <= 0)
    return;

  QUIT;

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_PTR:
      fprintf_filtered (stream, "^");
      pas_type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0, 1);
      break; /* pointer should be handled normally in pascal */

    case TYPE_CODE_MEMBER:
      if (passed_a_ptr)
	fprintf_filtered (stream, "(");
      pas_type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0, 0);
      fprintf_filtered (stream, " ");
      name = type_name_no_tag (TYPE_DOMAIN_TYPE (type));
      if (name)
	fputs_filtered (name, stream);
      else
        pas_type_print_base (TYPE_DOMAIN_TYPE (type), stream, 0, passed_a_ptr);
      fprintf_filtered (stream, "::");
      break;

    case TYPE_CODE_METHOD:
      if (passed_a_ptr)
	fprintf_filtered (stream, "(");
      if ( TYPE_CODE(TYPE_TARGET_TYPE (type)) != TYPE_CODE_VOID)
         {
          fprintf_filtered (stream, "function  ");
         }
      else
         {
          fprintf_filtered (stream, "procedure ");
         }
      
      if (passed_a_ptr)
	{
	  fprintf_filtered (stream, " ");
	  pas_type_print_base (TYPE_DOMAIN_TYPE (type), stream, 0, passed_a_ptr);
	  fprintf_filtered (stream, "::");
	}
      break;

    case TYPE_CODE_REF:
      pas_type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0, 1);
      fprintf_filtered (stream, "&");
      break;

    case TYPE_CODE_FUNC:
      if (passed_a_ptr)
	fprintf_filtered (stream, "(");

      if ( TYPE_CODE(TYPE_TARGET_TYPE (type)) != TYPE_CODE_VOID)
         {
          fprintf_filtered (stream, "function  ");
         }
      else
         {
          fprintf_filtered (stream, "procedure ");
         }
      
      break;

    case TYPE_CODE_ARRAY:
      if (passed_a_ptr)
	fprintf_filtered (stream, "(");
      fprintf_filtered (stream, "array ");
      if (TYPE_LENGTH (type) >= 0 && TYPE_LENGTH (TYPE_TARGET_TYPE (type)) > 0
	  && TYPE_ARRAY_UPPER_BOUND_TYPE(type) != BOUND_CANNOT_BE_DETERMINED)
	fprintf_filtered (stream, "[%d..%d] ",
                         TYPE_ARRAY_LOWER_BOUND_VALUE(type),
                         TYPE_ARRAY_UPPER_BOUND_VALUE(type)
			 );
      fprintf_filtered (stream, "of ");
      break;

    case TYPE_CODE_UNDEF:
    case TYPE_CODE_STRUCT:
    case TYPE_CODE_UNION:
    case TYPE_CODE_ENUM:
    case TYPE_CODE_INT:
    case TYPE_CODE_FLT:
    case TYPE_CODE_VOID:
    case TYPE_CODE_ERROR:
    case TYPE_CODE_CHAR:
    case TYPE_CODE_BOOL:
    case TYPE_CODE_SET:
    case TYPE_CODE_RANGE:
    case TYPE_CODE_STRING:
    case TYPE_CODE_BITSTRING:
    case TYPE_CODE_COMPLEX:
    case TYPE_CODE_TYPEDEF:
      /* These types need no prefix.  They are listed here so that
	 gcc -Wall will reveal any types that haven't been handled.  */
      break;
    }
}

static void
pas_type_print_args (type, stream)
     struct type *type;
     GDB_FILE *stream;
{
  int i;
  struct type **args;

  /*  fprintf_filtered (stream, "(");
    no () for procedures !! */
  args = TYPE_ARG_TYPES (type);
  if (args != NULL)
    {
      if ((args[1] != NULL && args[1]->code != TYPE_CODE_VOID) ||
         (args[2] != NULL))
       {
        fprintf_filtered (stream, "(");
       }
      if (args[1] == NULL)
	{
	  fprintf_filtered (stream, "...");
	}
      else
	{
	  for (i = 1;
	       args[i] != NULL && args[i]->code != TYPE_CODE_VOID;
	       i++)
	    {
	      pas_print_type (args[i], "", stream, -1, 0);
	      if (args[i+1] == NULL)
		{
		  fprintf_filtered (stream, "...");
		}
	      else if (args[i+1]->code != TYPE_CODE_VOID)
		{
		  fprintf_filtered (stream, ",");
		  wrap_here ("    ");
		}
	    }
	}
      if ((args[1] != NULL && args[1]->code != TYPE_CODE_VOID) ||
         (args[2] != NULL))
       {
        fprintf_filtered (stream, ")");
       }
    }
}

static void
pas_print_func_args (struct type *type, GDB_FILE *stream)
     { int i, len = TYPE_NFIELDS (type);
       if (len)
                {
                 fprintf_filtered (stream, "(");
                }
       for (i = 0; i < len; i++)
         {
           if (i > 0)
            {
              fputs_filtered (", ", stream);
              wrap_here ("    ");
            }
                  /*  can we find if it is a var parameter ??
                  if ( TYPE_FIELD(type, i) == )
                    {
                     fprintf_filtered (stream, "var ");
                    } */
           pas_print_type (TYPE_FIELD_TYPE (type, i), "" /* TYPE_FIELD_NAME seems invalid ! */
             , stream, -1, 0);
         }
       if (len)
         {
           fprintf_filtered (stream, ")");
         }
     }
     
/* Print any array sizes, function arguments or close parentheses
   needed after the variable name (to describe its type).
   Args work like pas_type_print_varspec_prefix.  */

static void
pas_type_print_varspec_suffix (type, stream, show, passed_a_ptr, demangled_args)
     struct type *type;
     GDB_FILE *stream;
     int show;
     int passed_a_ptr;
     int demangled_args;
{
  if (type == 0)
    return;

  if (TYPE_NAME (type) && show <= 0)
    return;

  QUIT;

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_ARRAY:
      if (passed_a_ptr)
	fprintf_filtered (stream, ")");
      break;

    case TYPE_CODE_MEMBER:
      if (passed_a_ptr)
	fprintf_filtered (stream, ")");
      pas_type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0, 0, 0);
      break;

    case TYPE_CODE_METHOD:
      if (passed_a_ptr)
	fprintf_filtered (stream, ")");
      pas_type_print_method_args ("",
					       "",
					       stream);
      /* pas_type_print_args (type, stream); */
      if ( TYPE_CODE(TYPE_TARGET_TYPE (type)) != TYPE_CODE_VOID)
         {
          fprintf_filtered (stream, " : ");
          pas_type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0, 0);
          pas_type_print_base (TYPE_TARGET_TYPE (type), stream, show, 0);
          pas_type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0,
				   passed_a_ptr, 0);
         }
      break;

    case TYPE_CODE_PTR:
    case TYPE_CODE_REF:
      pas_type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0, 1, 0);
      break;

    case TYPE_CODE_FUNC:
      if (passed_a_ptr)
	fprintf_filtered (stream, ")");
      if (!demangled_args)
        pas_print_func_args (type, stream);
      if ( TYPE_CODE(TYPE_TARGET_TYPE (type)) != TYPE_CODE_VOID)
         {
          fprintf_filtered (stream, " : ");
          pas_type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0, 0);
          pas_type_print_base (TYPE_TARGET_TYPE (type), stream, show, 0);
          pas_type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0,
				   passed_a_ptr, 0);
         }
      break;

    case TYPE_CODE_UNDEF:
    case TYPE_CODE_STRUCT:
    case TYPE_CODE_UNION:
    case TYPE_CODE_ENUM:
    case TYPE_CODE_INT:
    case TYPE_CODE_FLT:
    case TYPE_CODE_VOID:
    case TYPE_CODE_ERROR:
    case TYPE_CODE_CHAR:
    case TYPE_CODE_BOOL:
    case TYPE_CODE_SET:
    case TYPE_CODE_RANGE:
    case TYPE_CODE_STRING:
    case TYPE_CODE_BITSTRING:
    case TYPE_CODE_COMPLEX:
    case TYPE_CODE_TYPEDEF:
      /* These types do not need a suffix.  They are listed so that
	 gcc -Wall will report types that may not have been considered.  */
      break;
    }
}

/* Print the name of the type (or the ultimate pointer target,
   function value or array element), or the description of a
   structure or union.

   SHOW positive means print details about the type (e.g. enum values),
   and print structure elements passing SHOW - 1 for show.
   SHOW negative means just print the type name or struct tag if there is one.
   If there is no name, print something sensible but concise like
   "struct {...}".
   SHOW zero means just print the type name or struct tag if there is one.
   If there is no name, print something sensible but not as concise like
   "struct {int x; int y;}".

   LEVEL is the number of spaces to indent by.
   We increase it for some recursive calls.  */

void
pas_type_print_base (type, stream, show, level)
     struct type *type;
     GDB_FILE *stream;
     int show;
     int level;
{
  register int i;
  register int len;
  register int lastval;
  enum {s_none, s_public, s_private, s_protected} section_type;
  QUIT;

  wrap_here ("    ");
  if (type == NULL)
    {
      fputs_filtered ("<type unknown>", stream);
      return;
    }

  /* void pointer */
  if ((TYPE_CODE (type) == TYPE_CODE_PTR) && (TYPE_CODE (TYPE_TARGET_TYPE (type)) == TYPE_CODE_VOID))
    {
      fprintf_filtered (stream,
        TYPE_NAME (type) ? TYPE_NAME (type) : "pointer");
      return;
    }
  /* When SHOW is zero or less, and there is a valid type name, then always
     just print the type name directly from the type.  */
  /* If we have "typedef struct foo {. . .} bar;" do we want to print it
     as "struct foo" or as "bar"?  Pick the latter, because C++ folk tend
     to expect things like "class5 *foo" rather than "struct class5 *foo".  */

  if (show <= 0
      && TYPE_NAME (type) != NULL)
    {
      fputs_filtered (TYPE_NAME (type), stream);
      return;
    }

  CHECK_TYPEDEF (type);
	  
  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_TYPEDEF:
    case TYPE_CODE_PTR:
    case TYPE_CODE_MEMBER:
    case TYPE_CODE_REF:
    /* case TYPE_CODE_FUNC:
    case TYPE_CODE_METHOD: */
      pas_type_print_base (TYPE_TARGET_TYPE (type), stream, show, level);
      break;

    case TYPE_CODE_ARRAY:
      /* pas_type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0, 0);
      pas_type_print_base (TYPE_TARGET_TYPE (type), stream, show, level);
      pas_type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0, 0, 0); */
      pas_print_type(TYPE_TARGET_TYPE (type), NULL, stream, 0, 0);
      break;

    case TYPE_CODE_FUNC:
    case TYPE_CODE_METHOD:
      /*
      pas_type_print_base (TYPE_TARGET_TYPE (type), stream, show, level);
       only after args !! */
      break;
    case TYPE_CODE_STRUCT:
      if (TYPE_TAG_NAME (type) != NULL)
	{
	  fputs_filtered (TYPE_TAG_NAME (type), stream);
          fputs_filtered (" = ", stream);
	}
      if (HAVE_CPLUS_STRUCT (type))
	{
	  fprintf_filtered (stream, "class ");
	}
      else
	{
	  fprintf_filtered (stream, "record ");
	}
      goto struct_union;

    case TYPE_CODE_UNION:
      if (TYPE_TAG_NAME (type) != NULL)
	{
	  fputs_filtered (TYPE_TAG_NAME (type), stream);
          fputs_filtered (" = ", stream);
	}
      fprintf_filtered (stream, "case <?> of ");

    struct_union:
      wrap_here ("    ");
      if (show < 0)
	{
	  /* If we just printed a tag name, no need to print anything else.  */
	  if (TYPE_TAG_NAME (type) == NULL)
	    fprintf_filtered (stream, "{...}");
	}
      else if (show > 0 || TYPE_TAG_NAME (type) == NULL)
	{
	  pcpas_type_print_derivation_info (stream, type);
	  
	  fprintf_filtered (stream, "\n");
	  if ((TYPE_NFIELDS (type) == 0) && (TYPE_NFN_FIELDS (type) == 0))
	    {
	      if (TYPE_FLAGS (type) & TYPE_FLAG_STUB)
		fprintfi_filtered (level + 4, stream, "<incomplete type>\n");
	      else
		fprintfi_filtered (level + 4, stream, "<no data fields>\n");
	    }

	  /* Start off with no specific section type, so we can print
	     one for the first field we find, and use that section type
	     thereafter until we find another type. */

	  section_type = s_none;

	  /* If there is a base class for this type,
	     do not print the field that it occupies.  */

	  len = TYPE_NFIELDS (type);
	  for (i = TYPE_N_BASECLASSES (type); i < len; i++)
	    {
	      QUIT;
	      /* Don't print out virtual function table.  */
	      if (STREQN (TYPE_FIELD_NAME (type, i), "_vptr", 5)
		  && is_cplus_marker ((TYPE_FIELD_NAME (type, i))[5]))
		continue;

	      /* If this is a C++ class we can print the various C++ section
		 labels. */

	      if (HAVE_CPLUS_STRUCT (type))
		{
		  if (TYPE_FIELD_PROTECTED (type, i))
		    {
		      if (section_type != s_protected)
			{
			  section_type = s_protected;
			  fprintfi_filtered (level + 2, stream,
					     "protected\n");
			}
		    }
		  else if (TYPE_FIELD_PRIVATE (type, i))
		    {
		      if (section_type != s_private)
			{
			  section_type = s_private;
			  fprintfi_filtered (level + 2, stream, "private\n");
			}
		    }
		  else
		    {
		      if (section_type != s_public)
			{
			  section_type = s_public;
			  fprintfi_filtered (level + 2, stream, "public\n");
			}
		    }
		}

	      print_spaces_filtered (level + 4, stream);
	      if (TYPE_FIELD_STATIC (type, i))
		{
		  fprintf_filtered (stream, "static ");
		}
	      pas_print_type (TYPE_FIELD_TYPE (type, i),
			    TYPE_FIELD_NAME (type, i),
			    stream, show - 1, level + 4);
	      if (!TYPE_FIELD_STATIC (type, i)
		  && TYPE_FIELD_PACKED (type, i))
		{
		  /* It is a bitfield.  This code does not attempt
		     to look at the bitpos and reconstruct filler,
		     unnamed fields.  This would lead to misleading
		     results if the compiler does not put out fields
		     for such things (I don't know what it does).  */
		  fprintf_filtered (stream, " : %d",
				    TYPE_FIELD_BITSIZE (type, i));
		}
	      fprintf_filtered (stream, ";\n");
	    }

	  /* If there are both fields and methods, put a space between. */
	  len = TYPE_NFN_FIELDS (type);
	  if (len && section_type != s_none)
	     fprintf_filtered (stream, "\n");

	  /* C++: print out the methods */

	  for (i = 0; i < len; i++)
	    {
	      struct fn_field *f = TYPE_FN_FIELDLIST1 (type, i);
	      int j, len2 = TYPE_FN_FIELDLIST_LENGTH (type, i);
	      char *method_name = TYPE_FN_FIELDLIST_NAME (type, i);
	      char *name = type_name_no_tag (type);
              /* this is C++ specific
                how can we know constructor/destructor ?? */
	      for (j = 0; j < len2; j++)
		{
		  char *physname = TYPE_FN_FIELD_PHYSNAME (f, j);

		  int is_constructor = STREQN (physname, "__ct__", 6);
		  int is_destructor = STREQN (physname, "__dt__", 6);

		  QUIT;
		  if (TYPE_FN_FIELD_PROTECTED (f, j))
		    {
		      if (section_type != s_protected)
			{
			  section_type = s_protected;
			  fprintfi_filtered (level + 2, stream,
					     "protected\n");
			}
		    }
		  else if (TYPE_FN_FIELD_PRIVATE (f, j))
		    {
		      if (section_type != s_private)
			{
			  section_type = s_private;
			  fprintfi_filtered (level + 2, stream, "private\n");
			}
		    }
		  else
		    {
		      if (section_type != s_public)
			{
			  section_type = s_public;
			  fprintfi_filtered (level + 2, stream, "public\n");
			}
		    }

		  print_spaces_filtered (level + 4, stream);
		  if (TYPE_FN_FIELD_STATIC_P (f, j))
		    fprintf_filtered (stream, "static ");
		  if (TYPE_TARGET_TYPE (TYPE_FN_FIELD_TYPE (f, j)) == 0)
		    {
		      /* Keep GDB from crashing here.  */
		      fprintf_filtered (stream, "<undefined type> %s;\n",
			       TYPE_FN_FIELD_PHYSNAME (f, j));
		      break;
		    }

                  if (is_constructor)
                    {
                         fprintf_filtered(stream,"constructor ");
                    }
                  else if (is_destructor)
                    {
                         fprintf_filtered(stream,"destructor  ");
                    }
                  else if (TYPE_TARGET_TYPE (TYPE_FN_FIELD_TYPE (f, j)) != 0 &&
                        TYPE_CODE(TYPE_TARGET_TYPE (TYPE_FN_FIELD_TYPE (f, j))) != TYPE_CODE_VOID)
                        {
                         fprintf_filtered(stream,"function  ");
                        }
                  else
                        {
                         fprintf_filtered(stream,"procedure ");
                        }
                  /* this does not work, no idea why !! */
                  
                  pas_type_print_method_args (physname,
					       method_name,
					       stream);
                  
                  if (TYPE_TARGET_TYPE (TYPE_FN_FIELD_TYPE (f, j)) != 0 &&
                        TYPE_CODE(TYPE_TARGET_TYPE (TYPE_FN_FIELD_TYPE (f, j))) != TYPE_CODE_VOID)
                        {
  		         fputs_filtered (" : ", stream);
		         type_print (TYPE_TARGET_TYPE (TYPE_FN_FIELD_TYPE (f, j)),
				  "", stream, -1);
                        }
		  if (TYPE_FN_FIELD_VIRTUAL_P (f, j))
		    fprintf_filtered (stream, "; virtual");

		  fprintf_filtered (stream, ";\n");
		}
	    }
	  fprintfi_filtered (level, stream, "end");
	}
      break;

    case TYPE_CODE_ENUM:
      if (TYPE_TAG_NAME (type) != NULL)
	{
	  fputs_filtered (TYPE_TAG_NAME (type), stream);
	  if (show > 0)
	    fputs_filtered (" ", stream);
	}
      /* enum is just defined by
         type enume_name = (enum_member1,enum_member2,...) */
      fprintf_filtered (stream, " = ");
      wrap_here ("    ");
      if (show < 0)
	{
	  /* If we just printed a tag name, no need to print anything else.  */
	  if (TYPE_TAG_NAME (type) == NULL)
	    fprintf_filtered (stream, "(...)");
	}
      else if (show > 0 || TYPE_TAG_NAME (type) == NULL)
	{
	  fprintf_filtered (stream, "(");
	  len = TYPE_NFIELDS (type);
	  lastval = 0;
	  for (i = 0; i < len; i++)
	    {
	      QUIT;
	      if (i) fprintf_filtered (stream, ", ");
	      wrap_here ("    ");
	      fputs_filtered (TYPE_FIELD_NAME (type, i), stream);
	      if (lastval != TYPE_FIELD_BITPOS (type, i))
		{
		  fprintf_filtered (stream, " := %d", TYPE_FIELD_BITPOS (type, i));
		  lastval = TYPE_FIELD_BITPOS (type, i);
		}
	      lastval++;
	    }
	  fprintf_filtered (stream, ")");
	}
      break;

    case TYPE_CODE_VOID:
      fprintf_filtered (stream, "void");
      break;

    case TYPE_CODE_UNDEF:
      fprintf_filtered (stream, "record <unknown>");
      break;

    case TYPE_CODE_ERROR:
      fprintf_filtered (stream, "<unknown type>");
      break;

     /* this probably does not work for enums */
     case TYPE_CODE_RANGE:
      {
       struct type *target = TYPE_TARGET_TYPE (type);
       if (target == NULL)
         target = builtin_type_long;
       print_type_scalar (target, TYPE_LOW_BOUND (type), stream);
       fputs_filtered ("..", stream);
       print_type_scalar (target, TYPE_HIGH_BOUND (type), stream);
      }
      break;

    case TYPE_CODE_SET:
      fputs_filtered ("set of ", stream);
      pas_print_type (TYPE_INDEX_TYPE (type), "", stream,
                         show - 1, level);
      break;

    default:
      /* Handle types not explicitly handled by the other cases,
	 such as fundamental types.  For these, just print whatever
	 the type name is, as recorded in the type itself.  If there
	 is no type name, then complain. */
      if (TYPE_NAME (type) != NULL)
	{
	  fputs_filtered (TYPE_NAME (type), stream);
	}
      else
	{
	  /* At least for dump_symtab, it is important that this not be
	     an error ().  */
	  fprintf_filtered (stream, "<invalid unnamed pascal type code %d>",
			    TYPE_CODE (type));
	}
      break;
    }
}

/*
  $Log$
  Revision 1.2  1999/10/29 11:57:06  pierre
   * avoid NULL dereference

  Revision 1.1  1999/09/07 11:40:19  pierre
   First import of pascal gdb 4.18 changes

  Revision 1.1  1999/01/25 20:30:31  peter
    + first version

  Revision 1.2  1999/01/05 08:39:36  pierre
    + dbgcom.h and dbgcom.c modified by Andris Pavenis
    + added code for MMX state detection and display
      (still uggly :
       FPU stack shown by "info all"
       MMX registers shown by "info float" if in MMX mode)

  Revision 1.2  1998/12/23 15:53:06  Pierre
  exceptions and floatinfo

  Revision 1.1  1998/12/18 18:25:50  pierre
  first DJGPP v4.16 for pascal with exception debugging capabilities

  Revision 1.1  1998/12/04 10:31:49  dosuser
   + first version with pascal syntax

  Revision 1.1  1998/09/18 12:37:39  pierre
*/
