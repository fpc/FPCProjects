#include "dbgcom.h"
#include <i386/tm-i386v.h>

#define FLOAT_INFO {print_387_control_word(fpue.control); \
        print_387_status_word (fpue.status);\
        print_387_mmx(); \
        }

#define DIRNAME_SEPARATOR ';'
#define SLASH_P(X) ((X)=='/' || (X)=='\\')
#define SLASH_CHAR '/'
#define SLASH_STRING "/"
#define ROOTED_P(X) (SLASH_P((X)[0]) || ((X)[0] && (X)[1] == ':'))


