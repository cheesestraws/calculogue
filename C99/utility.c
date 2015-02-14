
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

#include "tisbl.h"

extern char* tl_clone_string_range(const char* source, size_t length)
{
    char* result = calloc(length + 1, 1);
    memcpy(result, source, length);
    return result;
}

extern char* tl_clone_string(const char* source)
{
    return tl_clone_string_range(source, strlen(source));
}

extern char* tl_append_strings(const char* a, const char* b)
{
    char* result = calloc(strlen(a) + strlen(b) + 1, 1);
    strcat(result, a);
    strcat(result, b);
    return result;
}

extern void tl_panic(const char* format, ...)
{
    va_list vl;
    va_start(vl, format);
    vfprintf(stderr, format, vl);
    va_end(vl);
    putchar('\n');
    exit(EXIT_FAILURE);
}

