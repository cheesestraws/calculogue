
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tisbl.h"

static int64_t min(int64_t a, int64_t b)
{
    return (a < b) ? a : b;
}

static int64_t max(int64_t a, int64_t b)
{
    return (a > b) ? a : b;
}

static char* sub_integer_from_string(const char* string, int64_t value)
{
    return tl_clone_string_range(string, max(min(strlen(string), value), 0));
}

static char* sub_string_from_string(const char* a, const char* b)
{
    char* result = calloc(strlen(a) + 1, 1);
    char* target = result;

    for (const char* source = a;  *source;  source++)
    {
        if (strchr(b, *source) == NULL)
            *target++ = *source;
    }

    return result;
}

static char* mul_string_by_float(const char* string, double value)
{
    const size_t ls = strlen(string);
    const size_t lr = lround(ls * value);
    char* result = calloc(lr + 1, 1);

    for (size_t i = 0;  i < lr;  i++)
        result[i] = string[i % ls];

    return result;
}

static char* mul_string_by_string(const char* haystack, const char* needle)
{
    const size_t ln = strlen(needle);
    size_t lr = 0;

    for (const char* c = haystack;  *c;  c++)
    {
        if (*c == needle[0])
            lr += ln - 1;
        else
            lr++;
    }

    char* result = calloc(lr + 1, 1);
    char* target = result;

    for (const char* source = haystack;  *source;  source++)
    {
        if (*source == needle[0])
        {
            memcpy(target, needle, ln);
            target += ln;
        }
        else
            *target++ = *source;
    }

    return result;
}

static char* div_string_by_float(const char* string, double value)
{
    return tl_clone_string_range(string, lround(strlen(string) / value));
}

static char* div_string_by_string(const char* a, const char* b)
{
    const size_t lb = strlen(b);
    size_t la = strlen(a);
    char* result = tl_clone_string(a);
    char* where;

    while ((where = strstr(result, b)))
    {
        memmove(where, where + lb, la - lb - (where - result) + 1);
        la -= lb;
    }

    return result;
}

static void stdlib_traceon(TLVM* vm, TLStack* input, TLStack* output)
{
    vm->trace = true;
}

static void stdlib_traceoff(TLVM* vm, TLStack* input, TLStack* output)
{
    vm->trace = false;
}

static void stdlib_exec(TLVM* vm, TLStack* input, TLStack* output)
{
    TLStack code = tl_new_stack();
    tl_multipop(vm, &code, input);
    tl_push_context(vm, &code, input, output);
    tl_execute(vm);
    tl_pop_context(vm);
}

static void stdlib_verb(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue name = tl_pop_value(vm, input);
    if (tl_is_number(name))
    {
        TLValue string = tl_cast_value(name, TL_STRING);
        vm->panic(vm, "Verb name not a string: %s (%s:%u)",
                  string.s, tl_file(vm, string), tl_line(string));
    }

    TLStack code = tl_new_stack();
    tl_multipop(vm, &code, input);
    tl_create_tisbl_verb(vm, name.s, code);
    tl_clear_value(&name);
}

static void stdlib_if(TLVM* vm, TLStack* input, TLStack* output)
{
    TLStack code = tl_new_stack();
    tl_multipop(vm, &code, input);

    TLValue cond = tl_pop_value(vm, input);
    if (tl_bool(cond))
    {
        tl_push_context(vm, &code, input, output);
        tl_execute(vm);
        tl_pop_context(vm);
    }

    tl_clear_value(&cond);
}

static void stdlib_while(TLVM* vm, TLStack* input, TLStack* output)
{
    TLStack code = tl_new_stack();
    tl_multipop(vm, &code, input);

    for (;;)
    {
        TLValue cond = tl_pop_value(vm, input);
        if (!tl_bool(cond))
        {
            tl_clear_value(&cond);
            break;
        }

        TLStack clone = tl_clone_stack(&code);
        tl_push_context(vm, &clone, input, output);
        tl_execute(vm);
        tl_pop_context(vm);
        tl_clear_value(&cond);
    }

    tl_clear_stack(&code);
}

static void stdlib_eq(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue a = tl_pop_value(vm, input);
    TLValue b = tl_pop_value(vm, input);

    if (tl_is_string(a) || tl_is_string(b))
    {
        a = tl_cast_value(a, TL_STRING);
        b = tl_cast_value(b, TL_STRING);
        tl_push_integer(vm, output, strcmp(a.s, b.s) == 0);
    }
    else if (tl_is_float(a) || tl_is_float(b))
        tl_push_integer(vm, output, tl_float(a) == tl_float(b));
    else
        tl_push_integer(vm, output, tl_integer(a) == tl_integer(b));

    tl_clear_value(&a);
    tl_clear_value(&b);
}

static void stdlib_not(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue v = tl_pop_value(vm, input);
    tl_push_integer(vm, output, tl_bool(v) == false);
    tl_clear_value(&v);
}

static void stdlib_swap(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue a = tl_pop_value(vm, input);
    TLValue b = tl_pop_value(vm, input);
    tl_push_value(output, a);
    tl_push_value(output, b);
}

static void stdlib_dup(TLVM* vm, TLStack* input, TLStack* output)
{
    tl_push_value(output, tl_clone_value(tl_peek_value(vm, input)));
}

static void stdlib_rm(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue v = tl_pop_value(vm, input);
    tl_clear_value(&v);
}

static void stdlib_mv(TLVM* vm, TLStack* input, TLStack* output)
{
    tl_push_value(output, tl_pop_value(vm, input));
}

static void stdlib_multipop(TLVM* vm, TLStack* input, TLStack* output)
{
    tl_multipop(vm, output, input);
}

static void stdlib_add(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue s = tl_pop_value(vm, input);
    TLValue f = tl_pop_value(vm, input);

    if (tl_is_integer(f) && tl_is_integer(s))
        tl_push_integer(vm, output, f.i + s.i);
    else if (tl_is_string(f) || tl_is_string(s))
    {
        f = tl_cast_value(f, TL_STRING);
        s = tl_cast_value(s, TL_STRING);
        tl_push_string(vm, output, tl_append_strings(f.s, s.s));
    }
    else
        tl_push_float(vm, output, tl_float(f) + tl_float(s));

    tl_clear_value(&f);
    tl_clear_value(&s);
}

static void stdlib_sub(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue s = tl_pop_value(vm, input);
    TLValue f = tl_pop_value(vm, input);

    if (tl_is_integer(f) && tl_is_integer(s))
        tl_push_integer(vm, output, f.i - s.i);
    else if (tl_is_number(f) && tl_is_string(s))
        tl_push_string(vm, output, sub_integer_from_string(s.s, tl_integer(f)));
    else if (tl_is_string(f) && tl_is_number(s))
        tl_push_string(vm, output, sub_integer_from_string(f.s, tl_integer(s)));
    else if (tl_is_string(f) && tl_is_string(s))
        tl_push_string(vm, output, sub_string_from_string(f.s, s.s));
    else
        tl_push_float(vm, output, tl_float(f) - tl_float(s));

    tl_clear_value(&f);
    tl_clear_value(&s);
}

static void stdlib_mul(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue s = tl_pop_value(vm, input);
    TLValue f = tl_pop_value(vm, input);

    if (tl_is_integer(f) && tl_is_integer(s))
        tl_push_integer(vm, output, f.i * s.i);
    else if (tl_is_number(f) && tl_is_string(s))
        tl_push_string(vm, output, mul_string_by_float(s.s, tl_float(f)));
    else if (tl_is_string(f) && tl_is_number(s))
        tl_push_string(vm, output, mul_string_by_float(f.s, tl_float(s)));
    else if (tl_is_string(f) && tl_is_string(s))
        tl_push_string(vm, output, mul_string_by_string(f.s, s.s));
    else
        tl_push_float(vm, output, tl_float(f) * tl_float(s));

    tl_clear_value(&f);
    tl_clear_value(&s);
}

static void stdlib_div(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue s = tl_pop_value(vm, input);
    TLValue f = tl_pop_value(vm, input);

    if (tl_is_integer(f) && tl_is_integer(s))
        tl_push_integer(vm, output, f.i / s.i);
    else if (tl_is_number(f) && tl_is_string(s))
        tl_push_string(vm, output, div_string_by_float(s.s, tl_float(f)));
    else if (tl_is_string(f) && tl_is_number(s))
        tl_push_string(vm, output, div_string_by_float(f.s, tl_float(s)));
    else if (tl_is_string(f) && tl_is_string(s))
        tl_push_string(vm, output, div_string_by_string(f.s, s.s));
    else
        tl_push_float(vm, output, tl_float(f) / tl_float(s));

    tl_clear_value(&f);
    tl_clear_value(&s);
}

static void stdlib_n(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue v = tl_cast_value(tl_pop_value(vm, input), TL_STRING);
    tl_push_string(vm, output, tl_append_strings(v.s, "\n"));
    tl_clear_value(&v);
}

static void stdlib_space(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue v = tl_cast_value(tl_pop_value(vm, input), TL_STRING);
    tl_push_string(vm, output, tl_append_strings(v.s, " "));
    tl_clear_value(&v);
}

static void stdlib_word(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue v = tl_pop_value(vm, input);
    tl_push_integer(vm, output, tl_is_string(v));
    tl_clear_value(&v);
}

static void stdlib_number(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue v = tl_pop_value(vm, input);
    tl_push_integer(vm, output, tl_is_number(v));
    tl_clear_value(&v);
}

static void stdlib_integer(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue v = tl_pop_value(vm, input);
    tl_push_integer(vm, output, tl_is_integer(v));
    tl_clear_value(&v);
}

static void stdlib_float(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue v = tl_pop_value(vm, input);
    tl_push_integer(vm, output, tl_is_float(v));
    tl_clear_value(&v);
}

static void stdlib_die(TLVM* vm, TLStack* input, TLStack* output)
{
    exit(EXIT_SUCCESS);
}

static void stdlib_out(TLVM* vm, TLStack* input, TLStack* output)
{
    TLValue v = tl_cast_value(tl_pop_value(vm, input), TL_STRING);
    vm->output(vm, v.s);
    tl_clear_value(&v);
}

static void stdlib_in(TLVM* vm, TLStack* input, TLStack* output)
{
    tl_push_string(vm, output, vm->input(vm));
}

extern void tl_register_stdlib(TLVM* vm)
{
    tl_create_native_verb(vm, "trace=1",  stdlib_traceon);
    tl_create_native_verb(vm, "trace=0",  stdlib_traceoff);
    tl_create_native_verb(vm, "exec",     stdlib_exec);
    tl_create_native_verb(vm, "verb",     stdlib_verb);
    tl_create_native_verb(vm, "if",       stdlib_if);
    tl_create_native_verb(vm, "while",    stdlib_while);
    tl_create_native_verb(vm, "eq?",      stdlib_eq);
    tl_create_native_verb(vm, "not",      stdlib_not);
    tl_create_native_verb(vm, "swap",     stdlib_swap);
    tl_create_native_verb(vm, "dup",      stdlib_dup);
    tl_create_native_verb(vm, "rm",       stdlib_rm);
    tl_create_native_verb(vm, "mv",       stdlib_mv);
    tl_create_native_verb(vm, "multipop", stdlib_multipop);
    tl_create_native_verb(vm, "+",        stdlib_add);
    tl_create_native_verb(vm, "-",        stdlib_sub);
    tl_create_native_verb(vm, "*",        stdlib_mul);
    tl_create_native_verb(vm, "div",      stdlib_div);
    tl_create_native_verb(vm, "n",        stdlib_n);
    tl_create_native_verb(vm, "exec",     stdlib_exec);
    tl_create_native_verb(vm, "_",        stdlib_space);
    tl_create_native_verb(vm, "word?",    stdlib_word);
    tl_create_native_verb(vm, "number?",  stdlib_number);
    tl_create_native_verb(vm, "integer?", stdlib_integer);
    tl_create_native_verb(vm, "float?",   stdlib_float);
    tl_create_native_verb(vm, "die",      stdlib_die);
    tl_create_native_verb(vm, "in",       stdlib_in);
    tl_create_native_verb(vm, "out",      stdlib_out);
}

