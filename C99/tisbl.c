
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <inttypes.h>

#include "tisbl.h"

static bool is_newline(char c)
{
    return c == '\r' || c == '\n';
}

static bool is_whitespace(char c)
{
    return c == ' ' || c == '\t' || is_newline(c);
}

static bool is_stack_name(char c)
{
    return c == ':' || c == '.' || c == ',' || c == ';';
}

static bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

static bool is_integer(const char* s)
{
    while (is_digit(*s))
        s++;

    return *s == '\0';
}

static bool is_float(const char* s)
{
    while (is_digit(*s))
        s++;
    if (*s++ != '.')
        return 0;
    while (is_digit(*s))
        s++;

    return *s == '\0';
}

static bool is_verb_name(const char* s)
{
    while (*s && !is_whitespace(*s) && !is_stack_name(*s))
        s++;

    return *s == '\0';
}

static TLVerb* find_verb(TLVM* vm, const char* name)
{
    for (int i = 0;  i < vm->vcount;  i++)
    {
        if (strcmp(vm->verbs[i].name, name) == 0)
            return vm->verbs + i;
    }

    return NULL;
}

static TLVerb* find_or_create_verb(TLVM* vm, const char* name)
{
    TLVerb* verb = find_verb(vm, name);
    if (verb)
    {
        tl_clear_stack(&verb->code);
        verb->proc = NULL;
    }
    else
    {
        vm->vcount++;
        vm->verbs = realloc(vm->verbs, vm->vcount * sizeof(TLVerb));
        verb = vm->verbs + vm->vcount - 1;
        memset(verb, 0, sizeof(TLVerb));
        verb->name = tl_clone_string(name);
    }

    return verb;
}

static TLStack* source_stack(TLContext* context, char c)
{
    switch (c)
    {
        case ':': return &context->secondary;
        case ',': return &context->execution;
        case ';': return context->parent;
        case '.': return context->input;
        default:  return NULL;
    }
}

static TLStack* target_stack(TLContext* context, char c)
{
    switch (c)
    {
        case ':': return &context->secondary;
        case ',': return &context->execution;
        case ';': return context->parent;
        case '.': return context->output;
        default:  return NULL;
    }
}

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

extern TLVM tl_new_vm(TLInputFn* input, TLOutputFn* output, TLStepFn* step, TLPanicFn* panic)
{
    return (TLVM) { NULL, 0, NULL, 0, false, input, output, step, panic };
}

extern void tl_clear_vm(TLVM* vm)
{
    while (vm->ccount)
        tl_pop_context(vm);

    for (size_t i = 0;  i < vm->vcount;  i++)
    {
        tl_clear_stack(&vm->verbs[i].code);
        free(vm->verbs[i].name);
    }

    for (size_t i = 0;  i < vm->fcount;  i++)
        free(vm->files[i]);

    free(vm->contexts);
    free(vm->verbs);
    free(vm->files);
    memset(vm, 0, sizeof(TLVM));
}

extern void tl_push_context(TLVM* vm, TLStack* execution, TLStack* input, TLStack* output)
{
    TLContext* context = calloc(1, sizeof(TLContext));
    context->input = input;
    context->output = output;

    if (execution)
    {
        context->execution = *execution;
        *execution = tl_new_stack();
    }

    if (vm->ccount)
        context->parent = &tl_top_context(vm)->execution;

    vm->ccount++;
    vm->contexts = realloc(vm->contexts, sizeof(TLContext*) * vm->ccount);
    vm->contexts[vm->ccount - 1] = context;
}

extern void tl_pop_context(TLVM* vm)
{
    TLContext* context = tl_top_context(vm);
    tl_clear_stack(&context->primary);
    tl_clear_stack(&context->secondary);
    tl_clear_stack(&context->execution);
    tl_clear_value(&context->token);
    free(context);
    vm->ccount--;
}

extern TLStack tl_new_stack(void)
{
    return (TLStack) { NULL, 0, 0 };
}

extern void tl_clear_stack(TLStack* stack)
{
    for (size_t i = 0;  i < stack->count;  i++)
        tl_clear_value(stack->values + i);
    free(stack->values);
    memset(stack, 0, sizeof(TLStack));
}

extern TLStack tl_clone_stack(const TLStack* stack)
{
    TLStack clone = tl_new_stack();
    tl_reserve(&clone, stack->count);
    for (size_t i = 0;  i < stack->count;  i++)
        clone.values[i] = tl_clone_value(stack->values[i]);

    clone.count = stack->count;
    return clone;
}

extern void tl_reserve(TLStack* stack, size_t capacity)
{
    if (stack->capacity >= capacity)
        return;

    stack->capacity = capacity + 512 - capacity % 512;
    stack->values = realloc(stack->values, stack->capacity * sizeof(TLValue));
}

extern void tl_push_value(TLStack* target, TLValue v)
{
    tl_reserve(target, target->count + 1);
    target->values[target->count++] = v;
}

extern void tl_push_integer(TLVM* vm, TLStack* target, int64_t i)
{
    tl_push_value(target, (TLValue) { { .i = i }, TL_INTEGER, tl_top_loc(vm) });
}

extern void tl_push_float(TLVM* vm, TLStack* target, double f)
{
    tl_push_value(target, (TLValue) { { .f = f }, TL_FLOAT, tl_top_loc(vm) });
}

extern void tl_push_string(TLVM* vm, TLStack* target, char* s)
{
    tl_push_value(target, (TLValue) { { .s = s }, TL_STRING, tl_top_loc(vm) });
}

extern TLValue tl_pop_value(TLVM* vm, TLStack* source)
{
    if (source->count == 0)
        tl_panic(vm, "Stack underflow");

    return source->values[--source->count];
}

extern TLValue tl_top_value(TLVM* vm, TLStack* source)
{
    if (source->count == 0)
        tl_panic(vm, "Stack underflow");

    return source->values[source->count - 1];
}

extern void tl_multipop(TLVM* vm, TLStack* target, TLStack* source)
{
    TLValue count = tl_pop_value(vm, source);
    if (!tl_is_integer(count))
    {
        TLValue string = tl_cast_value(count, TL_STRING);
        tl_panic(vm, "Multipop count not an integer: %s (%s:%u)",
                 string.s, tl_file(vm, string), tl_line(string));
    }

    tl_reserve(target, target->count + count.i);
    while (count.i--)
        tl_push_value(target, tl_pop_value(vm, source));
}

extern void tl_create_native_verb(TLVM* vm, const char* name, TLVerbProc* proc)
{
    find_or_create_verb(vm, name)->proc = proc;
}

extern void tl_create_tisbl_verb(TLVM* vm, const char* name, TLStack code)
{
    find_or_create_verb(vm, name)->code = code;
}

extern bool tl_bool(TLValue v)
{
    if (tl_is_integer(v) && v.i == 0)
        return false;
    else if (tl_is_float(v) && v.f == 0.0)
        return false;
    else
        return true;
}

extern double tl_float(TLValue v)
{
    if (tl_is_integer(v))
        return (double) v.i;
    else if (tl_is_float(v))
        return v.f;
    else
        return strtod(v.s, NULL);
}

extern int64_t tl_integer(TLValue v)
{
    if (tl_is_integer(v))
        return v.i;
    else if (tl_is_float(v))
        return (int64_t) v.f;
    else
        return strtoll(v.s, NULL, 10);
}

extern TLValue tl_cast_value(TLValue v, TLType type)
{
    if (type == TL_INTEGER)
    {
        if (tl_is_float(v))
            v.i = (int64_t) v.f;
        else if (tl_is_string(v))
        {
            const int64_t i = strtoll(v.s, NULL, 10);
            free(v.s);
            v.i = i;
        }
    }
    else if (type == TL_FLOAT)
    {
        if (tl_is_integer(v))
            v.f = (double) v.i;
        else if (tl_is_string(v))
        {
            const double f = strtod(v.s, NULL);
            free(v.s);
            v.f = f;
        }
    }
    else
    {
        char buffer[128];

        if (tl_is_integer(v))
        {
            snprintf(buffer, sizeof(buffer), "%" PRIi64, v.i);
            v.s = tl_clone_string(buffer);
        }
        else if (tl_is_float(v))
        {
            snprintf(buffer, sizeof(buffer), "%f", v.f);
            v.s = tl_clone_string(buffer);
        }
    }

    v.type = type;
    return v;
}

extern TLValue tl_clone_value(TLValue v)
{
    if (tl_is_string(v))
        v.s = tl_clone_string(v.s);

    return v;
}

extern void tl_clear_value(TLValue* v)
{
    if (tl_is_string(*v))
        free(v->s);

    memset(v, 0, sizeof(TLValue));
}

extern void tl_tokenize(TLVM* vm, const char* file, const char* text)
{
    size_t i;
    TLLoc loc = { 0, 1 };
    TLStack tokens = tl_new_stack();

    for (i = 0;  i < vm->fcount;  i++)
    {
        if (strcmp(vm->files[i], file) == 0)
            break;
    }

    if (i == vm->fcount)
    {
        vm->fcount++;
        vm->files = realloc(vm->files, vm->fcount * sizeof(char*));
        vm->files[vm->fcount - 1] = tl_clone_string(file);
    }

    loc.file = i;

    while (*text)
    {
        const char* end = text;

        if (is_whitespace(*end))
        {
            while (is_whitespace(*end))
            {
                if (is_newline(*end))
                {
                    loc.line++;
                    if (end[0] == '\r' && end[1] == '\n')
                        end++;
                }

                end++;
            }
        }
        else if (*end == '%')
        {
            while (*end && !is_newline(*end))
                end++;
        }
        else
        {
            while (*end && !is_whitespace(*end))
                end++;

            char* s = tl_clone_string_range(text, end - text);
            tl_push_value(&tokens, (TLValue) { { .s = s }, TL_STRING, loc });
        }

        text = end;
    }

    TLStack* target = &vm->contexts[0]->execution;

    tl_reserve(target, target->count + tokens.count);
    while (tokens.count)
        tl_push_value(target, tl_pop_value(vm, &tokens));

    tl_clear_stack(&tokens);
}

extern void tl_execute(TLVM* vm)
{
    TLContext* context = tl_top_context(vm);

    while (context->execution.count > 0)
    {
        TLStack* target = &context->primary;
        TLStack* source = &context->primary;
        TLValue token = tl_pop_value(vm, &context->execution);

        tl_clear_value(&context->token);
        context->token = tl_clone_value(token);

        if (tl_is_number(token))
            tl_panic(vm, "Cannot execute number");

        if (vm->trace)
            vm->step(vm);

        char* start = token.s;

        if (*start == '\\')
        {
            start++;

            if (is_stack_name(*start))
            {
                source = source_stack(context, *start);
                if (!source)
                    tl_panic(vm, "Invalid input stack: %c", *start);

                start++;
            }

            char* end = start + strlen(start);

            if (end > start && is_stack_name(end[-1]))
            {
                target = target_stack(context, end[-1]);
                if (!target)
                    tl_panic(vm, "Invalid output stack: %c", end[-1]);

                end--;
            }

            *end = '\0';

            if (!is_verb_name(start))
                tl_panic(vm, "Illegal verb name: %s", start);

            TLVerb* verb = find_verb(vm, start);
            if (!verb)
                tl_panic(vm, "Undefined verb: %s", start);

            if (verb->proc)
                verb->proc(vm, source, target);
            else
            {
                TLStack clone = tl_clone_stack(&verb->code);
                tl_push_context(vm, &clone, source, target);
                tl_execute(vm);
                tl_pop_context(vm);
            }
        }
        else
        {
            if (is_stack_name(*start))
            {
                target = target_stack(context, *start);
                start++;
            }

            if (*start == '#')
            {
                start++;

                if (is_integer(start))
                    tl_push_integer(vm, target, strtol(start, NULL, 10));
                else if (is_float(start))
                    tl_push_float(vm, target, strtod(start, NULL));
                else
                    tl_panic(vm, "Invalid number: %s", start);
            }
            else if (*start == '\'')
            {
                start++;
                tl_push_string(vm, target, tl_clone_string(start));
            }
            else
                tl_panic(vm, "Invalid token: %s", start);
        }

        tl_clear_value(&token);
    }
}

void tl_panic(TLVM* vm, const char* format, ...)
{
    va_list vl;
    char buffer[8192];

    va_start(vl, format);
    const int result = vsnprintf(buffer, sizeof(buffer), format, vl);
    va_end(vl);

    if (result < 0)
        vm->panic(vm, "Internal error");
    else
        vm->panic(vm, buffer);
}

