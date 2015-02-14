
#ifndef _tisbl_h_
#define _tisbl_h_

#include <stdbool.h>
#include <stdint.h>

#if defined(__cplusplus)
extern "C" {
#endif

typedef enum
{
    TL_STRING, TL_INTEGER, TL_FLOAT
} TLType;

typedef struct TLValue TLValue;
typedef struct TLContext TLContext;
typedef struct TLVerb TLVerb;
typedef struct TLVM TLVM;
typedef struct TLStack TLStack;

typedef void TLVerbProc(TLVM* vm, TLStack* input, TLStack* output);
typedef void TLOutputFn(const char* text);
typedef char* TLInputFn(void);
typedef void TLStepFn(TLVM* vm, const char* token);

#define tl_is_integer(x) ((x).type == TL_INTEGER)
#define tl_is_float(x) ((x).type == TL_FLOAT)
#define tl_is_string(x) ((x).type == TL_STRING)
#define tl_is_number(x) ((x).type == TL_INTEGER || (x).type == TL_FLOAT)

struct TLValue
{
    union
    {
        char* s;
        int64_t i;
        double f;
    };
    int32_t type;
};

struct TLStack
{
    TLValue* values;
    size_t count;
    size_t capacity;
};

struct TLContext
{
    TLStack primary;
    TLStack secondary;
    TLStack execution;
    TLStack* input;
    TLStack* output;
    TLStack* parent;
};

struct TLVerb
{
    TLVerbProc* proc;
    char* name;
    TLStack code;
};

struct TLVM
{
    TLContext** contexts;
    size_t ccount;
    TLVerb* verbs;
    size_t vcount;
    bool trace;
    TLInputFn* input;
    TLOutputFn* output;
    TLStepFn* step;
};

extern char* tl_clone_string_range(const char* source, size_t length);
extern char* tl_clone_string(const char* source);
extern char* tl_append_strings(const char* a, const char* b);

extern void tl_panic(const char* message, ...);

extern TLVM tl_new_vm(TLInputFn* input, TLOutputFn* output, TLStepFn* step);
extern void tl_clear_vm(TLVM* vm);
extern void tl_push_context(TLVM* vm, TLStack* execution, TLStack* input, TLStack* output);
extern void tl_pop_context(TLVM* vm);
extern void tl_execute(TLVM* vm);
extern void tl_tokenize(TLStack* target, const char* text);

extern TLStack tl_new_stack(void);
extern void tl_clear_stack(TLStack* stack);
extern TLStack tl_clone_stack(const TLStack* stack);
extern void tl_reserve(TLStack* stack, size_t capacity);
extern void tl_push_value(TLStack* target, TLValue value);
extern TLValue tl_pop_value(TLStack* source);
extern TLValue tl_peek_value(TLStack* source);
extern void tl_multipop(TLStack* target, TLStack* source);
extern void tl_push_integer(TLStack* target, int64_t i);
extern void tl_push_float(TLStack* target, double f);
extern void tl_push_string(TLStack* target, char* s);

extern bool tl_bool(TLValue v);
extern double tl_float(TLValue value);
extern int64_t tl_integer(TLValue value);
extern TLValue tl_cast_value(TLValue value, TLType type);
extern TLValue tl_clone_value(TLValue value);
extern void tl_clear_value(TLValue* value);

extern void tl_create_native_verb(TLVM* vm, const char* name, TLVerbProc* proc);
extern void tl_create_tisbl_verb(TLVM* vm, const char* name, TLStack code);

extern void tl_register_stdlib(TLVM* vm);

#if defined(__cplusplus)
}
#endif

#endif

