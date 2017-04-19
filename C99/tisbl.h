
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

typedef enum
{
    TL_CONTINUE, TL_LOOP, TL_RETURN
} TLFinal;

typedef struct TLLoc TLLoc;
typedef struct TLValue TLValue;
typedef struct TLContext TLContext;
typedef struct TLVerb TLVerb;
typedef struct TLVM TLVM;
typedef struct TLStack TLStack;

typedef void TLVerbProc(TLVM* vm, TLStack* input, TLStack* output);
typedef void TLOutputFn(TLVM* vm, const char* text);
typedef char* TLInputFn(TLVM* vm);
typedef void TLStepFn(TLVM* vm);
typedef void TLPanicFn(TLVM* vm, const char* message);

#define tl_is_integer(x) ((x).type == TL_INTEGER)
#define tl_is_float(x) ((x).type == TL_FLOAT)
#define tl_is_string(x) ((x).type == TL_STRING)
#define tl_is_number(x) ((x).type == TL_INTEGER || (x).type == TL_FLOAT)

#define tl_top_context(vm) ((vm)->contexts[(vm)->ccount-1])
#define tl_top_loc(vm) (tl_top_context(vm)->token.loc)
#define tl_top_file(vm) tl_file(vm, tl_top_context(vm)->token)
#define tl_top_line(vm) tl_line(tl_top_context(vm)->token)
#define tl_file(vm, v) ((vm)->files[(v).loc.file])
#define tl_line(v) ((v).loc.line)

#pragma pack(push, 1)

struct TLLoc
{
    uint16_t file;
    uint16_t line;
};

struct TLValue
{
    union
    {
        char* s;
        int64_t i;
        double f;
    };
    uint32_t type;
    TLLoc loc;
};

#pragma pack(pop)

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
    TLStack loop;
    TLStack* input;
    TLStack* output;
    TLStack* parent;
    TLStack* cond;
    TLValue token;
    TLFinal final;
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
    TLPanicFn* panic;
    char** files;
    size_t fcount;
};

extern char* tl_clone_string_range(const char* source, size_t length);
extern char* tl_clone_string(const char* source);
extern char* tl_append_strings(const char* a, const char* b);

extern TLVM tl_new_vm(TLInputFn* input, TLOutputFn* output, TLStepFn* step, TLPanicFn* panic);
extern void tl_clear_vm(TLVM* vm);
extern void tl_push_context(TLVM* vm, TLStack* execution, TLStack* input, TLStack* output, TLFinal final);
extern void tl_pop_context(TLVM* vm);
extern void tl_execute(TLVM* vm);
extern void tl_tokenize(TLVM* vm, const char* file, const char* text);
extern void tl_panic(TLVM* vm, const char* format, ...);

extern TLStack tl_new_stack(void);
extern void tl_clear_stack(TLStack* stack);
extern TLStack tl_clone_stack(const TLStack* stack);
extern void tl_reserve(TLStack* stack, size_t capacity);
extern void tl_push_value(TLStack* target, TLValue value);
extern TLValue tl_pop_value(TLVM* vm, TLStack* source);
extern TLValue tl_top_value(TLVM* vm, TLStack* source);
extern bool tl_pop_bool(TLVM* vm, TLStack* source);
extern void tl_multipop(TLVM* vm, TLStack* target, TLStack* source);
extern void tl_push_integer(TLVM* vm, TLStack* target, int64_t i);
extern void tl_push_float(TLVM* vm, TLStack* target, double f);
extern void tl_push_string(TLVM* vm, TLStack* target, char* s);

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

