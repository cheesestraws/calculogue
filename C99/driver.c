
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <inttypes.h>

#include "getopt.h"
#include "tisbl.h"

static void version(void)
{
    puts("TISBL-C 0.1");
}

static void usage(void)
{
    puts("Usage: tisbl [OPTION]... [FILE]");
    puts("Options:");
    puts("  -h, --help     show this help");
    puts("  -t, --trace    enable tracing");
    puts("  -v, --version  show version information");
}

static char* read_line(TLVM* vm)
{
    char* line = NULL;
    size_t length = 0, size = 1024;

    for (;;)
    {
        line = realloc(line, size);

        if (!fgets(line + length, size - length, stdin))
        {
            free(line);
            return NULL;
        }

        length = strlen(line);
        if (length && (line[length - 1] == '\n' || line[length - 1] == '\r'))
        {
            line[length - 1] = '\0';
            break;
        }

        size *= 2;
    }

    return line;
}

static void print(TLVM* vm, const char* text)
{
    printf("%s", text);
    if (vm->trace)
    {
        const size_t length = strlen(text);
        if (length > 0 && text[length - 1] != '\n')
            putchar('\n');
    }
}

static void print_value(FILE* stream, const TLValue* value)
{
    switch (value->type)
    {
        case TL_INTEGER:
            fprintf(stream, "%" PRIi64, value->i);
            break;
        case TL_FLOAT:
            fprintf(stream, "%f", value->f);
            break;
        case TL_STRING:
        {
            const char* c = value->s;

            fprintf(stream, "\"");
            while (*c)
            {
                if (*c == '\t')
                    fprintf(stream, "\\t");
                else if (*c == '\n')
                    fprintf(stream, "\\n");
                else if (*c == '\r')
                    fprintf(stream, "\\r");
                else if (*c == '\\')
                    fprintf(stream, "\\\\");
                else if (*c == '\"')
                    fprintf(stream, "\\\"");
                else
                    fputc(*c, stream);

                c++;
            }

            fputc('\"', stream);
            break;
        }
    }
}

static void print_stack(const TLStack* stack)
{
    printf("[");

    for (size_t i = 0;  i < stack->count;  i++)
    {
        printf(" ");
        print_value(stdout, stack->values + i);
    }

    printf(" ]\n");
}

static void step(TLVM* vm)
{
    const TLContext* context = vm->contexts[vm->ccount - 1];

    printf("(Depth %i) %s\n", vm->ccount - 1, context->token.s);
    printf("  Primary     ");
    print_stack(&context->primary);
    printf("  Secondary : ");
    print_stack(&context->secondary);
    printf("  Execution , ");
    print_stack(&context->execution);

    if (context->parent)
    {
        printf("  Input     . ");
        print_stack(context->input);
        printf("  Output    . ");
        print_stack(context->output);
        printf("  Parent    ; ");
        print_stack(context->parent);
    }
}

static void panic(TLVM* vm, const char* message)
{
    fprintf(stderr, "%s:%u: error: %s\n",
            tl_top_file(vm),
            tl_top_line(vm),
            message);
    fprintf(stderr, "Call stack:\n");

    for (size_t i = 0;  i < vm->ccount;  i++)
    {
        fprintf(stderr, "#%zu ", i);
        print_value(stderr, &vm->contexts[i]->token);
        fprintf(stderr, " (%s:%u)\n",
                tl_file(vm, vm->contexts[i]->token),
                tl_line(vm->contexts[i]->token));
    }

    exit(EXIT_FAILURE);
}

int main(int argc, char** argv)
{
    int ch;
    bool trace = false;
    enum { HELP, TRACE, VERSION };
    const struct option options[] =
    {
        { "help",    0, NULL, HELP    },
        { "trace",   0, NULL, TRACE,  },
        { "version", 0, NULL, VERSION },
        { NULL, 0, NULL, 0 }
    };

    while ((ch = getopt_long(argc, argv, "htv", options, NULL)) != -1)
    {
        switch (ch)
        {
            case 'h':
            case HELP:
                usage();
                exit(EXIT_SUCCESS);
            case 't':
            case TRACE:
                trace = true;
                break;
            case 'v':
            case VERSION:
                version();
                exit(EXIT_SUCCESS);
        }
    }

    argc -= optind;
    argv += optind;

    TLVM vm = tl_new_vm(read_line, print, step, panic);
    vm.trace = trace;
    tl_register_stdlib(&vm);
    tl_push_context(&vm, NULL, NULL, NULL, TL_RETURN);

    if (argc)
    {
        FILE* file = fopen(argv[0], "rb");
        if (!file)
        {
            fprintf(stderr, "%s: Failed to open file: %s\n", argv[0], strerror(errno));
            exit(EXIT_FAILURE);
        }

        fseek(file, 0, SEEK_END);
        const long size = ftell(file);
        fseek(file, 0, SEEK_SET);

        char* text = calloc(size + 1, 1);
        fread(text, 1, size, file);
        fclose(file);

        tl_tokenize(&vm, &tl_top_context(&vm)->execution, argv[0], text);
        free(text);
        tl_execute(&vm);
    }
    else
    {
        for (;;)
        {
            printf("> ");

            char* line = read_line(&vm);
            if (!line)
                break;

            tl_tokenize(&vm, &tl_top_context(&vm)->execution, "(stdin)", line);
            free(line);
            tl_execute(&vm);

            putchar('\n');
        }
    }

    tl_clear_vm(&vm);

    exit(EXIT_SUCCESS);
}

