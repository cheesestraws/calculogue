
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
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
            break;

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
}

static void print_stack(const TLStack* stack)
{
    printf("[");

    for (size_t i = 0;  i < stack->count;  i++)
    {
        switch (stack->values[i].type)
        {
            case TL_INTEGER:
                printf(" i:%" PRIi64, stack->values[i].i);
                break;
            case TL_FLOAT:
                printf(" f:%f", stack->values[i].f);
                break;
            case TL_STRING:
            {
                const char* c = stack->values[i].s;

                printf(" s:\"");
                while (*c)
                {
                    if (*c == '\t')
                        printf("\\t");
                    else if (*c == '\n')
                        printf("\\n");
                    else if (*c == '\r')
                        printf("\\r");
                    else
                        putchar(*c);

                    c++;
                }

                printf("\"");
                break;
            }
        }
    }

    printf(" ]\n");
}

static void step(TLVM* vm)
{
    const TLContext* context = vm->contexts[vm->ccount - 1];

    printf("T %s\n", context->token.s);
    printf("P ");
    print_stack(&context->primary);
    printf("S ");
    print_stack(&context->secondary);
    printf("C ");
    print_stack(&context->execution);
}

static void panic(TLVM* vm, const char* format, ...)
{
    va_list vl;

    fprintf(stderr, "Call stack:\n");

    for (size_t i = 0;  i < vm->ccount;  i++)
    {
        TLValue string = tl_cast_value(vm->contexts[i]->token, TL_STRING);
        fprintf(stderr, " %s (%s:%u)\n",
                string.s,
                tl_file(vm, string),
                tl_line(string));
    }

    va_start(vl, format);
    vfprintf(stderr, format, vl);
    va_end(vl);

    putchar('\n');
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
    tl_push_context(&vm, NULL, NULL, NULL);

    if (argc)
    {
        FILE* file = fopen(argv[0], "rb");
        if (!file)
        {
            fprintf(stderr, "Failed to open file\n");
            exit(EXIT_FAILURE);
        }

        fseek(file, 0, SEEK_END);
        const long size = ftell(file);
        fseek(file, 0, SEEK_SET);

        char* text = calloc(size + 1, 1);
        fread(text, 1, size, file);
        fclose(file);

        tl_tokenize(&vm, argv[0], text);
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

            tl_tokenize(&vm, "(stdin)", line);
            free(line);
            tl_execute(&vm);

            putchar('\n');
        }
    }

    tl_clear_vm(&vm);

    exit(EXIT_SUCCESS);
}
