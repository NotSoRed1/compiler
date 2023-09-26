#pragma once

#include "./utils/core.h"
#include "./utils/string.h"
#include "./utils/darray.h"

typedef struct Settings {
    const char* entry_file;
    const char* output_path;
    String*     include_dirs;
    String      other_args;

    b8          print_ast;
    b8          run;
} Settings;

inline void print_usage() {
    printf("Usage: ./compiler entry_file -flag -option=...\n");
    printf("\n");
    printf("Flags: \n");
    printf("    -p | -print-ast     => print the program ast. \n");
    printf("    -r | -run           => run the generated program. \n"); 
    printf("    -h | -help          => print this help text. \n"); 
    printf("\n\n");
    printf("Options: \n");
    printf("    -o | -output        => specifie the output file path. \n");
    printf("    -i | -include-dir   => add an additional include search directory. \n");
    printf("    -L | -lib-dir       => add an additional library search directory. \n");
    printf("    -l | -link          => link a given library into the program");
    printf("\n");
}

inline Settings parse_arguments(i32 argc, const char** argv) {
    Settings settings;
    settings.print_ast = false;
    settings.run = false;
    settings.output_path = "bin/out.c";
    settings.include_dirs = darray_init(String, 2);
    settings.other_args = string_c("");

    if (argc < 2) {
        print_usage();
        exit(1);
    }
    if (string_eq_cstr(string_c("-h"), argv[1]) || string_eq_cstr(string_c("-help"), argv[1])) {
        print_usage();
        exit(1);
    }

    settings.entry_file = argv[1];

    for (u32 i = 2; i < argc; i++) {
        String curr = string_c(argv[i]);

        if (string_eq(curr, string_c("-h")) || string_eq(curr, string_c("-help"))) {
            print_usage();
            exit(1);
        }

        if ((string_eq(curr, string_c("-l")) || string_eq(curr, string_c("-link")))
            || (string_eq(curr, string_c("-L")) || string_eq(curr, string_c("-lib-dir")))) 
        {
            if (i == argc - 1) {
                printf("Error: Expected a value after the %s option. \n\n", argv[i]);
                print_usage();
                exit(1);
            }
            i += 1;

            String arg;
            if (string_eq(curr, string_c("-l")) || string_eq(curr, string_c("-link"))) {
                arg = string_cat(string_c("-l"), string_c(argv[i]));
            } else {
                arg = string_cat(string_c("-L"), string_c(argv[i]));
            }

            String new_args = string_cat(settings.other_args, arg);
            String new_args_with_space = string_cat(new_args, string_c(" "));
            settings.other_args = new_args_with_space;

            free(arg.buffer);
            free(new_args.buffer);
            continue;
        }

        if (string_eq(curr, string_c("-p")) || string_eq(curr, string_c("-print-ast"))) {
            settings.print_ast = true;
            continue;
        }

        if (string_eq(curr, string_c("-r")) || string_eq(curr, string_c("-run"))) {
            settings.run = true;
            continue;
        }

        if (string_eq(curr, string_c("-i")) || string_eq(curr, string_c("-include-dir"))) {
            if (i == argc - 1) {
                printf("Error: Expected an include path after the %s option. \n\n", argv[i]);
                print_usage();
                exit(1);
            }

            i += 1;
            String inc_dir = string_c(argv[i]);
            string_replace(&inc_dir, '\\', '/');
            darray_push(settings.include_dirs, inc_dir);
            continue;
        }


        if (string_eq(curr, string_c("-o")) || string_eq(curr, string_c("-output"))) {
            if (i == argc - 1) {
                printf("Error: Expected an output path after the %s option. \n\n", argv[i]);
                print_usage();
                exit(1);
            }

            i += 1;
            settings.output_path = argv[i];
            continue;
        }

        printf("Error: Unexpected %s argument. \n\n", argv[i]);
        print_usage();
        exit(1);
    }



    // add the compiler std library directory
    String compiler_path = string_c(argv[0]);
    compiler_path = get_file_dir(compiler_path);


    darray_push(settings.include_dirs, compiler_path);
    return settings;
} 