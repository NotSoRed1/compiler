#include "parser.h"
#include "ast.h"

#include "utils/allocator.h"
#include "codegen/codegen.h"
#include "checker.h"
#include "driver.h"
#include <stdlib.h>



i32 main(i32 argc, const char** argv) {
    if (!global_allocator_init()) {
        printf("Failed to initialize the global allocator. \n");
        return 1;
    }

    // printf("path: %s \n", argv[0]);
    // exit(69);

    Settings settings = parse_arguments(argc, argv);

    u8* buffer = null;
    u64 size = 0;
    if (!open_file(settings.entry_file, &buffer, &size)) {
        printf("Failed to open the '%s' file. \n", settings.entry_file);
        return 1;
    }

    Module* module = parse_module(string(buffer, size));
    check_module(module, &settings);

    if (settings.print_ast) {
        print_module(module);
        global_allocator_init();
        return 0;
    }

    String output_dir = string_cat(get_file_dir(string_c(settings.output_path)), string_c("out.c"));
    generate_module(module, codegen_target_c, (char*)output_dir.buffer);


    String run_command = string_c(settings.output_path);
    String compile_command = string_fmt("clang.exe \
        -std=c11 \
        -Wno-unused-value \
        -Wno-microsoft-enum-forward-reference \
        -Wno-constant-conversion \
        -Wno-parentheses-equality \
        -Wno-incompatible-library-redeclaration \
        -Wno-duplicate-decl-specifier \
        -Wno-incompatible-pointer-types-discards-qualifiers \
        -Wno-implicitly-unsigned-literal \
        -Wno-tautological-constant-out-of-range-compare \
        %.*s \
        %.*s \
        -o %s", 
        (u32)output_dir.length, output_dir.buffer,
        (u32)settings.other_args.length, settings.other_args.buffer,
        settings.output_path);

    // run the compile command
    system((char*)compile_command.buffer);


    if (settings.run) {
        #ifdef _WIN32
            string_replace(&run_command, '/', '\\');
            system((char*)run_command.buffer);
        #else
            system((char*)run_command.buffer);
        #endif
    }


    global_allocator_free();
    free(compile_command.buffer);
    return 0;
}