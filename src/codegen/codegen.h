#pragma once

#include "c_target.h"



typedef enum CodegenTarget {
    codegen_target_c,
    codegen_target_asm,
    codegen_taget_elf,
    codegen_taget_coff,
} CodegenTarget;




inline void generate_module(Module* module, CodegenTarget target, const char* out_path) {
    switch (target) {
    case codegen_target_c: {
        u8* output = generate_c(module);
        u64 length = darray_length(output);
        write_file(out_path, output, length);

        darray_free(output);
    } break;
    case codegen_target_asm:
    case codegen_taget_elf:
    case codegen_taget_coff: {
        printf(RED_COLOR "Internal Error: This target is not supported yet. \n" WHITE_COLOR);
        exit(69);
    } break;
    }
}


