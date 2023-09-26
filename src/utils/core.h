
#pragma once

#include "types.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "allocator.h"
#include "darray.h"



// TERMINAL COLORS          ==========
#define BLACK_COLOR               "\x1B[30m"
#define RED_COLOR                 "\x1B[31m"
#define GREEN_COLOR               "\x1B[32m"
#define YELLOW_COLOR              "\x1B[33m"
#define BLUE_COLOR                "\x1B[34m"
#define MAGENTA_COLOR             "\x1B[35m"
#define CYAN_COLOR                "\x1B[36m"
#define WHITE_COLOR               "\x1B[37m"


#define core_max(a, b) (a > b ? a : b)
#define core_min(a, b) (a < b ? a : b)

#define is_alpha(c) ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') 
#define is_digit(c) (c >= '0' && c <= '9')
#define align_forward(num) ()
#define to_lower_case(c) (c >= 'A' && c <= 'Z' ? c + 32 : c)

#define align_up(num, align) (((num) + (align) - 1) & ~((align) - 1))

#define is_unary_op(op) ((op) == '-' || (op) == '!'|| (op) == '~' || (op) == '&' || (op) == '*')
#define is_mul_op(op) ((op) == token_mul || (op) == token_div || (op) == token_mod)
#define is_add_op(op) ((op) == token_plus || (op) == token_minus)
#define is_cmp_op(op) (((op) >= token_cmp_start && (op) <= token_cmp_end) || (op) == token_lt || (op) == token_gt)
#define is_logic_op(op) (is_cmp_op((op)) || (op) == token_logic_and || (op) == token_logic_or)
#define is_asign_op(op) (((op) >= token_asign_start && (op) <= token_asign_end) || (op) == token_eq)


inline String get_file_dir(String file_path) {
    String path = string_cat(file_path, string_c(""));
    string_replace(&path, '\\', '/');

    u64 size = file_path.length;
    while (size--) {
        if (path.buffer[size] == '/' ) break;
    }

    path.length = size + 1;
    if (size == -1) {
        path.buffer[0] = '.';
        path.length += 1;
    }

    if (path.buffer[path.length-1] != '/') {
        String new_path = string_cat(path, string_c("/"));
        free(path.buffer);
        path = new_path;
    }

    return path;
}

inline b8 file_exists(const char* path) {
    FILE* file = null;
    file = fopen(path, "r");
    if (!file) {
        return false;
    }

    fclose(file);
    return true;
}


inline b8 open_file(const char* path, u8** buffer, u64* size) {
    FILE* file = fopen(path, "rb");
    if (!file) return false;

    fseek(file, 0, SEEK_END);
    *size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (!(*size)) {
        fclose(file);
        return false;
    }

    *buffer = (u8*)global_allocator_alloc(*size);
    if(!(*buffer)) {
        fclose(file);
        return false;
    }

    if (!fread(*buffer, 1, *size, file)) {
        fclose(file);
        *buffer = null;
        return false;
    }

    fclose(file);
    return true;
}

inline b8 write_file(const char* path, u8* buffer, u64 size) {
    FILE* file = fopen(path, "wb");
    if (!file) return false;

    u32 res = fwrite(buffer, 1, size, file);
    fclose(file);

    return res != 0;
}



inline void* copy_darray_buffer(void* darray) {
    if (!darray) return null;

    DarrayHeader* header = (DarrayHeader*)DARRAY_HEADER(darray);

    u64 size = (header->length * header->type_size) + sizeof(DarrayHeader);
    u8* new_buffer = (u8*)global_allocator_alloc(size);

    memcpy(new_buffer, header, size);

    return (void*)(new_buffer + sizeof(DarrayHeader));
}