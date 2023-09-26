#include "error.h"

#define print_spaces(count) for (u32 i = 0; i < count; i++) { printf(" "); }

String get_error_line(Loc loc) {
    u32 start = loc.offset - loc.col;
    u32 end = loc.offset + 1;

    while (end < string_length(loc.source)) {
        if (string_at(loc.source, end) == '\n' || string_at(loc.source, end) == '\r') break;
        end += 1;
    }

    return (String) {.buffer = &loc.source.buffer[start], .length = end - start};
}


u8 get_digits_count(u64 number) {
    if (number == 0) {
        return 1;
    }

    number = number < 0 ? -number : number;
    u8 result = floor(log10(number)) + 1;

    return result;
}


void error(Loc loc, const char* fmt, ...) {
    va_list args;
    printf(RED_COLOR "[ERROR][%u:%u] => ", loc.line + 1, loc.col + 1);
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);

    String error_line = get_error_line(loc);
    u8     digits_count = get_digits_count(loc.line + 1);
    print_spaces(digits_count); printf(" |\n");
    print_spaces(digits_count); printf(" |\n");
    printf("%u | " YELLOW_COLOR "%.*s \n" , loc.line + 1, (u32)error_line.length, error_line.buffer);
    print_spaces(digits_count); printf(RED_COLOR " | " MAGENTA_COLOR); 
    print_spaces(loc.col); printf("^ error here");
    printf(WHITE_COLOR "\n");

    exit(69);
} 


#undef print_spaces