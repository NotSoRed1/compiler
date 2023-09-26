#pragma once 


#include "core.h"
#include "string.h"
#include "math.h"

typedef enum Error {
    error_none,
    error_digit_out_of_range,
    error_invalid_token,
} Error;

typedef struct Loc {
    String source;
    u64 offset;
    u32 line;
    u32 col;
} Loc;


// #define error(loc, ...) error_internal(loc, __VA_ARGS__)
// #define warning(loc, ...) //todo(redone): implement this!

void error(Loc loc, const char* fmt, ...);
void warning(Loc loc, const char* fmt, ...);