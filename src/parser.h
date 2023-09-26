#pragma once

#include "ast.h"


typedef struct Parser {
    Token* tokens;
    u64    cursor;

    u32 loop_scope_level;
    u32 scope_level;
    b8 is_cond_expr;
    b8 is_extern;
} Parser;


Module* parse_module(String source);



