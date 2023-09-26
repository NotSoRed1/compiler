#pragma once

#include "utils/error.h"
#include "utils/string.h"
#include "utils/darray.h"
#include "utils/core.h"



typedef enum TokenKind {
    token_lparen        = '(',
    token_rparen        = ')',
    token_lbrace        = '{',
    token_rbrace        = '}',
    token_lbracket       = '[',
    token_rbracket       = ']',

    token_colon         = ':',
    token_semi_colon    = ';',
    token_comma         = ',',
    token_dot           = '.',
    token_bang          = '!',
    token_question       = '?',

    token_minus         = '-',
    token_plus          = '+',
    token_mul           = '*',
    token_div           = '/',
    token_mod           = '%',

    token_and           = '&',
    token_or            = '|',
    token_xor           = '^',
    token_not           = '~',

    token_lt            = '<',
    token_gt            = '>',
    token_eq            = '=',

    _token_offset = 127, // used as an offset to avoid token collisions

    token_shl,          // <<
    token_shr,          // >>
    token_logic_and,    // &&
    token_logic_or,     // ||
    token_colon_colon,  // ::

    token_eq_eq,        // == 
    token_cmp_start     = token_eq_eq,  // start of comparison ops
    token_bang_eq,      // !=
    token_lt_eq,        // <=
    token_gt_eq,        // >=
    token_cmp_end       = token_gt_eq,  // end of comparison ops


    token_minus_eq,     // -=
    token_asign_start   = token_minus_eq,  // start of assign ops
    token_plus_eq,      // +=
    token_mul_eq,       // *=
    token_div_eq,       // /=
    token_mod_eq,       // %=

    token_and_eq,       // &=
    token_or_eq,        // |=
    token_xor_eq,       // ^=
    token_shl_eq,       // <<=
    token_shr_eq,       // >>=
    token_asign_end     = token_shr_eq, // end of the asign ops



    // keywords
    token_fn, 
    token_let,
    token_const,
    token_if,
    token_else,
    token_while,
    token_for,
    token_return,
    token_break,
    token_continue,
    token_struct,
    token_union,
    token_enum, 
    token_as,
    token_defer,
    token_extern,
    token_import,
    token_typeid,
    token_sizeof,
    // token_offsetof,

    // literals
    token_name,
    token_int,
    token_float,
    token_str,

} TokenKind;


typedef struct Token {
    TokenKind kind;
    Loc       location;
    union {
        String  string;
        u64     integer;
        f64     float_value;
    };
} Token;


const char* token_string(TokenKind kind);


typedef struct Lexer {
    String  source;
    Loc     loc;
} Lexer;


Token*  lex_module(String source);


