#include "lexer.h"


const char* token_string(TokenKind kind) {
    switch (kind) {
        case token_lparen       : return "(";
        case token_rparen       : return ")";
        case token_lbrace       : return "{";
        case token_rbrace       : return "}";

        case token_lbracket     : return "[";
        case token_rbracket     : return "]";
        case token_colon        : return ":";
        case token_semi_colon   : return ";";
        case token_comma        : return ",";
        case token_dot          : return ".";
        case token_bang         : return "!";
        case token_question     : return "?";
        case token_minus        : return "-";
        case token_plus         : return "+";
        case token_mul          : return "*";
        case token_div          : return "/";
        case token_mod          : return "%";
        case token_and          : return "&";
        case token_or           : return "|";
        case token_xor          : return "^";
        case token_not          : return "~";
        case token_shl          : return "<<";
        case token_shr          : return ">>";
        case token_logic_and    : return "&&";
        case token_logic_or     : return "||";
        case token_colon_colon  : return "::";
        case token_lt           : return "<";
        case token_gt           : return ">";
        case token_eq_eq        : return "==";
        case token_bang_eq      : return "!=";
        case token_lt_eq        : return "<=";
        case token_gt_eq        : return ">=";
        case token_eq           : return  "=";
        case token_minus_eq     : return "-=";
        case token_plus_eq      : return "+=";
        case token_mul_eq       : return "*=";
        case token_div_eq       : return "/=";
        case token_mod_eq       : return "%=";
        case token_and_eq       : return "&=";
        case token_or_eq        : return "|=";
        case token_xor_eq       : return "^=";
        case token_shl_eq       : return "<<=";
        case token_shr_eq       : return ">>=";



        case token_fn           : return "fn";
        case token_let          : return "let";
        case token_const        : return "const";
        case token_if           : return "if";
        case token_else         : return "else";
        case token_for          : return "for";
        case token_while        : return "while";
        case token_return       : return "return";
        case token_break        : return "break";
        case token_continue     : return "continue";
        case token_struct       : return "struct";
        case token_union        : return "union";
        case token_enum         : return "enum"; 
        case token_as           : return "as";
        case token_defer        : return "defer";
        case token_extern       : return "extern";
        case token_import       : return "import";
        case token_typeid       : return "typeid";
        case token_sizeof       : return "sizeof";
        // case token_typedef      : return "typedef";


        case token_name         : return "name";
        case token_int          : return "number";
        case token_float        : return "float";
        case token_str          : return "string";
        default                 : return "";
    }
}



void lexer_init(Lexer* lexer, String source) {
    lexer->source = source;
    lexer->loc.col = 0;
    lexer->loc.line = 0;
    lexer->loc.offset = 0;
    lexer->loc.source = source;
}

u8 lexer_advance(Lexer* lexer) {
    lexer->loc.offset += 1;
    lexer->loc.col    += 1;

    return lexer->source.buffer[lexer->loc.offset - 1];
}


u8 lexer_current(Lexer* lexer) {
    return lexer->source.buffer[core_min(lexer->loc.offset, lexer->source.length - 1)];
}


u8 lexer_previous(Lexer* lexer) {
    return lexer->source.buffer[core_max(lexer->loc.offset - 1, 0)];
}


u8 lexer_peek(Lexer* lexer) {
    return lexer->source.buffer[lexer->loc.offset + 1];
}

u8 lexer_peekn(Lexer* lexer, u32 offset) {
    return lexer->source.buffer[lexer->loc.offset + offset];
}


void lexer_advance_new_line(Lexer* lexer, b8 cond) {
    lexer->loc.col    = 0;
    lexer->loc.line   += 1;
    lexer->loc.offset += 1;

    if (cond) { 
        lexer->loc.offset += 1;
    }
}


b8 lexer_is_eof(Lexer* lexer) {
    return lexer->source.buffer[lexer->loc.offset] == '\0' ||
            lexer->loc.offset >= lexer->source.length;
}


void lexer_skip_whitespace(Lexer* lexer) {
    while (lexer_current(lexer) == ' ' || lexer_current(lexer) == '\t') {
        lexer_advance(lexer);
    }
}

static u8 digit_to_int[256] = {
    ['0'] = 0,
    ['1'] = 1,
    ['2'] = 2,
    ['3'] = 3,
    ['4'] = 4,
    ['5'] = 5,
    ['6'] = 6,
    ['7'] = 7,
    ['8'] = 8,
    ['9'] = 9,
    ['a'] = 10, ['A'] = 10,
    ['b'] = 11, ['B'] = 11,
    ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13,
    ['e'] = 14, ['E'] = 14,
    ['f'] = 15, ['F'] = 15,
};

Error lexer_lex_number(Lexer* lexer, u8 base, u64* out) {
    u64 result = 0;
    while (!lexer_is_eof(lexer)) {
        if (lexer_current(lexer) == '_') {
            lexer_advance(lexer);
            continue;
        }
        u8 current = lexer_current(lexer);
        u8 digit = digit_to_int[current];
        if (digit == 0 && lexer_current(lexer) != '0') {
            break;
        }

        if (digit >= base) {
            error(lexer->loc, "Digit is out of range for base %u. \n", base);
        }

        result = result * base + digit;
        lexer_advance(lexer);
    }

    *out = result;
    return error_none;
} 

Token token_init(TokenKind kind, Loc location) {
    Token token;
    token.kind = kind;
    token.location = location;

    return token;
}


#define case2(ch1, ch2, kind1, kind2) \
    case ch1: { \
        Loc location = lexer->loc; \
        lexer_advance(lexer); \
        if (lexer_current(lexer) == ch2) { \
            darray_push(tokens, token_init(kind2, location)); \
            lexer_advance(lexer); \
        } else { \
            darray_push(tokens, token_init(kind1, location)); \
        } \
    } break;



#define case3(ch1, ch2, kind1, kind2, kind3) \
    case ch1: { \
        Loc location = lexer->loc; \
        lexer_advance(lexer); \
        if (lexer_current(lexer) == ch2) { \
            darray_push(tokens, token_init(kind2, location)); \
            lexer_advance(lexer); \
        } else if (lexer_current(lexer) == ch1) { \
            darray_push(tokens, token_init(kind3, location)); \
            lexer_advance(lexer); \
        } else { \
            darray_push(tokens, token_init(kind1, location)); \
        } \
    } break;



#define case4(ch1, ch2, kind1, kind2, kind3, kind4) \
    case ch1: { \
        Loc location = lexer->loc; \
        lexer_advance(lexer); \
        if (lexer_current(lexer) == ch2) { \
            darray_push(tokens, token_init(kind2, location)); \
            lexer_advance(lexer); \
        } else if (lexer_current(lexer) == ch1) { \
            if (lexer_peek(lexer) == ch2) { \
                darray_push(tokens, token_init(kind3, location)); \
                lexer_advance(lexer); \
                lexer_advance(lexer); \
            } else {\
                darray_push(tokens, token_init(kind4, location)); \
                lexer_advance(lexer); \
            } \
        } else { \
            darray_push(tokens, token_init(kind1, location)); \
        } \
    } break;



Token* lexer_lex(Lexer* lexer) {
    Token* tokens = darray_init(Token, 10);
    u8* str_buff  = darray_init(u8, 128);
    while (!lexer_is_eof(lexer)) {
        lexer_skip_whitespace(lexer);
        darray_reset(str_buff); // reset the string buffer so it can be used again.

        switch(lexer_current(lexer)) {
            case '\n': {
                lexer_advance_new_line(lexer, false);
            } break;


            case '\r': {
                if (lexer_current(lexer) == '\n') {
                    lexer_advance_new_line(lexer, true);
                    continue;
                }

                lexer_advance(lexer);
            } break;


            case '/': {
                if (lexer_peek(lexer) == '/') {
                    while (!lexer_is_eof(lexer)) {
                        if (lexer_current(lexer) == '\n') {
                            break;
                        } else if (lexer_current(lexer) == '\r' && lexer_peek(lexer) == '\n') {
                            break;
                        }
                        lexer_advance(lexer);
                    }
                } else {
                    if (lexer_peek(lexer) == '=') {
                        darray_push(tokens, token_init(token_div_eq, lexer->loc));
                        lexer_advance(lexer);
                    } else {
                        darray_push(tokens, token_init(token_div, lexer->loc));
                    }
                    lexer_advance(lexer);
                }
            } break;

            case2('+', '=', token_plus, token_plus_eq);
            case2('-', '=', token_minus, token_minus_eq);
            case2('*', '=', token_mul, token_mul_eq);
            case2('%', '=', token_mod, token_mod_eq);
            case2('=', '=', token_eq, token_eq_eq);
            case2('!', '=', token_bang, token_bang_eq);
            case2('^', '=', token_xor, token_xor_eq);
            case2(':', ':', token_colon, token_colon_colon);

            case3('&', '=', token_and, token_and_eq, token_logic_and);
            case3('|', '=', token_or, token_or_eq, token_logic_or);
            case4('<', '=', token_lt, token_lt_eq, token_shl_eq, token_shl);
            case4('>', '=', token_gt, token_gt_eq, token_shr_eq, token_shr);

            case '(':
            case ')':
            case '{':
            case '}':
            case '[':
            case ']':
            case '~':
            case '?':
            case ',':
            case '.':
            case ';': {
                darray_push(tokens, token_init((TokenKind)lexer_current(lexer), lexer->loc))
                lexer_advance(lexer);
            } break;

            case '0': {
                Token token = token_init(token_int, lexer->loc);
                // lexer_advance(lexer);

                u8 base = 10;
                u8 next = lexer_peek(lexer);
                if (to_lower_case(next) == 'x') {
                    base = 16;
                } else if (to_lower_case(next) == 'b') {
                    base = 2;
                } else if (to_lower_case(next) == 'o') {
                    base = 8;
                } else{
                    goto default_case;
                }

                lexer_advance(lexer);
                lexer_advance(lexer);
                if (lexer_lex_number(lexer, base, &token.integer)) {
                    darray_free(tokens);
                    return null;
                }

                darray_push(tokens, token);
            } break;

            case '\'': {
                lexer_advance(lexer);
                Token token = token_init(token_int, lexer->loc);

                u8 curr = lexer_current(lexer);
                if (curr == '\\') {
                    lexer_advance(lexer);
                    switch (lexer_current(lexer)) {
                        case 'a': token.integer = '\a'; break;
                        case 'b': token.integer = '\b'; break;
                        case 'f': token.integer = '\f'; break;
                        case 'n': token.integer = '\n'; break;
                        case 'r': token.integer = '\r'; break;
                        case 't': token.integer = '\t'; break;
                        case 'v': token.integer = '\n'; break;
                        case '"': token.integer = '"';   break;
                        case '\'': token.integer = '\''; break;
                        case '\\': token.integer = '\\'; break;
                        default: error(lexer->loc, "Unexpected escape character. \n");
                    }
                    lexer_advance(lexer);
                } else {
                    token.integer = curr;
                    lexer_advance(lexer);
                }
                if (lexer_current(lexer) != '\'') {
                    error(token.location, "Expected a closing <'> after character literal. \n");
                }
                lexer_advance(lexer);
                darray_push(tokens, token);
            } break;

            case '"': {
                lexer_advance(lexer);
                Loc location = lexer->loc;
                while (lexer_current(lexer) != '"' && !lexer_is_eof(lexer)) {
                    u8 curr = lexer_current(lexer);
                    if (curr == '\\') {
                        lexer_advance(lexer);
                        switch (lexer_current(lexer)) {
                            case 'a': darray_push(str_buff, '\a'); break;
                            case 'b': darray_push(str_buff, '\b'); break;
                            case 'f': darray_push(str_buff, '\f'); break;
                            case 'n': darray_push(str_buff, '\n'); break;
                            case 'r': darray_push(str_buff, '\r'); break;
                            case 't': darray_push(str_buff, '\t'); break;
                            case 'v': darray_push(str_buff, '\n'); break;
                            case '"': darray_push(str_buff, '"'); break;
                            case '\'': darray_push(str_buff, '\''); break;
                            case '\\': darray_push(str_buff, '\\'); break;
                            default: error(lexer->loc, "Unexpected escape character. \n");
                        }
                        lexer_advance(lexer);
                    } else {
                        darray_push(str_buff, curr);
                        lexer_advance(lexer);
                    }
                }
                lexer_advance(lexer); // eat the " character

                Token token = token_init(token_str, location);
                token.string = string_cpy(str_buff, darray_length(str_buff));

                darray_push(tokens, token);
            } break;

            default: {
default_case:
                if (is_digit(lexer_current(lexer))) {
                    Token token = token_init(token_int, lexer->loc);
                    u32 offset = 0;
                    b8 is_float = false;

                    while (is_digit(lexer_peekn(lexer, offset)) || lexer_peekn(lexer, offset) == '.') {
                        if (lexer_peekn(lexer, offset) == '.') {
                            is_float = true;
                        }
                        offset += 1;
                    }

                    if (is_float) {
                        token.kind = token_float;
                        char* end = null; 
                        token.float_value = strtof((char*)&lexer->source.buffer[lexer->loc.offset], &end);
                        lexer->loc.offset += offset;
                        lexer->loc.col += offset;

                    } else {
                        if (lexer_lex_number(lexer, 10, &token.integer) != error_none) {
                            darray_free(tokens);
                            return null;
                        }
                    }

                    darray_push(tokens, token);
                } else if (is_alpha(lexer_current(lexer))) {
                    u64 start = lexer->loc.offset;
                    Loc start_loc = lexer->loc;

                    lexer_advance(lexer);
                    while ((is_alpha(lexer_current(lexer)) || is_digit(lexer_current(lexer))) 
                        && !lexer_is_eof(lexer)) {

                        lexer_advance(lexer);
                    }

                    Token token = token_init(token_name, start_loc);
                    token.string = string_init(&lexer->source.buffer[start], lexer->loc.offset - start);

                    if (string_eq_cstr(token.string,        "fn")) {
                        token.kind = token_fn;
                    } else if (string_eq_cstr(token.string, "let")) {
                        token.kind = token_let;
                    } else if (string_eq_cstr(token.string, "const")) {
                        token.kind = token_const;
                    } else if (string_eq_cstr(token.string, "if")) {
                        token.kind = token_if;
                    } else if (string_eq_cstr(token.string, "else")) {
                        token.kind = token_else;
                    } else if (string_eq_cstr(token.string, "for")) {
                        token.kind = token_for;
                    } else if (string_eq_cstr(token.string, "while")) {
                        token.kind = token_while;
                    } else if (string_eq_cstr(token.string, "return")) {
                        token.kind = token_return;
                    } else if (string_eq_cstr(token.string, "break")) {
                        token.kind = token_break;
                    } else if (string_eq_cstr(token.string, "continue")) {
                        token.kind = token_continue;
                    } else if (string_eq_cstr(token.string, "struct")) {
                        token.kind = token_struct;
                    } else if (string_eq_cstr(token.string, "union")) {
                        token.kind = token_union;
                    } else if (string_eq_cstr(token.string, "enum")) {
                        token.kind = token_enum;
                    } else if (string_eq_cstr(token.string, "as")) {
                        token.kind = token_as;
                    } else if (string_eq_cstr(token.string, "defer")) {
                        token.kind = token_defer;
                    } else if (string_eq_cstr(token.string, "extern")) {
                        token.kind = token_extern;
                    } else if (string_eq_cstr(token.string, "import")) {
                        token.kind = token_import;
                    } else if (string_eq_cstr(token.string, "typeid")) {
                        token.kind = token_typeid;
                    } else if (string_eq_cstr(token.string, "sizeof")) {
                        token.kind = token_sizeof;
                    }
                    darray_push(tokens, token);
                } 
                else {
                    darray_free(tokens);
                    darray_free(str_buff);
                    error(lexer->loc, "Unexpected token '%c' was found in the source code. \n", lexer_current(lexer));
                }
            } 
        }
    }

    darray_free(str_buff);
    return tokens;
}


Token* lex_module(String source) {
    Lexer lexer;
    lexer_init(&lexer, source);
    
    return lexer_lex(&lexer);
}