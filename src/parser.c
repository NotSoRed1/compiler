#include "parser.h"

Expr*     parse_expr(Parser* parser);
TypeSpec* parse_type_spec(Parser* parser);
Stmt*     parse_stmt(Parser* parser);
Stmt**    parse_stmt_block(Parser* parser);


void parser_init(Parser* parser, String source) {
    parser->tokens = lex_module(source);
    parser->cursor = 0;
    parser->is_cond_expr = false;
    parser->is_extern = false;
    parser->loop_scope_level = 0;
    parser->scope_level = 0;
}


void parser_free(Parser* parser) {
    darray_free(parser->tokens);
    parser->cursor = 0;
}


Token parser_current(Parser* parser) {
    u64 length = darray_length(parser->tokens) - 1;
    return parser->tokens[parser->cursor <= length ? parser->cursor: length];
}


Token parser_peek(Parser* parser) {
    u64 length = darray_length(parser->tokens) - 1;
    return parser->tokens[parser->cursor + 1 <= length ? parser->cursor + 1: length];
}


Token parser_advance(Parser* parser) {
    return parser->tokens[parser->cursor++];
}

Token parser_expect(Parser* parser, TokenKind kind) {
    Token tk = parser_current(parser);
    if (tk.kind != kind) {

        error(tk.location, "Unexpected token was found. \n");

        exit(69);
    }
    parser_advance(parser);
    return tk;
}


b8 parser_is_eof(Parser* parser) {
    return parser->cursor >= darray_length(parser->tokens);
}

StructInitItem parse_struct_init_item(Parser* parser) {
    StructInitItem item;
    Loc location = parser_current(parser).location;
    item.name = parser_expect(parser, token_name).string;
    parser_expect(parser, token_colon);
    item.expr = parse_expr(parser);
    item.location = location;

    return item;
}

Expr* parse_compound_expr(Parser* parser) {
    Loc loc = parser_current(parser).location;
    String name = parser_expect(parser, token_name).string;
    parser_expect(parser, token_lbrace);
    StructInitItem* items = darray_init(StructInitItem, 2);

    if (parser_current(parser).kind != token_rbrace) {

        darray_push(items, parse_struct_init_item(parser));
        while (parser_current(parser).kind == token_comma) {
            parser_advance(parser);
            darray_push(items, parse_struct_init_item(parser));
        }
    }

    parser_expect(parser, token_rbrace);

    return expr_compound_init(loc, name, items);
}

Expr* parse_operand(Parser* parser) {
    switch (parser_current(parser).kind) {
        case token_name: {
            if (!parser->is_cond_expr) {
                if (parser_peek(parser).kind == token_lbrace) {
                    return parse_compound_expr(parser);
                }
            }
            Token name = parser_advance(parser);
            return expr_name_init(name.location, name.string);
        } break;

        case token_int: {
            Token num = parser_advance(parser);
            return expr_int_init(num.location, num.integer);
        } break;
        
        case token_float: {
            Token num = parser_advance(parser);
            return expr_float_init(num.location, num.float_value);
        } break;

        case token_str: {
            Token string = parser_advance(parser);
            return expr_str_init(string.location, string.string);
        } break;

        case token_lparen: {
            parser_advance(parser);
            Expr* expr = parse_expr(parser);
            parser_expect(parser, token_rparen);
            return expr;
        } break;

        case token_lbracket: {
            Loc location = parser_advance(parser).location;
            Expr** exprs = darray_init(Expr*, 2);

            if (parser_current(parser).kind != token_rbracket) {
                darray_push(exprs, parse_expr(parser));
                while (parser_current(parser).kind == token_comma) {
                    parser_advance(parser);
                    if(parser_current(parser).kind == token_rbracket) {
                        break;
                    }

                    darray_push(exprs, parse_expr(parser));
                }
            }

            parser_expect(parser, token_rbracket);
            return expr_init_list_init(location, exprs);
        } break;

        case token_typeid: {
            Loc location = parser_advance(parser).location;
            parser_expect(parser, token_lparen);
            Expr* expr = null;
            if (parser_current(parser).kind == token_colon) {
                parser_advance(parser);
                expr = expr_typeid_type_init(location, parse_type_spec(parser));
            } else {
                expr = expr_typeid_expr_init(location, parse_expr(parser));
            }

            parser_expect(parser, token_rparen);
            return expr;
        } break;

        case token_sizeof: {
            Loc location = parser_advance(parser).location;
            parser_expect(parser, token_lparen);
            Expr* expr = null;
            if (parser_current(parser).kind == token_colon) {
                parser_advance(parser);
                expr = expr_sizeof_type_init(location, parse_type_spec(parser));
            } else {
                expr = expr_sizeof_expr_init(location, parse_expr(parser));
            }
            parser_expect(parser, token_rparen);
            return expr;
        } break;

        default: {
            return expr_none_init();
        } break;
    }
}

Expr** parse_call_params(Parser* parser) {
    parser_expect(parser, token_lparen);
    Expr** params = darray_init(Expr*, 2);
    if (parser_current(parser).kind != token_rparen) {
        darray_push(params, parse_expr(parser));
        while (parser_current(parser).kind == ',') {
            if (parser_peek(parser).kind == token_rparen) {
                parser_advance(parser);
                break;
            }

            parser_advance(parser);
            darray_push(params, parse_expr(parser));
        }
    }

    parser_expect(parser, token_rparen);

    return params;
}

Expr* parse_primary(Parser* parser) {
    Expr* expr = parse_operand(parser);
    while(!parser_is_eof(parser)) {
        if (parser_current(parser).kind == token_lparen) {
            Loc location = parser_current(parser).location;
            expr = expr_call_init(location, expr, parse_call_params(parser));
        } else if (parser_current(parser).kind == token_dot) {
            Loc location = parser_advance(parser).location;
            expr = expr_member_init(location, expr, parser_expect(parser, token_name).string);
        } else if (parser_current(parser).kind == token_lbracket) {
            Loc location = parser_advance(parser).location;
            expr = expr_index_init(location, expr, parse_expr(parser));
            parser_expect(parser, token_rbracket);
        } else {
            break;
        }
    }
    return expr;
}


Expr* parse_unary_expr(Parser* parser) {
    if (is_unary_op(parser_current(parser).kind)) {
        Token op = parser_advance(parser);

        return expr_unary_init(op.location, op.kind, parse_primary(parser));
    } else {
        return parse_primary(parser);
    }
}

Expr* parse_cast_expr(Parser* parser) {
    Expr* left = parse_unary_expr(parser);
    while (parser_current(parser).kind == token_as) {
        Loc location = parser_advance(parser).location;
        left = expr_cast_init(location, left, parse_type_spec(parser));
    }

    return left;
}


Expr* parse_mul_expr(Parser* parser) {
    Expr* left = parse_cast_expr(parser);
    while (is_mul_op(parser_current(parser).kind)) {
        Token op = parser_advance(parser); 
        left = expr_binary_init(op.location, op.kind, left, parse_cast_expr(parser));
    }

    return left;
}


Expr* parse_add_expr(Parser* parser) {
    Expr* left = parse_mul_expr(parser);
    while (is_add_op(parser_current(parser).kind)) {
        Token op = parser_advance(parser); 
        left = expr_binary_init(op.location, op.kind, left, parse_mul_expr(parser));
    }

    return left;
}

Expr* parse_shift_expr(Parser* parser) {
    Expr* left = parse_add_expr(parser);
    while (parser_current(parser).kind == token_shl || parser_current(parser).kind == token_shr) {
        Token op = parser_advance(parser);
        left = expr_binary_init(op.location, op.kind, left, parse_add_expr(parser));
    }

    return left;
}


Expr* parse_and_expr(Parser* parser) {
    Expr* left = parse_shift_expr(parser);
    while (parser_current(parser).kind == token_and) {
        Token op = parser_advance(parser);
        left = expr_binary_init(op.location, op.kind, left, parse_shift_expr(parser));
    }

    return left;
}

Expr* parse_xor_expr(Parser* parser) {
    Expr* left = parse_and_expr(parser);
    while (parser_current(parser).kind == token_xor) {
        Token op = parser_advance(parser);
        left = expr_binary_init(op.location, op.kind, left, parse_and_expr(parser));
    }

    return left;
}

Expr* parse_or_expr(Parser* parser) {
    Expr* left = parse_xor_expr(parser);
    while (parser_current(parser).kind == token_or) {
        Token op = parser_advance(parser);
        left = expr_binary_init(op.location, op.kind, left, parse_xor_expr(parser));
    }

    return left;
}


Expr* parse_cmp_expr(Parser* parser) {
    Expr* left = parse_or_expr(parser);
    while (is_cmp_op(parser_current(parser).kind)) {
        Token op = parser_advance(parser);
        left = expr_binary_init(op.location, op.kind, left, parse_or_expr(parser));
    }

    return left;
}

Expr* parse_logic_and_expr(Parser* parser) {
    Expr* left = parse_cmp_expr(parser);
    while (parser_current(parser).kind == token_logic_and) {
        Token op = parser_advance(parser);
        left = expr_binary_init(op.location, op.kind, left, parse_cmp_expr(parser));
    }

    return left;
}

Expr* parse_logic_or_expr(Parser* parser) {
    Expr* left = parse_logic_and_expr(parser);
    while (parser_current(parser).kind == token_logic_or) {
        Token op = parser_advance(parser);
        left = expr_binary_init(op.location, op.kind, left, parse_logic_and_expr(parser));
    }

    return left;
}

Expr* parse_ternary_expr(Parser* parser) {
    Loc location = parser_current(parser).location;
    Expr* cond = parse_logic_or_expr(parser);

    if (parser_current(parser).kind == token_question) {
        parser_advance(parser);
        Expr* then_expr = parse_ternary_expr(parser);
        parser_expect(parser, token_colon);
        Expr* else_expr = parse_ternary_expr(parser);

        return expr_ternary_init(location, cond, then_expr, else_expr);
    }

    return cond;
}

Expr* parse_expr(Parser* parser) {
    return parse_ternary_expr(parser);
}


TypeSpec* parse_type_spec(Parser* parser) {
    Loc location = parser_current(parser).location;
    TypeSpec* type;
    switch (parser_current(parser).kind) {
        case token_name: {
            Token name = parser_advance(parser);

            type = type_spec_name_init(location, name.string);
        } break;

        case token_fn: {
            parser_advance(parser); // eat the fn keyword
            parser_expect(parser, token_lparen);

            TypeSpec** args = darray_init(TypeSpec*, 10);
            if (parser_current(parser).kind != token_rparen) {
                darray_push(args, parse_type_spec(parser));
                while (!parser_is_eof(parser) && parser_current(parser).kind == ',') {
                    parser_advance(parser);
                    darray_push(args, parse_type_spec(parser));
                }
            }
            parser_expect(parser, token_rparen);
            parser_expect(parser, token_colon);
            type = type_spec_func_init(location, args, parse_type_spec(parser));
        } break;

        case token_lparen: {
            parser_advance(parser);
            type = parse_type_spec(parser);
            parser_expect(parser, token_rparen);
        } break;

        case token_mul: {
            parser_advance(parser);
            type = type_spec_ptr_init(location, parse_type_spec(parser));
        } break;

        default: {
            error(parser_current(parser).location, "Invalid type specifier. \n");
        }
    }

    while (parser_current(parser).kind == token_lbracket) {
        parser_advance(parser);
        u64 length = parser_expect(parser, token_int).integer;
        parser_expect(parser, token_rbracket);

        type = type_spec_array_init(location, type, length);
    }
    
    return type;
}

Stmt* parse_var_decl(Parser* parser, b8 is_const) {
    Loc location = {};
    if (is_const) {
        location = parser_expect(parser, token_const).location; // eat the let token
    } else {
        location = parser_expect(parser, token_let).location; // eat the let token
    }

    String name = parser_expect(parser, token_name).string;
    TypeSpec* type = null;
    if (parser_current(parser).kind == token_semi_colon) {
        error(parser_current(parser).location, "You cannot declare an uninitialized variable without a given type. \n");
    }

    if (parser_current(parser).kind == token_colon) {
        parser_advance(parser);
        type = parse_type_spec(parser);
    } else {
        type = type_spec_none_init();
    }
    if (parser_current(parser).kind == token_semi_colon) {
        if (is_const) {
            return stmt_const_decl_init(location, name, type, expr_none_init());
        } else {
            return stmt_var_decl_init(location, name, type, expr_none_init());
        }
    }

    if (parser->is_extern) {
        error(location, "Cannot initialize external variables. \n");
    }

    parser_expect(parser, token_eq);

    Stmt* result = null;
    if (is_const) {
        result = stmt_const_decl_init(location, name, type, parse_expr(parser));
    } else {
        result = stmt_var_decl_init(location, name, type, parse_expr(parser));
    }


    return result;
}

Argument parse_member(Parser* parser) {
    Argument arg;
    arg.location = parser_current(parser).location;

    arg.name = parser_expect(parser, token_name).string;
    parser_expect(parser, token_colon);
    arg.type = parse_type_spec(parser);

    return arg;
}
Argument* parse_compound(Parser* parser, TokenKind token) {
    Argument* args = darray_init(Argument, 2);

    if (parser_current(parser).kind != token) {
        darray_push(args, parse_member(parser));

        while (parser_current(parser).kind == token_comma) {
            if (parser_peek(parser).kind == token) {
                parser_advance(parser);
                break;
            }

            parser_advance(parser);
            darray_push(args, parse_member(parser));
        }
    }

    return args;
}

Stmt* parse_func_decl(Parser* parser, b8 is_extern) {
    Loc location = parser_expect(parser, token_fn).location; // eat the fn token
    String name = parser_expect(parser, token_name).string;

    parser_expect(parser, token_lparen); 
    Argument* args = parse_compound(parser, token_rparen);
    parser_expect(parser, token_rparen);

    TypeSpec* ret_type = null;
    if (parser_current(parser).kind == token_colon) {
        parser_advance(parser);
        ret_type = parse_type_spec(parser);
    } else {
        ret_type = type_spec_none_init();
    }

    if (is_extern) {
        if (parser_current(parser).kind == token_lbrace) {
            error(parser_current(parser).location, "External functions must not have a function body. \n");
        }
        return stmt_func_decl_init(location, name, args, ret_type, null);
    }

    Stmt** body = parse_stmt_block(parser);
    return stmt_func_decl_init(location, name, args, ret_type, body);
}


Stmt* parse_struct_decl(Parser* parser) {
    Loc location = parser_expect(parser, token_struct).location;
    String name = parser_expect(parser, token_name).string;

    parser_expect(parser, token_lbrace);
    if (parser_current(parser).kind == token_rbrace) {
        error(parser_current(parser).location, "You cannot declare an empty struct. \n");
    }

    Member* members = parse_compound(parser, token_rbrace); // darray_init(Member, 2);
    parser_expect(parser, token_rbrace);

    return stmt_struct_decl_init(location, name, members);
}


Stmt* parse_union_decl(Parser* parser) {
    Loc location = parser_expect(parser, token_union).location;
    String name = parser_expect(parser, token_name).string;

    parser_expect(parser, token_lbrace);
    if (parser_current(parser).kind == token_rbrace) {
        error(parser_current(parser).location, "You cannot declare an empty union. \n");
    }

    Member* members = parse_compound(parser, token_rbrace); // darray_init(Member, 2);
    parser_expect(parser, token_rbrace);

    return stmt_union_decl_init(location, name, members);
}


Stmt* parse_enum_decl(Parser* parser) {
    Loc location = parser_expect(parser, token_enum).location;
    String name = parser_expect(parser, token_name).string;
    TypeSpec* type = type_spec_none_init();


    parser_expect(parser, token_lbrace);

    if (parser_current(parser).kind == token_rbrace) {
        error(parser_current(parser).location, "You cannot declare an empty enum. \n");
    }

    EnumItem* items = darray_init(EnumItem, 2);
    darray_push(items, enum_item_from_token(parser_expect(parser, token_name)));
    while (parser_current(parser).kind == token_comma) {
        if (parser_peek(parser).kind == token_rbrace) {
            parser_advance(parser);
            break;
        }

        parser_advance(parser);
        darray_push(items, enum_item_from_token(parser_expect(parser, token_name)));
    }

    parser_expect(parser, token_rbrace);

    return stmt_enum_decl_init(location, name, items, type);
}


Stmt* parse_while_stmt(Parser* parser) {
    parser->loop_scope_level += 1;
    parser_expect(parser, token_while); // eat the fn token

    parser->is_cond_expr = true;
    Expr* cond = parse_expr(parser);
    parser->is_cond_expr = false;

    Stmt** body = parse_stmt_block(parser);

    parser->loop_scope_level -= 1;
    return stmt_while_init(cond, body);
}

Stmt* parse_init_stmt(Parser* parser) {
    switch (parser_current(parser).kind) {
        case token_let: {
            return parse_var_decl(parser, false);
        } break;
        case token_const: {
            return parse_var_decl(parser, true);
        } break;
        default: {
            Expr* lhs = parse_expr(parser);
            if (!parser_is_eof(parser) && is_asign_op(parser_current(parser).kind)) {
                Token op = parser_advance(parser);
                Expr* rhs = parse_expr(parser);

                return stmt_assign_init(op.location, op.kind, lhs, rhs);
            } 

            return stmt_expr_init(lhs);
        } break;
    }
}

Stmt* parse_for_stmt(Parser* parser) {
    parser_expect(parser, token_for);
    b8 should_close = false;
    if (parser_current(parser).kind == token_lparen) {
        parser_advance(parser);
        should_close = true;
    }

    parser->is_cond_expr = true;
    Stmt* init = parse_init_stmt(parser);

    parser_expect(parser, token_semi_colon);
    Expr* cond = parse_expr(parser);
    parser_expect(parser, token_semi_colon);

    Stmt* post = parse_init_stmt(parser);
    parser->is_cond_expr = false;

    if (should_close) {
        parser_expect(parser, token_rparen);
    }

    return stmt_for_init(init, cond, post, parse_stmt_block(parser));
}

IfStmt* parse_if(Parser* parser, b8 is_elseif) {
    if (is_elseif) parser_expect(parser, token_else);
    parser_expect(parser, token_if);

    parser->is_cond_expr = true;
    Expr* cond = parse_expr(parser);
    parser->is_cond_expr = false;

    Stmt** then = parse_stmt_block(parser);
    return if_init(cond, then);
}

Stmt* parse_if_stmt(Parser* parser) {
    parser->scope_level += 1;
    IfStmt* if_ = parse_if(parser, false);

    IfStmt** else_ifs = darray_init(IfStmt*, 2);
    while (parser_current(parser).kind == token_else && parser_peek(parser).kind == token_if) {
        darray_push(else_ifs, parse_if(parser, true));
    }

    Stmt** else_ = null;
    if (parser_current(parser).kind == token_else) {
        parser_advance(parser);
        else_ = parse_stmt_block(parser);
    } 

    parser->scope_level -= 1;
    return stmt_if_init(if_, else_ifs , else_);
}


Stmt* parse_stmt(Parser* parser) {
    switch (parser_current(parser).kind) {
        case token_let: {
            Stmt* stmt = parse_var_decl(parser, false);
            parser_expect(parser, token_semi_colon);

            return stmt;
        } break;

        case token_const: {
            Stmt* stmt = parse_var_decl(parser, true);
            parser_expect(parser, token_semi_colon);

            return stmt;
        } break;

        case token_extern: {
            if (parser->scope_level > 0) {
                error(parser_current(parser).location, "exernal decls can only be declared in the global scope. \n");
            }
            parser_advance(parser);
            Token curr = parser_current(parser);
            parser->is_extern = true;
            Stmt* result = null;
            if (curr.kind == token_let) {
                Stmt* var = parse_var_decl(parser, false);
                var->decl.var.is_extern = true;
                result = var;
            } else if (curr.kind == token_const) {
                Stmt* var = parse_var_decl(parser, true);
                var->decl.var.is_extern = true;
                result = var;
            } else {
                Stmt* func = parse_func_decl(parser, true);
                func->decl.func.is_extern = true;
                result = func;
            }
            parser->is_extern = false;
            return result;
        } break;

        case token_import: {
            if (parser->scope_level > 0) {
                error(parser_current(parser).location, "Import statements can only be declared in the global scope. \n");
            }

            Loc location = parser_advance(parser).location;
            Stmt* stmt = stmt_import_decl_init(location, parser_expect(parser, token_str).string);
            parser_expect(parser, token_semi_colon);

            return stmt;
        } break;

        case token_fn: {
            if (parser->scope_level > 0) {
                error(parser_current(parser).location, "Functions can only be declared in the global scope. \n");
            }
            return parse_func_decl(parser, false);
        } break;

        case token_struct: {
            if (parser->scope_level > 0) {
                error(parser_current(parser).location, "Structs can only be declared in the global scope. \n");
            }
            return parse_struct_decl(parser);
        } break;

        case token_union: {
            if (parser->scope_level > 0) {
                error(parser_current(parser).location, "Structs can only be declared in the global scope. \n");
            }
            return parse_union_decl(parser);
        } break;

        case token_enum: {
            if (parser->scope_level > 0) {
                error(parser_current(parser).location, "Enums can only be declared in the global scope. \n");
            }
            return parse_enum_decl(parser);
        } break;

        case token_for: {
            if (parser->scope_level == 0) {
                error(parser_current(parser).location, "For loops cannot be used in the global scope. \n");
            }
            return parse_for_stmt(parser);
        } break;

        case token_while: {
            if (parser->scope_level == 0) {
                error(parser_current(parser).location, "While loops cannot be used in the global scope. \n");
            }
            return parse_while_stmt(parser);
        } break;

        case token_if: {
            if (parser->scope_level == 0) {
                error(parser_current(parser).location, "If statements cannot be used in the global scope. \n");
            }
            return parse_if_stmt(parser);
        } break;

        case token_lbrace: {
            if (parser->scope_level == 0) {
                error(parser_current(parser).location, "Statement blocks cannot be used in the global scope. \n");
            }
            return stmt_block_init(parse_stmt_block(parser));
        } break;

        case token_return: {
            parser_advance(parser);

            // if (parser_current(parser).kind == token_semi_colon) {
            //     parser_advance(parser);
            //     return stmt_return_init(expr_none_init());
            // }

            Expr* return_expr = parse_expr(parser);
            parser_expect(parser, token_semi_colon);
            return stmt_return_init(return_expr);
        } break;

        case token_break: {
            if (parser->loop_scope_level == 0) {
                error(parser_current(parser).location, "You cannot use break statements outside of a loop. \n");
            }
            parser_advance(parser);
            parser_expect(parser, token_semi_colon);
            return stmt_break_init();
        } break;

        case token_continue: {
            if (parser->loop_scope_level == 0) {
                error(parser_current(parser).location, "You cannot use continue statements outside of a loop. \n");
            }
            parser_advance(parser);
            parser_expect(parser, token_semi_colon);
            return stmt_break_init();
        } break;
        case token_defer: {
            if (parser->scope_level == 0) {
                error(parser_current(parser).location, "You cannot use defer statements in the global scope. \n");
            }
            parser_advance(parser);
            return stmt_defer_init(parse_stmt(parser));
        } break;
        default: {
            Expr* lhs = parse_expr(parser);
            if (!parser_is_eof(parser) && is_asign_op(parser_current(parser).kind)) {
                if (parser->scope_level == 0) {
                    error(parser_current(parser).location, "Invalid assign expression. You can't assign expression in the global scope. \n");
                }

                Token op = parser_advance(parser);
                Expr* rhs = parse_expr(parser);
                parser_expect(parser, token_semi_colon);
                return stmt_assign_init(op.location, op.kind, lhs, rhs);
            } 

            parser_expect(parser, token_semi_colon);
            return stmt_expr_init(lhs);
        } break;
    }

    return null;



}


Stmt** parse_stmt_block(Parser* parser) {
    parser->scope_level += 1;
    parser_expect(parser, token_lbrace);
    Stmt** stmts = darray_init(Stmt*, 2);
    while (parser_current(parser).kind != token_rbrace && !parser_is_eof(parser)) {
        darray_push(stmts, parse_stmt(parser));
    }

    parser_expect(parser, token_rbrace);

    parser->scope_level -= 1;
    return stmts;
}


Module* parse_module(String source) {
    Parser parser;
    parser_init(&parser, source);

    Module* module = (Module*)malloc(sizeof(Module));
    module->statements = darray_init(Stmt*, 2);
    while (!parser_is_eof(&parser)) {
        darray_push(module->statements, parse_stmt(&parser));
    }

    parser_free(&parser);
    return module;
}
