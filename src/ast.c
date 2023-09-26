#include "ast.h"


Expr* expr_none_init() {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_none;

    return out;
}

Expr* expr_name_init(Loc location, String name) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_name;
    out->name = name;
    out->location = location;

    return out;
}


Expr* expr_int_init(Loc location, u64 integer) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_int;
    out->integer = integer;
    out->location = location;

    return out;
}

Expr* expr_float_init(Loc location, f64 value) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_float;
    out->float_lit = value;
    out->location = location;

    return out;
}

Expr* expr_str_init(Loc location, String value) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));

    out->kind = expr_str;
    out->name = value;
    out->location = location;

    return out;
}

Expr* expr_ternary_init(Loc location, Expr* cond, Expr* then, Expr* else_expr) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_ternary;
    out->ternary.cond = cond;
    out->ternary.then_expr = then;
    out->ternary.else_expr = else_expr;
    out->location = location;

    return out;
}

Expr* expr_binary_init(Loc location, u8 op, Expr* lhs, Expr* rhs) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_binary;
    out->binary.op  = op;
    out->binary.lhs = lhs;
    out->binary.rhs = rhs;
    out->location = location;

    return out;
}

Expr* expr_unary_init(Loc location, u8 op, Expr* expr) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_unary;
    out->unary.op  = op;
    out->unary.expr = expr;
    out->location = location;

    return out;
}

Expr* expr_call_init(Loc location, Expr* callee, Expr** params) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_call;
    out->call.callee = callee;
    out->call.params = (Expr**)copy_darray_buffer(params);
    out->location = location;

    darray_free(params);

    return out;
}

Expr* expr_compound_init(Loc location, String name, StructInitItem* items) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_compound;
    out->compound.name = name;
    out->compound.items = (StructInitItem*)copy_darray_buffer(items);
    out->location = location;

    darray_free(items);

    return out;
}

Expr* expr_member_init(Loc location, Expr* expr, String name) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_member;
    out->member.expr = expr;
    out->member.name = name;
    out->location = location;

    return out;
}

Expr* expr_index_init(Loc location, Expr* expr, Expr* index) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_index;
    out->index.expr = expr;
    out->index.index = index;
    out->location = location;

    return out;
}


Expr* expr_init_list_init(Loc location, Expr** items) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));

    out->kind = expr_init_list;
    out->init_list.items= (Expr**)copy_darray_buffer(items);
    out->location = location;
    
    darray_free(items);

    return out;
}


Expr* expr_cast_init(Loc location, Expr* expr, TypeSpec* type) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_cast;
    out->cast.expr = expr;
    out->cast.type = type;
    out->location = location;

    return out;
}

Expr* expr_typeid_expr_init(Loc location, Expr* expr) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_typeid_expr;
    out->type_id.expr = expr;
    out->location = location;

    return out;
}

Expr* expr_typeid_type_init(Loc location, TypeSpec* type) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_typeid_type;
    out->type_id.type = type;
    out->location = location;

    return out;
}

Expr* expr_sizeof_expr_init(Loc location, Expr* expr) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_sizeof_expr;
    out->type_id.expr = expr;
    out->location = location;

    return out;
}

Expr* expr_sizeof_type_init(Loc location, TypeSpec* type) {
    Expr* out = (Expr*)global_allocator_alloc(sizeof(Expr));
    memset(out, 0, sizeof(Expr));
    out->kind = expr_sizeof_type;
    out->type_id.type = type;
    out->location = location;

    return out;
}

TypeSpec* type_spec_none_init() {
    TypeSpec* out= (TypeSpec*)global_allocator_alloc(sizeof(TypeSpec));
    out->kind = type_spec_none;

    return out;
}

TypeSpec* type_spec_name_init(Loc location, String name) {
    TypeSpec* out= (TypeSpec*)global_allocator_alloc(sizeof(TypeSpec));
    out->kind = type_spec_name;
    out->name = name;
    out->location = location;

    return out;
}

TypeSpec* type_spec_func_init(Loc location, TypeSpec** args, TypeSpec* ret_type) {
    TypeSpec* out= (TypeSpec*)global_allocator_alloc(sizeof(TypeSpec));
    out->kind = type_spec_func;
    out->func.args = (TypeSpec**)copy_darray_buffer(args);
    out->func.ret_type = ret_type;
    out->location = location;

    darray_free(args);

    return out;
}

TypeSpec* type_spec_ptr_init(Loc location, TypeSpec* base) {
    TypeSpec* out= (TypeSpec*)global_allocator_alloc(sizeof(TypeSpec));
    out->kind = type_spec_ptr;
    out->ptr.base = base;
    out->location = location;

    return out;
}

TypeSpec* type_spec_array_init(Loc location, TypeSpec* base, u64 length) {
    TypeSpec* out= (TypeSpec*)global_allocator_alloc(sizeof(TypeSpec));
    out->kind = type_spec_array;
    out->array.base = base;
    out->array.length = length;
    out->location = location;

    return out;
}

Stmt* stmt_const_decl_init(Loc location, String name, TypeSpec* type, Expr* expr) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_decl;
    out->decl.kind = decl_const;
    out->decl.name = name;
    out->decl.var.type = type;
    out->decl.var.value = expr;
    out->decl.location = location;

    return out;
}

Stmt* stmt_import_decl_init(Loc location, String name) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_decl;
    out->decl.kind = decl_import;
    out->decl.name = name;
    out->decl.location = location;

    return out;
}

Stmt* stmt_var_decl_init(Loc location, String name, TypeSpec* type, Expr* expr) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_decl;
    out->decl.kind = decl_var;
    out->decl.name = name;
    out->decl.var.type = type;
    out->decl.var.value = expr;
    out->decl.location = location;

    return out;
}

Stmt* stmt_func_decl_init(Loc location, String name, Argument* args, TypeSpec* ret_type, Stmt** body) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_decl;
    out->decl.kind = decl_func;
    out->decl.name = name;
    out->decl.func.ret_type = ret_type;
    out->decl.func.args = (Argument*)copy_darray_buffer(args);
    out->decl.func.body = body; // todo(redone): why copying this buffer causes a segfault!
    out->decl.location = location;
    out->decl.func.is_extern = false;

    darray_free(args);

    return out;
}


Stmt* stmt_struct_decl_init(Loc location, String name, Member* members) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_decl;
    out->decl.kind = decl_struct;
    out->decl.name = name;
    out->decl.compound.members = (Member*)copy_darray_buffer(members);
    out->decl.location = location;

    darray_free(members);

    return out;
}

Stmt* stmt_union_decl_init(Loc location, String name, Member* members) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_decl;
    out->decl.kind = decl_union;
    out->decl.name = name;
    out->decl.compound.members = (Member*)copy_darray_buffer(members);
    out->decl.location = location;

    darray_free(members);

    return out;
}

Stmt* stmt_enum_decl_init(Loc location, String name, EnumItem* items, TypeSpec* type) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_decl;
    out->decl.kind = decl_enum;
    out->decl.name = name;
    out->decl.enum_decl.type = type;
    out->decl.enum_decl.items= (EnumItem*)copy_darray_buffer(items);
    out->decl.location = location;

    darray_free(items);

    return out;
}

Stmt* stmt_while_init(Expr* cond, Stmt** body) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_while;
    out->while_stmt.cond = cond;
    out->while_stmt.body = (Stmt**)copy_darray_buffer(body);

    darray_free(body);

    return out;
}

Stmt* stmt_for_init(Stmt* init, Expr* cond, Stmt* post, Stmt** body) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_for;
    out->for_stmt.init = init;
    out->for_stmt.cond = cond;
    out->for_stmt.post = post;
    out->for_stmt.body = (Stmt**)copy_darray_buffer(body);

    darray_free(body);

    return out;
}

Stmt* stmt_block_init(Stmt** body) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));
    out->kind = stmt_block;
    out->block.body = (Stmt**)copy_darray_buffer(body);

    darray_free(body);

    return out;
}

IfStmt* if_init(Expr* cond, Stmt** body) {
    IfStmt* out = (IfStmt*)global_allocator_alloc(sizeof(IfStmt));
    memset(out, 0, sizeof(IfStmt));
    out->cond = cond;
    out->then = (Stmt**)copy_darray_buffer(body);

    darray_free(body);

    return out;
}

Stmt* stmt_if_init(IfStmt* if_stmt, IfStmt** else_ifs, Stmt** else_stmt) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));
    out->kind = stmt_if;
    out->if_stmt.if_stmt = if_stmt;
    out->if_stmt.else_ifs = (IfStmt**)copy_darray_buffer(else_ifs);
    out->if_stmt.else_stmt = else_stmt; // todo(redone): see why copying this also causes a segfault!

    darray_free(else_ifs);

    return out;
}

Stmt* stmt_assign_init(Loc location, u8 op, Expr* lhs, Expr* rhs) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));
    out->kind = stmt_assign;
    out->assign.lhs = lhs;
    out->assign.rhs = rhs;
    out->assign.op = op;
    out->location = location;

    return out;
}

Stmt* stmt_expr_init(Expr* expr) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));
    out->kind = stmt_expr;
    out->expr = expr;

    return out;
}

Stmt* stmt_break_init() {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));
    out->kind = stmt_break;

    return out;
}

Stmt* stmt_continue_init() {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));
    out->kind = stmt_continue;

    return out;
}

Stmt* stmt_return_init(Expr* return_expr) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));

    out->kind = stmt_return;
    out->expr = return_expr;

    return out;
}

Stmt*   stmt_defer_init(Stmt* defered) {
    Stmt* out = (Stmt*)global_allocator_alloc(sizeof(Stmt));
    memset(out, 0, sizeof(Stmt));
    out->kind = stmt_defer;
    out->defer = defered;

    return out;
}



#define print_tabs(count) for (i32 i = 0; i < (count); i++) { printf("  "); }

void print_expr(Expr* expr, u32 level, b8 is_last) {
    if (!expr) return;

    switch (expr->kind) {
        case expr_none:
            printf("None"); if (!is_last) printf(","); printf("\n");
            break;

        case expr_name:
            printf("NameExpr { name: '%.*s' }", (i32)expr->name.length, expr->name.buffer);
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_int:
            printf("IntLiteral { value: %llu }", expr->integer);
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_float:
            printf("FloatLiteral { value: %f }", expr->float_lit);
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_str:
            printf("StringLiteral { value: %.*s }", (i32)expr->name.length, expr->name.buffer);
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_cast:
            printf("CastExpr { \n");
            print_tabs(level + 1); printf("type: "); print_type_spec(expr->cast.type); printf(", \n");
            print_tabs(level + 1); print_expr(expr->cast.expr, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_ternary:
            printf("TernaryExpr { \n");
            print_tabs(level + 1); printf("cond: "); print_expr(expr->ternary.cond , level + 1, false);
            print_tabs(level + 1); printf("then: "); print_expr(expr->ternary.then_expr, level + 1, false);
            print_tabs(level + 1); printf("else: "); print_expr(expr->ternary.else_expr, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_binary:
            printf("BinaryExpr { \n");
            print_tabs(level + 1); printf("op: '%s', \n", token_string((TokenKind)expr->binary.op));
            print_tabs(level + 1); printf("left: "); print_expr(expr->binary.lhs, level + 1, false);
            print_tabs(level + 1); printf("right: "); print_expr(expr->binary.rhs, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_unary:
            printf("UnaryExpr { \n");
            print_tabs(level + 1); printf("op: '%s', \n", token_string((TokenKind)expr->unary.op));
            print_tabs(level + 1); printf("expr: "); print_expr(expr->unary.expr, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_member:
            printf("MemberExpr { \n");
            print_tabs(level + 1); printf("expr: "); print_expr(expr->member.expr, level + 1, false);
            print_tabs(level + 1); printf("member: %.*s\n", (i32)expr->member.name.length, expr->member.name.buffer); 
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_index:
            printf("IndexExpr { \n");
            print_tabs(level + 1); printf("expr: "); print_expr(expr->index.expr, level + 1, false);
            print_tabs(level + 1); printf("index: "); print_expr(expr->index.index, level + 1, false); 
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
            break;

        case expr_call: {
            u32 num_args = darray_length(expr->call.params);
            printf("CallExpr { \n");
            print_tabs(level + 1); printf("callee: "); print_expr(expr->call.callee, level + 1, false);
            print_tabs(level + 1); printf("args: ["); 
            if (!num_args) printf("]"); printf("\n");
            for (u32 i = 0; i < num_args; i++) {
                b8 last = (i == num_args - 1);
                print_tabs(level + 2);
                print_expr(expr->call.params[i], level + 2, last);
            }

            if (num_args) {
                print_tabs(level + 1); printf("]\n");
            }

            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
        } break;
        case expr_compound: {
            u32 num_args = darray_length(expr->compound.items);
            printf("Struct Initializer { \n");
            print_tabs(level + 1); printf("name: %.*s, \n", (i32)expr->compound.name.length, expr->compound.name.buffer);
            print_tabs(level + 1); printf("items: ["); 
            if (!num_args) printf("]"); printf("\n");
            for (u32 i = 0; i < num_args; i++) {
                b8 last = (i == num_args - 1);
                print_tabs(level + 2);
                print_expr(expr->compound.items[i].expr, level + 2, last);
            }

            if (num_args) {
                print_tabs(level + 1); printf("]\n");
            }

            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");

        } break;
        case expr_init_list: {
            u32 num_items = darray_length(expr->init_list.items);
            printf("Initializer List {\n");
            print_tabs(level + 1); printf("items: ["); 
            if (!num_items) printf("]"); printf("\n");
            for (u32 i = 0; i < num_items; i++) {
                b8 last = (i == num_items - 1);
                print_tabs(level + 2);
                print_expr(expr->init_list.items[i], level + 2, last);
            }

            if (num_items) {
                print_tabs(level + 1); printf("]\n");
            }

            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");

        } break;
        case expr_typeid_expr: {
            printf("TypeIdExpr { \n");
            print_tabs(level + 1); printf("expr: ");
            print_expr(expr->type_id.expr, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
        } break;
        case expr_typeid_type: {
            printf("TypeIdType { \n");
            print_tabs(level + 1); printf("type: ");
            print_type_spec(expr->type_id.type); printf("\n");
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
        } break;
        case expr_sizeof_expr: {
            printf("SizeOfExpr { \n");
            print_tabs(level + 1); printf("expr: ");
            print_expr(expr->type_id.expr, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
        } break;
        case expr_sizeof_type: {
            printf("SizeOfType { \n");
            print_tabs(level + 1); printf("type: ");
            print_type_spec(expr->type_id.type); printf("\n");
            print_tabs(level); printf("}");
            if (!is_last) printf(",");
            printf("\n");
        } break;
        }
}


void print_type_spec(TypeSpec* spec) {
    if (!spec) return;

    switch (spec->kind) {
    case type_spec_none:
        printf("undefined");
        break;
    case type_spec_name:
        printf("%.*s", (i32)spec->name.length, spec->name.buffer);
        break;
    case type_spec_func:
        printf("fn(");
        for (u32 i = 0; i < darray_length(spec->func.args); i++) {
            print_type_spec(spec->func.args[i]);
            if (i != darray_length(spec->func.args) - 1) {
                printf(", ");
            }
        }
        printf(")");
        if (spec->func.ret_type) {
            printf(": ");
            print_type_spec(spec->func.ret_type);
        }
        break;
        case type_spec_ptr: {
            printf("*");
            print_type_spec(spec->ptr.base);
        } break;
        case type_spec_array: {
            print_type_spec(spec->array.base);
            printf("[%llu]", spec->array.length);
        }
    } 
}


void print_func_args(Argument* args, u32 level, b8 is_struct) {
    u32 num_args = darray_length(args);
    print_tabs(level); if (is_struct) printf("items: ["); else printf("args: [");
    if (!num_args) printf("]"); printf("\n");
    for (u32 i = 0; i < num_args; i++) {
        print_tabs(level + 1); 
        if (is_struct) printf("SturctItem {"); else printf("FuncArg {");
        printf("name: %.*s, type: ", (i32)args[i].name.length, args[i].name.buffer);
        print_type_spec(args[i].type); printf(" }");
        if (i != num_args - 1) printf(",");
        printf("\n");
    }
    print_tabs(level); printf("],\n");
}


void print_func_body(Stmt** body, u32 level) {
    u32 num_stmts = darray_length(body);
    print_tabs(level); printf("body: [");
    if (num_stmts) printf("\n");
    for (u32 i = 0; i < num_stmts; i++) {
        print_stmt(body[i], level + 1, i == num_stmts - 1, true);
    }
    print_tabs(level); printf("]\n");
}


void print_decl(Decl decl, u32 level, b8 is_last) {
    switch (decl.kind) {
        case decl_import: {

        } break;
        case decl_var: {
            print_tabs(level); printf("VarDecl {\n");
            print_tabs(level + 1); printf("type: "); print_type_spec(decl.var.type); printf(",\n");
            print_tabs(level + 1); printf("name: '%.*s',\n", (i32)decl.name.length, decl.name.buffer);
            print_tabs(level + 1); printf("value: "); print_expr(decl.var.value, level + 1, true); 
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
        } break;
        case decl_const: {
            print_tabs(level); printf("ConstDecl {\n");
            print_tabs(level + 1); printf("type: "); print_type_spec(decl.var.type); printf(",\n");
            print_tabs(level + 1); printf("name: '%.*s',\n", (i32)decl.name.length, decl.name.buffer);
            print_tabs(level + 1); printf("value: "); print_expr(decl.var.value, level + 1, true); 
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
        } break;

        case decl_func: {
            print_tabs(level); printf("FuncDecl {\n");
            print_tabs(level + 1); printf("name: '%.*s',\n", (i32)decl.name.length, decl.name.buffer);
            print_tabs(level + 1); printf("ret_type: "); print_type_spec(decl.func.ret_type); printf(",\n");
            print_func_args(decl.func.args, level + 1, false);
            if (decl.func.body) {
                print_func_body(decl.func.body, level + 1);
            }
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
        } break;

        case decl_enum: {
            print_tabs(level); printf("EnumDecl {\n");
            print_tabs(level + 1); printf("name: '%.*s',\n", (i32)decl.name.length, decl.name.buffer);
            print_tabs(level + 1); printf("type: "); print_type_spec(decl.enum_decl.type); printf(",\n");
            print_tabs(level + 1); printf("items: [");
            u32 items_count = darray_length(decl.enum_decl.items);
            if (!items_count) printf("]"); printf("\n");
            for (u32 i = 0; i < items_count; i++) {
                print_tabs(level + 2); printf("%.*s", (i32)decl.enum_decl.items[i].name.length, decl.enum_decl.items[i].name.buffer);
                if (i < items_count - 1) printf(","); printf("\n");
            }
            print_tabs(level + 1); printf("]\n");
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
        } break;

        case decl_struct: {
            print_tabs(level); printf("StructDecl {\n");
            print_tabs(level + 1); printf("name: '%.*s',\n", (i32)decl.name.length, decl.name.buffer);
            print_func_args(decl.compound.members, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
        } break;
        case decl_union: {
            print_tabs(level); printf("StructDecl {\n");
            print_tabs(level + 1); printf("name: '%.*s',\n", (i32)decl.name.length, decl.name.buffer);
            print_func_args(decl.compound.members, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
        } break;
        }
}


void print_if_stmt(IfStmt* stmt, const char* name, u32 level, b8 is_last) {
    print_tabs(level); printf("%s {\n", name);
    print_tabs(level + 1); printf("cond: "); print_expr(stmt->cond, level + 1, false);
    print_func_body(stmt->then, level + 1);
    print_tabs(level); printf("}");
    if (!is_last) printf(","); printf("\n");
}


void print_stmt(Stmt* stmt, u32 level, b8 is_last, b8 add_spaces) {
    if (!stmt) return;

    switch (stmt->kind) {
        case stmt_expr:
            print_expr(stmt->expr, level, is_last);
            break;

        case stmt_decl:
            print_decl(stmt->decl, level, is_last);
            break;

        case stmt_assign:
            if (add_spaces) print_tabs(level); printf("AsignStmt {\n");
            print_tabs(level + 1); printf("op: '%s',\n", token_string((TokenKind)stmt->assign.op));
            print_tabs(level + 1); printf("lhs: "); print_expr(stmt->assign.lhs, level + 1, false);
            print_tabs(level + 1); printf("rhs: "); print_expr(stmt->assign.rhs, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
            break;

        case stmt_while:
            if (add_spaces) print_tabs(level); printf("WhileStmt {\n");
            print_tabs(level + 1); printf("cond: "); print_expr(stmt->while_stmt.cond, level + 1, false);
            print_func_body(stmt->while_stmt.body, level + 1);
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
            break;

        case stmt_for:
            if (add_spaces) print_tabs(level); printf("ForStmt {\n");
            print_tabs(level + 1); printf("init: "); print_stmt(stmt->for_stmt.init, level + 1, false, false);
            print_tabs(level + 1); printf("cond: "); print_expr(stmt->for_stmt.cond, level + 1, false);
            print_tabs(level + 1); printf("init: "); print_stmt(stmt->for_stmt.post, level + 1, false, false);
            print_func_body(stmt->for_stmt.body, level + 1);
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
            break;

        case stmt_block:
            if (add_spaces) print_tabs(level); printf("BlockStmt {\n");
            print_func_body(stmt->block.body, level + 1);
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
            break;

        case stmt_if:
            if (add_spaces) print_if_stmt(stmt->if_stmt.if_stmt, "IfStmt", level + 1, false);
            for (u32 i = 0; i < darray_length(stmt->if_stmt.else_ifs); i++) {
                b8 is_end =  i == darray_length(stmt->if_stmt.else_ifs) - 1 && !stmt->if_stmt.else_stmt;
                print_if_stmt(stmt->if_stmt.else_ifs[i], "ElseIfStmt", level + 1, is_end);
            }
            print_if_stmt(stmt->if_stmt.if_stmt, "ElseStmt", level + 1, true);
            break;

        case stmt_break:
            if (add_spaces) print_tabs(level); printf("BreakStmt {}"); 
            if (!is_last) printf(","); printf("\n");
            break;

        case stmt_continue:
            if (add_spaces) print_tabs(level); printf("ContinueStmt {}"); 
            if (!is_last) printf(","); printf("\n");
            break;

        case stmt_return:
            if (add_spaces) print_tabs(level); printf("ReturnStmt {\n"); 
            print_tabs(level + 1); printf("expr: "); print_expr(stmt->expr, level + 1, true);
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
            break;

        case stmt_defer:
            if (add_spaces) print_tabs(level); printf("DeferStmt {\n"); 
            print_tabs(level + 1); printf("defered: "); print_stmt(stmt->defer, level + 1, true, false);
            print_tabs(level); printf("}");
            if (!is_last) printf(","); printf("\n");
            break;
    }
}


void print_module(Module* module) {
    for (u32 i = 0; i < darray_length(module->statements); i++) {
        print_stmt(module->statements[i], 0, true, true);
    }
}