#include "checker.h"
#include "float.h"
#include "time.h"



b8          check_stmt(CheckerContext* ctx, Stmt* stmt, Type* required_type);
b8          check_decl(CheckerContext* ctx, Stmt* decl);
Type*       check_type_spec(CheckerContext* ctx, TypeSpec* type);
Operand*    check_expr(CheckerContext* ctx, Expr* expr, Type* expected_type);

void        check_sym(CheckerContext* ctx, Sym* sym);

Type* none_type = null;
Type* cstr_type = null;

#define error_msg_undefined_symbol  ("Cannot find " MAGENTA_COLOR "%.*s" RED_COLOR " in this scope. \n")
#define error_msg_symbol_redefinition ("Refinition of " MAGENTA_COLOR "%.*s" RED_COLOR ". \n")
#define error_msg_int_literal_is_too_big ("Integer literal is too big for " MAGENTA_COLOR "%.*s" RED_COLOR " type. \n")
#define error_msg_float_literal_is_too_big ("Float literal is too big for " MAGENTA_COLOR "%.*s" RED_COLOR " type. \n")




ConstVal* const_val_int_init(u64 val) {
    ConstVal* res = (ConstVal*)global_allocator_alloc(sizeof(ConstVal));
    memset(res, 0, sizeof(ConstVal));

    res->kind = const_val_int;
    res->int_val = val;

    return res;
}

ConstVal* const_val_float_init(f64 val) {
    ConstVal* res = (ConstVal*)global_allocator_alloc(sizeof(ConstVal));
    memset(res, 0, sizeof(ConstVal));

    res->kind = const_val_float;
    res->float_val = val;

    return res;
}

ConstVal* const_val_bool_init(u8 val) {
    ConstVal* res = (ConstVal*)global_allocator_alloc(sizeof(ConstVal));
    memset(res, 0, sizeof(ConstVal));

    res->kind = const_val_bool;
    res->bool_val = val;

    return res;
}

ConstVal* const_val_array_init(ConstVal** values) {
    ConstVal* res = (ConstVal*)global_allocator_alloc(sizeof(ConstVal));
    memset(res, 0, sizeof(ConstVal));

    res->kind = const_val_array;
    res->values = (ConstVal**)copy_darray_buffer(values);

    return res;
}

ConstVal* const_val_compound_init(String* names, ConstVal** values) {
    ConstVal* res = (ConstVal*)global_allocator_alloc(sizeof(ConstVal));
    memset(res, 0, sizeof(ConstVal));

    res->kind = const_val_compound;
    res->names = (String*)copy_darray_buffer(names);
    res->values = (ConstVal**)copy_darray_buffer(values);

    return res;
}

static ConstVal* const_val_default_value(Type* type) {
    if (!type) return null;
    switch (type->kind) {
        case type_none: 
        case type_enum:
        case type_func: 
        case type_rawptr:
        case type_ptr: 
        case type_array: return null;
        case type_struct: 
        case type_union: {
            String*    names  = darray_init(String,    1);
            ConstVal** values = darray_init(ConstVal*, 1);

            ConstVal* val = const_val_compound_init(names, values);

            darray_free(names);
            darray_free(values);

            return val;
        } break;
        case type_integer: return const_val_int_init(0);
        case type_boolean: return const_val_bool_init(false);
        case type_float: return const_val_float_init(0.0);
    }
}


Sym* sym_type_init(String name, Type* type) {
    Sym * sym = (Sym*)malloc(sizeof(Sym));
    memset(sym, 0, sizeof(Sym));
    sym->kind = sym_type;
    sym->name = name;
    sym->type = type;
    sym->state = sym_resolved;
    sym->order_state = sym_unordered;

    return sym;
}

Sym* sym_var_init(String name, Type* type) {
    Sym * sym = (Sym*)malloc(sizeof(Sym));
    memset(sym, 0, sizeof(Sym));
    sym->kind = sym_var;
    sym->name = name;
    sym->type = type;
    sym->state = sym_resolved;
    sym->order_state = sym_unordered;
    return sym;
}

Sym* sym_const_init(String name, Type* type, ConstVal* val) {
    Sym * sym = (Sym*)malloc(sizeof(Sym));
    memset(sym, 0, sizeof(Sym));
    sym->kind = sym_const;
    sym->name = name;
    sym->type = type;
    sym->val = val;
    sym->state = sym_resolved;
    sym->order_state = sym_unordered;
    return sym;
}

Sym* sym_func_init(String name, Type* type) {
    Sym * sym = (Sym*)malloc(sizeof(Sym));
    memset(sym, 0, sizeof(Sym));
    sym->kind = sym_func;
    sym->name = name;
    sym->type = type;
    sym->state = sym_resolved;
    sym->order_state = sym_unordered;
    return sym;
}

void sym_set_state(Sym *sym, SymState state) {
    sym->state = state;
}


Env* env_init(Env* parent) {
    Env* env = (Env*)malloc(sizeof(Env));
    memset(env, 0, sizeof(Env));

    env->parent = parent;
    env->symbols = darray_init(Sym*, 2);
    env->defered = darray_init(Stmt*, 2);

    return env;
}

Sym* env_get_current(Env* env, String name) {
    u32 length = darray_length(env->symbols);
    for (u32 i =0 ; i < length; i++) {
        Sym* curr = env->symbols[i];

        if (string_eq(curr->name, name)) {
            return curr;
        }
    }

    return null;
}


Sym* env_get_current_cstr(Env* env, const char* name) {
    u32 length = darray_length(env->symbols);
    for (u32 i =0 ; i < length; i++) {
        Sym* curr = env->symbols[i];

        if (string_eq_cstr(curr->name, name)) {
            return curr;
        }
    }

    return null;
}

Sym* env_get(Env* env, String name) {
    Env* it = env;
    while (it) {
        Sym* res = env_get_current(it, name);
        if (res)  {
            return res;
        }

        it = it->parent;
    }

    return null;
}

Sym* env_get_cstr(Env* env, const char* name) {
    Env* it = env;
    while (it) {
        Sym* res = env_get_current_cstr(it, name);
        if (res)  {
            return res;
        }

        it = it->parent;
    }

    return null;
}

b8 env_push(Env* env, Sym* sym) {
    if (env_get_current(env, sym->name)) {
        return false;
    }

    darray_push(env->symbols, sym);
    return true;
}

void env_defer(Env* env, Stmt* stmt) {
    darray_push(env->defered, stmt);
}

static void setup_global_symbols(Env* env) {
    if (!none_type) {
        none_type = type_none_init();
    }

    env_push(env, sym_type_init(string_c("i8"),  type_integer_init(string_c("i8"),  1, true)));
    env_push(env, sym_type_init(string_c("i16"), type_integer_init(string_c("i16"), 2, true)));
    env_push(env, sym_type_init(string_c("i32"), type_integer_init(string_c("i32"), 4, true)));
    env_push(env, sym_type_init(string_c("i64"), type_integer_init(string_c("i64"), 8, true)));
    env_push(env, sym_type_init(string_c("u8"),  type_integer_init(string_c("u8"),  1, false)));
    env_push(env, sym_type_init(string_c("u16"), type_integer_init(string_c("u16"), 2, false)));
    env_push(env, sym_type_init(string_c("u32"), type_integer_init(string_c("u32"), 4, false)));
    env_push(env, sym_type_init(string_c("u64"), type_integer_init(string_c("u64"), 8, false)));
    env_push(env, sym_type_init(string_c("f32"), type_float_init(string_c("f32"), 4)));
    env_push(env, sym_type_init(string_c("f64"), type_float_init(string_c("f64"), 8)));

    env_push(env, sym_type_init(string_c("bool"), type_boolean_init()));
    env_push(env, sym_type_init(string_c("rawptr"), type_rawptr_init()));


    env_push(env, sym_const_init(string_c("true"), env_get_cstr(env, "bool")->type, const_val_bool_init(true)));
    env_push(env, sym_const_init(string_c("false"), env_get_cstr(env, "bool")->type, const_val_bool_init(false)));
}

void scope_push_env(CheckerContext* ctx, Env* scope) {
    Env* env = env_init(scope);
    ctx->curr_scope = env;
    darray_push(ctx->scopes, env);
}

void scope_pop_to(CheckerContext* ctx, Env* scope) {
    ctx->curr_scope = scope;
}

void scope_push(CheckerContext* ctx) {
    Env* env = env_init(ctx->curr_scope);
    ctx->curr_scope = env;
    darray_push(ctx->scopes, env);
}


void scope_pop(CheckerContext* ctx) {
    ctx->curr_scope = ctx->curr_scope->parent;
}


ImportedSym* get_import(CheckerContext* ctx, String path) {
    u32 length = darray_length(ctx->imports);
    for (u32 i = 0; i < length; i++) {
        if (string_eq(ctx->imports[i]->name, path)) {
            return ctx->imports[i];
        }
    }

    return null;
}

ImportedSym* imported_sym_init(String name) {
    ImportedSym* sym = (ImportedSym*)malloc(sizeof(ImportedSym));
    sym->name = name;
    sym->state = sym_importing;
    
    return sym;
}


CheckerContext* checker_context_init() {
    CheckerContext* ctx = (CheckerContext*)malloc(sizeof(CheckerContext));
    ctx->scopes = darray_init(Env*, 2);
    ctx->defered_types = darray_init(DeferedType, 2);
    ctx->types = darray_init(Type*, 2);
    ctx->imports = darray_init(ImportedSym*, 2);
    ctx->string_table = darray_init(u8, 2);
    ctx->curr_working_dirs_stack = darray_init(String, 2);
    ctx->counter = 0;

    ctx->curr_stmt = null;
    ctx->curr_scope = null;
    ctx->is_checking_cond = false;

    scope_push(ctx);
    setup_global_symbols(ctx->curr_scope);

    // add the global types to the types list
    for (u32 i = 0; i < darray_length(ctx->curr_scope->symbols); i++) {
        Sym* curr = ctx->curr_scope->symbols[i];
        if (curr->kind == sym_type) {
            darray_push(ctx->types, curr->type);
        }
    }

    return ctx;
}


void push_curr_working_dir(CheckerContext* ctx, String str) {
    darray_push(ctx->curr_working_dirs_stack, str);
} 

String get_curr_workign_dir(CheckerContext* ctx) {
    return ctx->curr_working_dirs_stack[darray_length(ctx->curr_working_dirs_stack) - 1];
}

void pop_curr_working_dir(CheckerContext* ctx) {
    darray_pop(ctx->curr_working_dirs_stack);
}

String create_curr_working_dir(String file_path) {
    while (file_path.length--) {
        if (file_path.buffer[file_path.length] == '/') {
            break;
        }
    }
    file_path.length += 1;

    return file_path;
}


Operand* operand_init(Type* type) {
    Operand* op = (Operand*)global_allocator_alloc(sizeof(Operand));
    memset(op, 0, sizeof(Operand));

    op->type = type;
    op->flags = 0;
    return op;
}

void operand_set_type(Operand* op, Type* type) {
    op->type = type;
}

void operand_set_value(Operand* op, ConstVal* val) {
    op->val = val; 
}

void operand_set_lvalue(Operand* op) {
    op->flags |= operand_lvalue_flag;
}

void operand_set_rvalue(Operand* op) {
    op->flags |= operand_rvalue_flag;
}

void operand_set_type_flag(Operand* op) {
    op->flags |= operand_type_flag;
}

void operand_set_const(Operand* op) {
    op->flags |= operand_const_flag;
}

b8 operand_has_lvalue_flag(Operand* op) {
    return (op->flags & operand_lvalue_flag) == operand_lvalue_flag;
}

b8 operand_has_rvalue_flag(Operand* op) {
    return (op->flags & operand_rvalue_flag) == operand_rvalue_flag;
}

b8 operand_has_type_flag(Operand* op) {
    return (op->flags & operand_type_flag) == operand_type_flag;
}

b8 operand_has_const_flag(Operand* op) {
    return (op->flags & operand_const_flag) == operand_const_flag;
}

void operand_free(Operand* op) {
    free(op);
}

u64 get_integer_max_value(Type* type) {
    u64 offset = 1;
    if (type->integer.is_signed) {
        offset = 2;
    }

    u64 size = ((u64)2 << (type->integer.size * 8 - offset)) - 1;

    return size;
}

f64 get_float_max_value(Type* type) {
    if (type->size == 4) {
        return FLT_MAX;
    } else {
        return DBL_MAX;
    }
}

ConstVal* evaluate_member_expr(ConstVal* val, String member_name, Type* type) {
    if (!val) return null;

    for (u32 i = 0; i < darray_length(val->names); i++) {
        if (string_eq(val->names[i], member_name)) {
            return val->values[i];
        }
    }

    return const_val_default_value(type);
}

ConstVal* evaluate_cast_expr(ConstVal* val, Type* type) {
    if (!val) return null;

    switch (type->kind) {
        case type_float: {
            switch (val->kind) {
                case const_val_int: return const_val_float_init((f64)val->int_val);
                case const_val_float: return val;
                case const_val_bool: return const_val_float_init((f64)val->bool_val);
                default: return null;
            }
        } break;
        case type_integer: {
            switch (val->kind) {
                case const_val_int: return val; 
                case const_val_float: return const_val_int_init((f64)val->float_val);
                case const_val_bool: return const_val_int_init((f64)val->bool_val);
                default: return null;
            }
        } break;
        case type_boolean: {
            switch (val->kind) {
                case const_val_int: return const_val_bool_init((b8)val->int_val);
                case const_val_float: return const_val_bool_init((b8)val->float_val);
                case const_val_bool: return val;
                default: return null;
            }
        } break;
        default: return null;
    }
}

ConstVal* evaluate_unary_expr(ConstVal* val, u8 op) {
    if (!val) return null;

    switch (op) {
        case '-': {
            switch (val->kind) {
                case const_val_int: return const_val_int_init((-val->int_val));
                case const_val_float: return const_val_float_init(-val->float_val);
                default: return null;
            }
        } break;
        case '!': {
            switch (val->kind) {
                case const_val_int: return const_val_bool_init(!val->int_val);
                case const_val_float: return const_val_bool_init(!val->float_val);
                case const_val_bool: return const_val_bool_init(!val->bool_val);
                default: return null;
            }
        } break;
        case '~': {
            switch (val->kind) {
                case const_val_int: return const_val_int_init(~val->int_val);
                default: return null;
            }
        } break;
        default: return null;
    }
}
ConstVal* evaluate_binary_expr(ConstVal* lhs, ConstVal* rhs, u8 op) {
    if(!lhs || !rhs) return null;

    ConstVal* val;
    switch (op) {
        case '+': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_int_init(lhs->int_val + rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_float_init(lhs->float_val+ rhs->float_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case '-': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_int_init(lhs->int_val - rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_float_init(lhs->float_val - rhs->float_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case '*': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_int_init(lhs->int_val * rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_float_init(lhs->float_val * rhs->float_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case '/': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_int_init(lhs->int_val / rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_float_init(lhs->float_val / rhs->float_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case '%': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_int_init(lhs->int_val % rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_float_init(fmod(lhs->float_val, rhs->float_val));
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case '&': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_int_init(lhs->int_val & rhs->int_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case '|': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_int_init(lhs->int_val | rhs->int_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case '^': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_int_init(lhs->int_val ^ rhs->int_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case token_shl: {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val << rhs->int_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case token_shr: {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val >> rhs->int_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case '<': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val < rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_bool_init(lhs->float_val < rhs->float_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case '>': {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val > rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_bool_init(lhs->float_val > rhs->float_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case token_lt_eq: {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val <= rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_bool_init(lhs->float_val <= rhs->float_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case token_gt_eq: {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val >= rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_bool_init(lhs->float_val >= rhs->float_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case token_logic_and: {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val && rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_bool_init(lhs->float_val && rhs->float_val);
            } break;
            case const_val_bool: {
                val = const_val_bool_init(lhs->bool_val && rhs->bool_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case token_logic_or: {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val || rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_bool_init(lhs->float_val || rhs->float_val);
            } break;
            case const_val_bool: {
                val = const_val_bool_init(lhs->bool_val || rhs->bool_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case token_eq_eq: {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val == rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_bool_init(lhs->float_val == rhs->float_val);
            } break;
            case const_val_bool: {
                val = const_val_bool_init(lhs->bool_val == rhs->bool_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        case token_bang_eq: {
            switch (lhs->kind) {
            case const_val_int: {
                val = const_val_bool_init(lhs->int_val != rhs->int_val);
            } break;
            case const_val_float: {
                val = const_val_bool_init(lhs->float_val != rhs->float_val);
            } break;
            case const_val_bool: {
                val = const_val_bool_init(lhs->bool_val != rhs->bool_val);
            } break;
            default: {
                return null;
            } break;
            }
        } break;
        default: {
            return null;
        } break;
    }

    return val;
}

Expr* convert_to_any_type(CheckerContext* ctx, Expr* expr) {
    StructInitItem* members = darray_init(StructInitItem, 2);

    // create the data member
    StructInitItem data = {
        .location = expr->location, 
        .name = string_c("data"),
        .expr = expr_unary_init(expr->location, (TokenKind)'&', expr),
    };
    data.expr->type = type_ptr_init(expr->type);
    darray_push(ctx->types, data.expr->type);

    // create the type identifier expression
    StructInitItem type_id = {
        .location = expr->location, 
        .name = string_c("id"),
        .expr = expr_typeid_expr_init(expr->location, expr),
    };
    type_id.expr->type = expr->type;

    // push the members in the struct initializer 
    darray_push(members, type_id);
    darray_push(members, data);

    Expr* result = expr_compound_init(expr->location, string_c("any"), members);
    result->type = env_get(ctx->scopes[0], string_c("any"))->type;

    return result;
}

Stmt* generate_temp_variable(CheckerContext* ctx, Expr* expr) {
    Stmt* stmt = stmt_var_decl_init(expr->location, 
        string_fmt("__internal_temp_variable_%llu", ctx->counter++), type_spec_none_init(), expr);

    stmt->decl.type = expr->type;

    // push the generated the statement into the correct scope
    if (!ctx->curr_stmt->generated_stmts) {
        ctx->curr_stmt->generated_stmts = darray_init(Stmt*, 2);
    }

    darray_push(ctx->curr_stmt->generated_stmts, stmt);

    return stmt;
}

void convert_str_literal_to_cstr(CheckerContext* ctx, Expr* expr, u64 offset, u64 len) {
    Loc location = expr->location;
    Expr** params = darray_init(Expr*, 2);
    Type* u64_type = env_get_current_cstr(ctx->scopes[0], "u64")->type;
    Type* cstr_type = env_get_current_cstr(ctx->scopes[0], "cstr")->type;

    // create the a pointer to the actual string in the string table
    Expr* data = expr_binary_init(location, '+', 
        expr_name_init(location, string_c("__internal_string_table")), 
        expr_int_init(location, offset));


    // create the length expression
    Expr* length = expr_int_init(location, len);
    length->type = u64_type;


    // push the expression into the call arguments
    darray_push(params, data);
    darray_push(params, length);


    // modify the actual literal string to be a call expression to
    // cstr_from_buffer function instead.
    expr->kind = expr_call;
    expr->call.callee  = expr_name_init(location, string_c("cstr_from_buffer"));
    expr->call.params = params;
    expr->type = cstr_type;

}

Expr* convert_cstr_to_string(CheckerContext* ctx, Expr* expr) {
    Loc location = expr->location;
    Expr** params = darray_init(Expr*, 2);

    Type* string_type = env_get_current_cstr(ctx->scopes[0], "string")->type;

    darray_push(params, expr);
    Expr* call = expr_call_init(location, 
        expr_name_init(location, string_c("string_from_cstr")), params);
    call->type = string_type;

    return call;
}

static void convert_string_indexing(CheckerContext* ctx, Expr* expr, Type* type) {
    Expr* old = expr->index.expr;
    expr->index.expr = expr_member_init(old->location, old, string_c("data"));
    expr->index.expr->member.expr->type = type;

    expr->type = env_get_cstr(ctx->scopes[0], "u8")->type;
}

static void convert_expr_to_operand(CheckerContext* ctx, Expr* expr, ConstVal* val, Type* type) {
    if (!val || !expr || !type) return;

    switch (val->kind) {
        case const_val_int: {
            expr->kind = expr_int;
            expr->integer = val->int_val;
            expr->type = type;
        } break;
        case const_val_float: {
            expr->kind = expr_float;
            expr->float_lit = val->float_val;
            expr->type = type;
        } break;
        case const_val_bool: {
            expr->kind = expr_name;
            if (val->bool_val) {
                expr->name = string_c("true");
            } else {
                expr->name = string_c("false");
            }
            expr->type = type;
        } break;
        default: break;
    }
}


#define check_result(operand) if (!(operand)) { return 0; }


Operand* check_name_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Sym* sym = env_get(ctx->curr_scope, expr->name);
    if (!sym) {
        error(expr->location, error_msg_undefined_symbol, 
            (i32)expr->name.length, expr->name.buffer);
    }

    if (sym->kind == sym_type) {
        error(expr->location, "Unexpected expression. \n");
    }

    check_sym(ctx, sym);

    Operand* operand = operand_init(sym->type);
    operand_set_lvalue(operand);
    if (sym->kind == sym_const) {
        operand_set_const(operand);
        operand_set_value(operand, sym->val);
    }

    expr->type = sym->type;
    return operand;
}


Operand* check_int_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Type* type = env_get_cstr(ctx->scopes[0], "i32")->type;

    if (expected_type) {
        if (is_integer_type(expected_type)) {
            type = expected_type;
        } 
    } 

    if (expr->integer > get_integer_max_value(type)) {
        error(expr->location, error_msg_int_literal_is_too_big, 
            (i32)type->name.length, type->name.buffer);
    }

    Operand* operand = operand_init(type);
    operand_set_rvalue(operand);
    operand_set_const(operand);
    operand_set_value(operand, const_val_int_init(expr->integer));

    
    expr->type = operand->type;
    return operand; 
}


Operand* check_float_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Type* type = env_get_cstr(ctx->scopes[0], "f32")->type;
    if (expected_type) {
        if (is_float_type(expected_type)) {
            type = expected_type;
        }
    }

    if (expr->float_lit > get_float_max_value(type)) {
        error(expr->location, error_msg_float_literal_is_too_big, 
            (i32)type->name.length, type->name.buffer);
    }

    Operand* operand = operand_init(type);
    operand_set_rvalue(operand);
    operand_set_const(operand);
    operand_set_value(operand, const_val_float_init(expr->float_lit));

    expr->type = operand->type;
    return operand; 
}


Operand* check_cast_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Type* type = check_type_spec(ctx, expr->cast.type);
    check_result(type);

    Operand* lhs = check_expr(ctx, expr->cast.expr, type);
    check_result(lhs);

    if (type_eq(lhs->type, type)) {
        Operand* operand = operand_init(type);
        operand_set_rvalue(operand);

        if (operand_has_const_flag(lhs)) {
            operand_set_const(operand);
            operand_set_value(operand, lhs->val);
        }

        expr->type = type;
        return operand;
    }

    if (!type_castable(lhs->type, type)) {
        if (is_string_type(type) && is_cstr_type(lhs->type)) {
            expr->cast.expr = convert_cstr_to_string(ctx, expr->cast.expr);
            goto check_cast_finish;
        }
        String lhs_type_name = type_get_name(lhs->type);
        String cast_type_name = type_get_name(type);

        error(expr->location, "Cannot cast " MAGENTA_COLOR"%.*s" RED_COLOR " to " MAGENTA_COLOR "%.*s" RED_COLOR ". \n", 
            (i32)lhs_type_name.length, lhs_type_name.buffer, 
            (i32)cast_type_name.length, cast_type_name.buffer);
    }

    if (is_any_type(type) && !is_any_type(lhs->type)) {
        if (operand_has_rvalue_flag(lhs)) {
            Stmt* generated = generate_temp_variable(ctx, expr->cast.expr);

            expr->cast.expr = expr_name_init(generated->location, generated->decl.name);
            expr->cast.expr->type = generated->decl.type;
        }

        expr->cast.expr = convert_to_any_type(ctx, expr->cast.expr);
    }

check_cast_finish:
    Operand* operand = operand_init(type);
    operand_set_rvalue(operand);
    if (operand_has_const_flag(lhs)) {
        operand_set_const(operand);

        operand_set_value(operand, evaluate_cast_expr(lhs->val, type));
        convert_expr_to_operand(ctx, expr, lhs->val, type);
    }

    expr->type = type;
    return operand;
}

Operand* check_unary_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Operand* operand = operand_init(null);
    Operand* res = null;

    u8 op = expr->unary.op;
    if (op == '!') {
        Operand* rhs = check_expr(ctx, expr->unary.expr, expected_type);
        check_result(rhs);

        if (!is_scalar_type(rhs->type)) {
            error(expr->location, "Expected a scalar type expression. \n");
        }

        operand_set_type(operand, env_get_current_cstr(ctx->scopes[0], "bool")->type);
        operand_set_rvalue(operand);
        res = rhs;
    }

    if (op == '*') {
        Operand* rhs = check_expr(ctx, expr->unary.expr, null);
        check_result(rhs);

        if (!is_ptr_type(rhs->type)) {
            error(expr->location, "Cannot dereference a non Pointer type. \n");
        }

        operand_set_type(operand, rhs->type->ptr.base);
        operand_set_lvalue(operand);
        res = rhs;
    }

    if (op == '&') {
        Operand* rhs = check_expr(ctx, expr->unary.expr, null);
        check_result(rhs);

        if (operand_has_rvalue_flag(rhs)) {
            Stmt* generated = generate_temp_variable(ctx, expr->unary.expr); 

            expr->unary.expr = expr_name_init(expr->unary.expr->location, generated->decl.name);
            expr->unary.expr->type = generated->decl.type;
        }
        Type* ptr_type = type_ptr_init(rhs->type);

        darray_push(ctx->types, ptr_type);
        operand_set_type(operand, ptr_type);
        operand_set_rvalue(operand);
        res = rhs;
    }

    if ((op == '~' || op == '-')) {
        Operand* rhs = check_expr(ctx, expr->unary.expr, expected_type);
        check_result(rhs);

        if (!is_scalar_type(rhs->type)) {
            error(expr->location, "Invalid unary expression. \n");
        }

        if ((is_boolean_type(rhs->type) || is_float_type(rhs->type)) && op == '~') {
            error(expr->location, "Invalid unary expression. \n");
        }


        operand_set_type(operand, rhs->type);
        operand_set_rvalue(operand);
        res = rhs;
    }

    if (res && operand_has_const_flag(res)) {
        operand_set_const(operand);

        operand_set_value(operand, evaluate_unary_expr(res->val, expr->unary.op));
        convert_expr_to_operand(ctx, expr, operand->val, operand->type);
    }

    expr->type = operand->type;
    return operand;

}

b8 validate_binary_expr(u8 op, Type* type) {
    switch (op) {
        case '%': return is_integer_type(type);

        case '+':
        case '-':
        case '*':
        case '/': 
        case '>': 
        case '<': 
        case token_lt_eq: 
        case token_gt_eq: return is_integer_type(type) || is_float_type(type) || is_ptr_type(type);

        case '|':
        case '&':
        case '^': 
        case token_shl:
        case token_shr: return is_integer_type(type);
        default: {
            if (is_logic_op(op)) {
                return true;
            } else {
                return false;
            }
        } break;
    }
}

Operand* check_binary_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Operand* lhs = null;
    Operand* rhs = null;
    Type* prefered_type = expected_type;

    if (is_logic_op(expr->binary.op)) {
        prefered_type = null;
    } 

    if (expr->binary.lhs->kind == expr_int) {
        rhs = check_expr(ctx, expr->binary.rhs, prefered_type);
        check_result(rhs);

        lhs = check_expr(ctx, expr->binary.lhs, rhs->type);
        check_result(lhs);
    } else {
        lhs = check_expr(ctx, expr->binary.lhs, prefered_type);
        check_result(lhs);

        rhs = check_expr(ctx, expr->binary.rhs, lhs->type);
        check_result(rhs);
    }


    if (!type_eq(lhs->type, rhs->type)) {
        error(expr->location, "The rhs doesn't match the lhs of the binary expression. \n");
    }

    if (!is_scalar_type(lhs->type) || is_rawptr_type(lhs->type)) {
        if (!is_enum_type(lhs->type)) {
            error(expr->location, "Invalid binary expressions. \n");
        }
    }

    if (!validate_binary_expr(expr->binary.op, lhs->type)) {
        error(expr->location, "Invalid binary expressions. \n");
    }


    Operand* operand = operand_init(lhs->type);
    operand_set_rvalue(operand);

    if (is_logic_op(expr->binary.op)) {
        operand_set_type(operand, env_get_current_cstr(ctx->scopes[0], "bool")->type);
    }

    if (operand_has_const_flag(lhs) && operand_has_const_flag(rhs)) {
        operand_set_const(operand);

        operand_set_value(operand, evaluate_binary_expr(lhs->val, rhs->val, expr->binary.op));
        convert_expr_to_operand(ctx, expr, operand->val, operand->type);
    }

    expr->type = operand->type;
    return operand;
}


Operand* check_ternary_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Operand* cond = check_expr(ctx, expr->ternary.cond, expected_type);
    check_result(cond);

    if (cond->type->kind != type_boolean) {
        error(expr->ternary.cond->location, "Invalid condition expression. \n");
    }

    Operand* lhs = check_expr(ctx, expr->ternary.then_expr, expected_type);
    if (!lhs) {
        return null;
    }
    Operand* rhs = check_expr(ctx, expr->ternary.else_expr, lhs->type);
    if (!rhs) {
        return null;
    }

    if (!type_eq(lhs->type, rhs->type)) {
        error(expr->location, "The if and else types of the ternary expression must match. \n");
    }

    Operand* operand = operand_init(lhs->type);
    operand_set_rvalue(operand);

    if (operand_has_const_flag(cond) && operand_has_const_flag(lhs) && operand_has_const_flag(rhs)) {
        operand_set_const(operand);

        operand_set_value(operand, null);
    }


    expr->type = operand->type;
    return operand;
}


Operand* check_member_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    // check if the lhs of the mmember expression is an enum type name
    if (expr->member.expr->kind == expr_name) {
        Sym* sym = env_get(ctx->curr_scope, expr->member.expr->name);

        if (sym && sym->kind == sym_type && is_enum_type(sym->type)) {
            for (u32 i = 0; i < sym->type->enum_type.num_items; i++) {
                String curr = sym->type->enum_type.items[i];

                if (string_eq(curr, expr->member.name)) {
                    Operand* operand = operand_init(sym->type);
                    operand_set_rvalue(operand);

                    expr->type = sym->type;
                    expr->member.expr->type = sym->type;
                    return operand;
                }
            }

            error(expr->location, "There is no member called " MAGENTA_COLOR "%.*s" RED_COLOR " in the " MAGENTA_COLOR "%.*s" RED_COLOR " struct. \n",
                (i32)expr->member.name.length, expr->member.name.buffer,
                (i32)sym->type->name.length, sym->type->name.buffer);
        }
    } 

    // else we check it as a normal expression
    Operand* lhs = check_expr(ctx, expr->member.expr, null);
    check_result(lhs);


    if (is_compound_type(lhs->type) || (is_ptr_type(lhs->type) && is_compound_type(lhs->type->ptr.base))) {
        Type* type = null;
        if (is_compound_type(lhs->type)) {
            type = lhs->type;
        } else {
            type = lhs->type->ptr.base;
        }

        for (u32 i = 0; i < type->compound.num_members; i++) {
            TypeMember* curr = &type->compound.members[i];
            if (string_eq(curr->name, expr->member.name)) {
                Operand* operand = operand_init(curr->type);
                operand_set_lvalue(operand);

                if (operand_has_const_flag(lhs)) {
                    operand_set_const(operand);

                    operand_set_value(operand, evaluate_member_expr(lhs->val, curr->name, curr->type));
                    convert_expr_to_operand(ctx, expr, operand->val, curr->type);
                    return operand;

                } else {
                    expr->type = curr->type;
                    expr->member.expr->type = lhs->type;
                    return operand;
                }

            }
        }

        error(expr->location, "There is no member called " MAGENTA_COLOR "%.*s" RED_COLOR " in the " MAGENTA_COLOR "%.*s" RED_COLOR " struct. \n",
            (i32)expr->member.name.length, expr->member.name.buffer,
            (i32)type->name.length, type->name.buffer);

    } else {
        error(expr->location, MAGENTA_COLOR "%.*s" RED_COLOR " type doesn't have members/fields. \n",
            (u32)lhs->type->name.length, lhs->type->name.buffer);
    }

    return null;
}


Operand* check_index_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    b8 is_string = false;
    b8 is_cstr   = false;
    Operand* lhs = check_expr(ctx, expr->index.expr, null);
    check_result(lhs);

    if (!is_array_type(lhs->type) && !is_ptr_type(lhs->type)) {
        if (is_string_type(lhs->type)) {
            is_string = true;
        } else if (is_cstr_type(lhs->type)) {
            is_cstr = true;
        } else {
            error(expr->location, "Cannot index a non array type.\n");
        }
    }

    Operand* index = check_expr(ctx, expr->index.index, null);
    check_result(index);

    if (!is_integer_type(index->type)) {
        error(expr->location, "Expected an integer but found " MAGENTA_COLOR "%.*s" RED_COLOR ".",
            (i32)index->type->name.length, index->type->name.buffer);
    }


    Operand* operand = operand_init(null);
    if (is_string || is_cstr) {
        convert_string_indexing(ctx, expr, lhs->type);
        operand_set_type(operand, expr->type);
    } else {
        if (is_ptr_type(lhs->type)) {
            operand_set_type(operand, lhs->type->ptr.base);
        } else {
            operand_set_type(operand, lhs->type->array.base);
        }
    }

    operand_set_lvalue(operand);
    if (operand_has_const_flag(lhs)) {

        operand_set_const(operand);
        operand_set_value(operand, null);
    }


    expr->type = operand->type;
    return operand;
}

Operand* check_call_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Operand* lhs = check_expr(ctx, expr->call.callee, expected_type);
    check_result(lhs);

    if (lhs->type->kind != type_func) {
        error(expr->location, "Cannot call a non function type.\n");
    }



    u32 expected_count = lhs->type->func.num_args;
    u32 passed_count = darray_length(expr->call.params);
    if (expected_count != passed_count) {
        error(expr->location, "The function takes %u %s but %u %s passed. \n", 
            expected_count, expected_count == 1 ? "argument": "arguments",
            passed_count, passed_count == 1 ? "was" : "were");
    }

    for (u32 i = 0; i < lhs->type->func.num_args; i++) {
        Type* curr = lhs->type->func.args[i];
        Operand* arg = check_expr(ctx, expr->call.params[i], curr);
        if (!arg) {
            return null;
        }
        Type* expected_type = curr;
        Type* found_type = arg->type;

        if (!type_eq(expected_type, found_type)) {

            if (is_rawptr_type(expected_type) && is_ptr_type(found_type)) {
                continue;
            } else if (is_any_type(expected_type) && !is_any_type(found_type)) {
                if (operand_has_rvalue_flag(arg)) {

                    Stmt* generated = generate_temp_variable(ctx, expr->call.params[i]);

                    expr->call.params[i] = expr_name_init(generated->location, generated->decl.name);
                    expr->call.params[i]->type = generated->decl.type;
                }

                expr->call.params[i] = convert_to_any_type(ctx, expr->call.params[i]);
                continue;
            }

            String expected_type_name = type_get_name(expected_type);
            String found_type_name = type_get_name(found_type);

            error(expr->call.params[i]->location, 
                "Expected " MAGENTA_COLOR "%.*s" RED_COLOR " type but got " MAGENTA_COLOR "%.*s" RED_COLOR ". \n", 
                (i32)expected_type_name.length, expected_type_name.buffer, 
                (i32)found_type_name.length, found_type_name.buffer);
        }

        if (is_array_type(arg->type) && operand_has_rvalue_flag(arg)) {
            Stmt* generated = generate_temp_variable(ctx, expr->call.params[i]);

            expr->call.params[i] = expr_name_init(generated->location, generated->decl.name);
            expr->call.params[i]->type = generated->decl.type;
        }

    }


    Operand* operand = operand_init(lhs->type->func.ret_type);
    operand_set_rvalue(operand);

    expr->type = operand->type;
    return operand;
}


Operand* check_compound_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Sym* sym = env_get(ctx->curr_scope, expr->compound.name);
    if (!sym) {
        error(expr->location, "Cannot find the " MAGENTA_COLOR "%.*s" RED_COLOR " type in this scope. \n",
            (i32)expr->compound.name.length, expr->compound.name.buffer);
    }

    if (!is_struct_type(sym->type)) {
        error(expr->location, "Cannot use struct initializer with non struct types. \n");
    }

    b8 is_const = true;

    ConstVal** values = darray_init(ConstVal*, 2);
    String*    names  = darray_init(String,    2);


    for (u32 i = 0; i < darray_length(expr->compound.items); i++) {
        StructInitItem* curr = &expr->compound.items[i];
        Type* type = type_struct_get_member_type(sym->type, curr->name);
        if (!type) {
            error(curr->location, "There is no member called " MAGENTA_COLOR "%.*s" RED_COLOR " in the " MAGENTA_COLOR "%.*s" RED_COLOR " struct. \n",
                (i32)curr->name.length, curr->name.buffer, (i32)sym->name.length, sym->name.buffer);
        }

        Operand* value = check_expr(ctx, curr->expr, type);
        check_result(value);

        if (!operand_has_const_flag(value)) {
            is_const = false;
        }


        darray_push(values, value->val);
        darray_push(names, curr->name);


        if (!type_eq(type, value->type)) {
            if (is_rawptr_type(type) && is_ptr_type(value->type)) {
                continue;
            } else if (is_any_type(type) && !is_any_type(value->type)) {
                if (operand_has_rvalue_flag(value)) {

                    Stmt* generated = generate_temp_variable(ctx, curr->expr);

                    curr->expr = expr_name_init(generated->location, generated->decl.name);
                    curr->expr->type = generated->decl.type;
                }

                curr->expr = convert_to_any_type(ctx, curr->expr);
                continue;
            }

            String expected_type = type_get_name(type);
            String found_type = type_get_name(value->type);

            error(curr->expr->location, "Expected " MAGENTA_COLOR "%.*s" RED_COLOR " type but got " MAGENTA_COLOR "%.*s" RED_COLOR ". \n", 
                (i32)expected_type.length, expected_type.buffer, 
                (i32)found_type.length, found_type.buffer);
        }
    }

    Operand* operand = operand_init(sym->type);
    operand_set_rvalue(operand);
    if (is_const) {
        operand_set_const(operand);

        operand_set_value(operand, const_val_compound_init(names, values));
    }

    darray_free(names);
    darray_free(values);

    expr->type = sym->type;
    return operand;
}

Operand* check_init_list_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    u64 length = darray_length(expr->init_list.items);

    b8 expected_type_is_array = false;
    b8 is_const = true;
    Type* expected = null;
    u64 start = 0;

    if (!expected_type) {
        if (length  == 0) {
            error(expr->location, "Cannot infer the type of the array. \n");
        }

        Operand* first = check_expr(ctx, expr->init_list.items[0], null);
        check_result(first);

        start = 1;
        expected = first->type;
    } else {
        if (expected_type->kind != type_array) {
            error(expr->location, "Expected " MAGENTA_COLOR "%.*s" RED_COLOR " but found an " MAGENTA_COLOR "array" RED_COLOR ". \n",
                (i32)expected_type->name.length, expected_type->name.buffer);
        }

        expected_type_is_array = true;
        expected = expected_type->array.base;
    }


    for (u64 i = start; i < length; i++) {
        Expr* curr = expr->init_list.items[i];
        Operand* res = check_expr(ctx, curr, expected);
        check_result(res);

        if (!operand_has_const_flag(res)) {
            is_const = false;
        }

        if (!type_eq(expected, res->type)) {
            if (is_rawptr_type(expected) && is_ptr_type(res->type)) {
                continue;
            } else if (is_any_type(expected) && !is_any_type(res->type)) {
                if (operand_has_rvalue_flag(res)) {

                    Stmt* generated = generate_temp_variable(ctx, expr->init_list.items[i]);

                    expr->init_list.items[i] = expr_name_init(generated->location, generated->decl.name);
                    expr->init_list.items[i]->type = generated->decl.type;
                }

                expr->init_list.items[i] = convert_to_any_type(ctx, expr->init_list.items[i]);
                continue;
            }


            String expected_type = type_get_name(expected);
            String found_type = type_get_name(res->type);

            error(curr->location, "Expected " MAGENTA_COLOR "%.*s" RED_COLOR " but got " MAGENTA_COLOR "%.*s" RED_COLOR ". \n", 
                (i32)expected_type.length, expected_type.buffer, 
                (i32)found_type.length, found_type.buffer);
        }

    }


    Type* array_type = null;
    if (!expected_type_is_array) {
        array_type = type_array_init(expected, length);
        darray_push(ctx->types, array_type);
    } else {
        array_type = expected_type;
    }

    Operand* operand = operand_init(array_type);
    operand_set_rvalue(operand);
    if (is_const) {
        operand_set_const(operand);

        // todo(redone): generate the correct value using the compound expression members
        operand_set_value(operand, null);
    }

    expr->type = array_type;
    return operand;
}

Operand* check_typeid_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Operand* res = check_expr(ctx, expr->type_id.expr, null);
    check_result(res);

    Operand* operand = operand_init(env_get_cstr(ctx->scopes[0], "u64")->type);
    operand_set_rvalue(operand);
    operand_set_const(operand);
    operand_set_value(operand, null);

    expr->type = res->type;
    return operand;
}

Operand* check_typeid_type(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Type* res = check_type_spec(ctx, expr->type_id.type);
    check_result(res);

    Operand* operand = operand_init(env_get_cstr(ctx->scopes[0], "u64")->type);
    operand_set_rvalue(operand);
    operand_set_const(operand);
    operand_set_value(operand, null);

    expr->type = res;
    return operand;
}

Operand* check_sizeof_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Operand* res = check_expr(ctx, expr->type_id.expr, null);
    check_result(res);

    Operand* operand = operand_init(env_get_cstr(ctx->scopes[0], "u32")->type);
    operand_set_rvalue(operand);
    operand_set_const(operand);
    operand_set_value(operand, null);

    expr->type = res->type;
    return operand;
}

Operand* check_sizeof_type(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    Type* res = check_type_spec(ctx, expr->type_id.type);
    check_result(res);

    Operand* operand = operand_init(env_get_cstr(ctx->scopes[0], "u32")->type);
    operand_set_rvalue(operand);
    operand_set_const(operand);
    operand_set_value(operand, null);

    expr->type = res;
    return operand;
}

Operand* check_str_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    u64 offset = darray_length(ctx->string_table);

    // copy the literal string into the string table
    for (u64 i = 0; i < expr->name.length; i++) {
        darray_push(ctx->string_table, expr->name.buffer[i]);
    }
    darray_push(ctx->string_table, 0);

    convert_str_literal_to_cstr(ctx, expr, offset, expr->name.length);

    Operand* operand = operand_init(expr->type);
    operand_set_rvalue(operand);
    operand_set_const(operand);
    return operand;
}

Operand* check_expr(CheckerContext* ctx, Expr* expr, Type* expected_type) {
    switch (expr->kind) {
        case expr_none: {
            Operand* operand = operand_init(none_type);
            operand_set_rvalue(operand);
            return operand;
        } break;
        case expr_name: {
            return check_name_expr(ctx, expr, expected_type);
        } break;
        case expr_int: {
            return check_int_expr(ctx, expr, expected_type);
        } break;
        case expr_float: {
            return check_float_expr(ctx, expr, expected_type);
        } break;
        case expr_str: {
            return check_str_expr(ctx, expr, expected_type);
        } break;
        case expr_cast: {
            return check_cast_expr(ctx, expr, expected_type);
        } break;
        case expr_unary: {
            return check_unary_expr(ctx, expr, expected_type);
        } break;
        case expr_binary: {
            return check_binary_expr(ctx, expr, expected_type);
        } break;
        case expr_ternary: {
            return check_ternary_expr(ctx, expr, expected_type);
        } break;
        case expr_member: {
            return check_member_expr(ctx, expr, expected_type);
        } break;
        case expr_index: {
            return check_index_expr(ctx, expr, expected_type);
        } break;
        case expr_call: {
            return check_call_expr(ctx, expr, expected_type);
        } break;
        case expr_compound: {
            return check_compound_expr(ctx, expr, expected_type);
        } break;
        case expr_init_list: {
            return check_init_list_expr(ctx, expr, expected_type);
        } break;
        case expr_typeid_expr: {
            return check_typeid_expr(ctx, expr, expected_type);
        } break;
        case expr_typeid_type: {
            return check_typeid_type(ctx, expr, expected_type);
        } break;
        case expr_sizeof_expr: {
            return check_sizeof_expr(ctx, expr, expected_type);
        } break;
        case expr_sizeof_type: {
            return check_sizeof_type(ctx, expr, expected_type);
        } break;
    }
    printf("Unreachable code. \n");
    exit(69);
}

Type* check_type_spec(CheckerContext* ctx, TypeSpec* type) {
    switch (type->kind) {
        case type_spec_name: {
            Sym* sym = env_get(ctx->curr_scope, type->name);
            if (!sym) {
                error(type->location, error_msg_undefined_symbol, 
                    (i32)type->name.length, type->name.buffer);
            }

            check_sym(ctx, sym);
            return sym->type;
        } break;
        case type_spec_func: {
            Type** args = darray_init(Type*, 2);
            u32 length = darray_length(type->func.args);

            for (u32 i = 0; i < length; i++) {
                Type* curr = check_type_spec(ctx, type->func.args[i]);
                if (!curr) {
                    error(type->location, MAGENTA_COLOR "%.*s" RED_COLOR " type cannot be found in this scope. \n", 
                        (i32)type->name.length, type->name.buffer);
                }

                darray_push(args, curr);
            }

            Type* ret_type = check_type_spec(ctx, type->func.ret_type);
            if (!ret_type) {
                error(type->location, MAGENTA_COLOR "%.*s" RED_COLOR " type cannot be found in this scope. \n", 
                    (i32)type->name.length, type->name.buffer);
            }

            Type* func_type = type_func_init(args, ret_type);
            darray_push(ctx->types, func_type);

            return func_type;
        } break;
        case type_spec_ptr: {
            if (type->ptr.base->kind == type_spec_name) {
                if (env_get(ctx->curr_scope, type->ptr.base->name)->state == sym_resolving) {
                    Type* res = type_ptr_init(null);
                    DeferedType defered = {env_get(ctx->curr_scope, type->ptr.base->name), res};
                    darray_push(ctx->defered_types, defered);
                    return res;
                } 
            }

            Type* base = check_type_spec(ctx, type->ptr.base);
            Type* ptr_type = type_ptr_init(base);
            darray_push(ctx->types, ptr_type);

            return ptr_type;
        } break;
        case type_spec_array: {
            Type* base = check_type_spec(ctx, type->array.base);
            Type* arr_type = type_array_init(base, type->array.length);
            darray_push(ctx->types, arr_type);

            return arr_type;
        } break;
        case type_spec_none: {
            return none_type;
        } break;
    }

    printf(RED_COLOR "Unreachable code .\n" WHITE_COLOR);
    exit(69);
}


void check_enum_decl(CheckerContext* ctx, Sym* sym) {
    ctx->curr_stmt = sym->decl;

    scope_push(ctx);

    String* items = darray_init(String, 2);
    u32 length = darray_length(sym->decl->decl.enum_decl.items);

    // printf("checking enum %.*s =============== \n", (i32)sym->decl->name.length, sym->decl->name.buffer);
    for (u32 i = 0; i < length;  i++) {
        EnumItem curr = sym->decl->decl.enum_decl.items[i];

        // printf("\tchecking %.*s in %.*s \n", (i32)curr.name.length, curr.name.buffer, (i32)sym->decl->name.length, sym->decl->name.buffer);
        if (env_get_current(ctx->curr_scope, curr.name)) {
            error(curr.location, error_msg_symbol_redefinition,
                (i32)curr.name.length, curr.name.buffer);
        }

        env_push(ctx->curr_scope, sym_var_init(curr.name, null));
        darray_push(items, curr.name);
    }
    // printf("finished checking enum %.*s =============== \n", (i32)sym->decl->name.length, sym->decl->name.buffer);

    sym->decl->decl.scope = ctx->curr_scope;
    sym->type = type_enum_init(sym->decl->decl.name, items);
    darray_push(ctx->types, sym->type);
    scope_pop(ctx);
}


void check_union_decl(CheckerContext* ctx, Sym* sym) {
    ctx->curr_stmt = sym->decl;

    scope_push(ctx);

    TypeMember* members = darray_init(TypeMember, 2);
    u32 length = darray_length(sym->decl->decl.compound.members);
    u32 maximum_size = 0;
    u32 maximum_align = 0;

    // printf("checking union %.*s =============== \n", (i32)sym->decl->name.length, sym->decl->name.buffer);
    for (u32 i = 0; i < length; i++) {
        Member* curr = &sym->decl->decl.compound.members[i];

        // printf("\tchecking %.*s in %.*s \n", (i32)curr->name.length, curr->name.buffer, (i32)sym->decl->name.length, sym->decl->name.buffer);
        if (env_get_current(ctx->curr_scope, curr->name)) {
            error(curr->location, error_msg_symbol_redefinition,
                (i32)curr->name.length, curr->name.buffer);
        }

        Type* type = check_type_spec(ctx, curr->type);
        if (!type) return;

        TypeMember member = {.name = curr->name, .type = type, .offset = 0};
        maximum_size = type->size > maximum_size ? type->size : maximum_size;
        maximum_align = type->align > maximum_align ? type->align : maximum_align;

        env_push(ctx->curr_scope, sym_var_init(curr->name, type));
        darray_push(members, member);
    }
    // printf("finished checking union %.*s =============== \n", (i32)sym->decl->name.length, sym->decl->name.buffer);

    sym->decl->decl.scope = ctx->curr_scope;
    sym->type = type_union_init(sym->decl->decl.name, members, align_up(maximum_size, maximum_align), maximum_align);
    darray_push(ctx->types, sym->type);
    scope_pop(ctx);
}


void check_struct_decl(CheckerContext* ctx, Sym* sym) {
    ctx->curr_stmt = sym->decl;

    scope_push(ctx);

    TypeMember* members = darray_init(TypeMember, 2);
    u32 length = darray_length(sym->decl->decl.compound.members);
    u32 offset = 0;
    u32 maximum_align = 0;


    // printf("checking struct %.*s =============== \n", (i32)sym->decl->name.length, sym->decl->name.buffer);
    for (u32 i = 0; i < length; i++) {
        Member* curr = &sym->decl->decl.compound.members[i];

        // printf("\tchecking %.*s in %.*s \n", (i32)curr->name.length, curr->name.buffer, (i32)sym->decl->name.length, sym->decl->name.buffer);
        if (env_get_current(ctx->curr_scope, curr->name)) {
            error(curr->location, error_msg_symbol_redefinition,
                (i32)curr->name.length, curr->name.buffer);
        }

        Type* type = check_type_spec(ctx, curr->type);
        if (!type) return;

        offset = align_up(offset, type->align);
        TypeMember member = {.name = curr->name, .type = type, .offset = offset};
        offset += type->size;
        maximum_align = type->align > maximum_align ? type->align : maximum_align;

        env_push(ctx->curr_scope, sym_var_init(curr->name, type));
        darray_push(members, member);
    }
    // printf("finished checking struct %.*s =============== \n", (i32)sym->decl->name.length, sym->decl->name.buffer);
    sym->decl->decl.scope = ctx->curr_scope;
    sym->type = type_struct_init(sym->decl->decl.name, members, align_up(offset, maximum_align), maximum_align);
    darray_push(ctx->types, sym->type);
    scope_pop(ctx);
}


void check_func_decl(CheckerContext* ctx, Sym* sym) {
    ctx->curr_stmt = sym->decl;

    Type* ret_type = check_type_spec(ctx, sym->decl->decl.func.ret_type);
    if (!ret_type) return;

    Env* old_scope = ctx->curr_scope;
    scope_push_env(ctx, ctx->scopes[0]);

    Type** args = darray_init(Type*, 2);
    u32 length = darray_length(sym->decl->decl.func.args);

    for (u32 i = 0;i < length; i++) {
        Argument curr = sym->decl->decl.func.args[i];

        if (env_get_current(ctx->curr_scope, curr.name)) {
            error(curr.location, error_msg_symbol_redefinition, 
                (i32)curr.name.length, curr.name.buffer);
        }

        Type* type = check_type_spec(ctx, curr.type);
        if (!type) return;

        env_push(ctx->curr_scope, sym_var_init(curr.name, type));
        darray_push(args, type);
    }

    sym->type = type_func_init(args, ret_type);
    sym->state = sym_resolved;

    if (!sym->decl->decl.func.is_extern) {
        length = darray_length(sym->decl->decl.func.body); 
        for (u32 i = 0; i < length; i++) {
            Stmt* curr = sym->decl->decl.func.body[i];
            check_stmt(ctx, curr, ret_type);
        }
    }


    sym->decl->decl.scope = ctx->curr_scope;
    darray_push(ctx->types, sym->type);
    scope_pop_to(ctx, old_scope);
}


void check_var_decl(CheckerContext* ctx, Sym* sym) {
    ctx->curr_stmt = sym->decl;

    if (sym->decl->decl.var.type->kind == type_spec_none) {
        Operand* rhs = check_expr(ctx, sym->decl->decl.var.value, null);
        if (!rhs) return ;


        if (rhs->type && rhs->type->kind == type_none) {
            error(sym->decl->decl.location, 
                "Cannot assign a none type expression to a variable. \n");
        }

        sym->type = rhs->type;
        sym->decl->decl.type = rhs->type;
        return ;
    } else {
        Type* type = check_type_spec(ctx, sym->decl->decl.var.type);
        if (!type) return;

        if (sym->decl->decl.var.value->kind == expr_none) {
            sym->type = type;
            sym->decl->decl.type = type;

            return ;
        } else {
            Operand* rhs = check_expr(ctx, sym->decl->decl.var.value, type);
            if (!rhs) return ;

            if (rhs->type && rhs->type->kind == type_none) {
                error(sym->decl->decl.location, 
                    "Cannot assign a none type expression to a variable. \n");
            }

            if (!type_eq(type, rhs->type)) {
                if (type->kind == type_rawptr && rhs->type->kind == type_ptr) {
                    goto var_decl_finish;
                } else if (is_any_type(type) && !is_any_type(rhs->type)) {
                    if (operand_has_rvalue_flag(rhs)) {

                        Stmt* generated = generate_temp_variable(ctx, sym->decl->decl.var.value);

                        sym->decl->decl.var.value = expr_name_init(generated->location, generated->decl.name);
                        sym->decl->decl.var.value->type = generated->decl.type;
                    }

                    sym->decl->decl.var.value = convert_to_any_type(ctx, sym->decl->decl.var.value);
                    goto var_decl_finish;
                }

                String expected_name = type_get_name(type);
                String found_name = type_get_name(rhs->type);

                error(sym->decl->decl.var.type->location, 
                    "Expected " MAGENTA_COLOR "%.*s" RED_COLOR " type but got " MAGENTA_COLOR "%.*s" RED_COLOR ". \n", 
                    (i32)expected_name.length, expected_name.buffer, 
                    (i32)found_name.length, found_name.buffer);
            }

var_decl_finish:
            sym->type = type;
            sym->decl->decl.type = type;
            return ;
        }

    }

    return ;
}

void check_const_decl(CheckerContext* ctx, Sym* sym) {
    ctx->curr_stmt = sym->decl;
    if (sym->decl->decl.var.type->kind == type_spec_none) {
        Operand* rhs = check_expr(ctx, sym->decl->decl.var.value, null);
        if (!rhs) return ;

        if (rhs->type && rhs->type->kind == type_none) {
            error(sym->decl->decl.location, 
                "Cannot assign a none type expression to a variable. \n");
        }

        if (!operand_has_const_flag(rhs)) {
            error(sym->decl->decl.location,
            "Cannot assign non const expression to a const variable. \n");
        }

        sym->type = rhs->type;
        sym->val = rhs->val;
        sym->decl->decl.type = rhs->type;
        return ;
    } else {
        Type* type = check_type_spec(ctx, sym->decl->decl.var.type);
        if (!type) return ;

        if (sym->decl->decl.var.value->kind == expr_none) {
            if (!sym->decl->decl.var.is_extern) {
                error(sym->decl->decl.location, "Cannot declare uninitialized const variable. \n");
            }

            sym->type = type;
            sym->val = null;
            sym->decl->decl.type = type;
            return ;
        } else {
            Operand* rhs = check_expr(ctx, sym->decl->decl.var.value, type);
            if (!rhs) return ;

            if (rhs->type && rhs->type->kind == type_none) {
                error(sym->decl->decl.location, 
                    "Cannot assign a none type expression to a variable. \n");
            }

            if (!operand_has_const_flag(rhs)) {
                error(sym->decl->decl.location,
                "Cannot assign non const expression to a const variable. \n");
            }

            if (!type_eq(type, rhs->type)) {
                if (type->kind == type_rawptr && rhs->type->kind == type_ptr) {
                    goto const_decl_finish;
                } else if (is_any_type(type) && !is_any_type(rhs->type)) {
                    if (operand_has_rvalue_flag(rhs)) {

                        Stmt* generated = generate_temp_variable(ctx, sym->decl->decl.var.value);

                        sym->decl->decl.var.value = expr_name_init(generated->location, generated->decl.name);
                        sym->decl->decl.var.value->type = generated->decl.type;
                    }

                    sym->decl->decl.var.value = convert_to_any_type(ctx, sym->decl->decl.var.value);
                    goto const_decl_finish;
                }


                String expected_name = type_get_name(type);
                String found_name = type_get_name(rhs->type);

                error(sym->decl->decl.var.type->location, 
                    "Expected " MAGENTA_COLOR "%.*s" RED_COLOR " type but got " MAGENTA_COLOR "%.*s" RED_COLOR ". \n", 
                    (i32)expected_name.length, expected_name.buffer, 
                    (i32)found_name.length, found_name.buffer);
            }

const_decl_finish:
            sym->type = type;
            sym->val = rhs->val;
            sym->decl->decl.type = type;
            return ;
        }

    }

    return ;
}

void check_sym(CheckerContext* ctx, Sym* sym) {
    if (sym->state == sym_resolving) {
        printf(RED_COLOR "Cyclic dependency. \n" WHITE_COLOR);
        exit(69);
    } else if (sym->state == sym_resolved) {
        return ;
    } else {
        sym_set_state(sym, sym_resolving);
        switch (sym->decl->decl.kind) {
            case decl_union: {
                check_union_decl(ctx, sym);
                sym->decl->decl.type = sym->type;
            } break;
            case decl_struct: {
                check_struct_decl(ctx, sym);
                sym->decl->decl.type = sym->type;
            } break;
            case decl_enum: {
                check_enum_decl(ctx, sym);
                sym->decl->decl.type = sym->type;
            } break;
            case decl_func: {
                check_func_decl(ctx, sym);
                sym->decl->decl.type = sym->type;
            } break;
            case decl_var: {
                check_var_decl(ctx, sym);
                sym->decl->decl.type = sym->type;
            } break;
            case decl_const: {
                check_const_decl(ctx, sym);
                sym->decl->decl.type = sym->type;
            } break;
            case decl_import: {
                printf(RED_COLOR "Unreachable code. \n" WHITE_COLOR);
                exit(69);
            } break;
        }

        sym_set_state(sym, sym_resolved);
        return ;
    }

    printf(RED_COLOR "Unreachable code. \n" WHITE_COLOR);
    exit(69);
}


b8 check_decl(CheckerContext* ctx, Stmt* decl) {
    switch (decl->decl.kind) {
        case decl_var: {
            if (env_get_current(ctx->curr_scope, decl->decl.name)) {
                error(decl->decl.location, error_msg_symbol_redefinition, 
                    (i32)decl->decl.name.length, decl->decl.name.buffer);
            }

            Sym* sym = sym_var_init(decl->decl.name, null);
            sym->decl = decl;

            check_var_decl(ctx, sym);
            env_push(ctx->curr_scope, sym);

            return true;
        } break;
        case decl_const: {
            if (env_get_current(ctx->curr_scope, decl->decl.name)) {
                error(decl->decl.location, error_msg_symbol_redefinition, 
                    (i32)decl->decl.name.length, decl->decl.name.buffer);
            }

            Sym* sym = sym_const_init(decl->decl.name, null, null);
            sym->decl = decl;

            check_const_decl(ctx, sym);
            env_push(ctx->curr_scope, sym);

            return true;
        } break;
        case decl_func:
        case decl_enum:
        case decl_union:
        case decl_struct: {
            Sym* sym = env_get(ctx->curr_scope, decl->decl.name);
            if (!sym) {
                error(decl->decl.location, error_msg_undefined_symbol, 
                    (i32)decl->decl.name.length, decl->decl.name.buffer);
            }

            check_sym(ctx, sym);

            return true;
        } break;
        case decl_import: {
        } break;
    }

    printf(RED_COLOR "Unreachable code .\n" WHITE_COLOR);
    exit(69);
}

b8 check_if(CheckerContext* ctx, IfStmt* stmt, Type* expected_type) {
    scope_push(ctx);

    ctx->is_checking_cond = true;
    Operand* cond = check_expr(ctx, stmt->cond, null);
    ctx->is_checking_cond = false;
    check_result(cond);

    if (!is_scalar_type(cond->type)) {
        error(stmt->cond->location, "Invalid condition expression. \n");
    }

    for (u32 i = 0; i < darray_length(stmt->then); i++) {
        check_stmt(ctx, stmt->then[i], expected_type);
    }

    stmt->scope = ctx->curr_scope;
    scope_pop(ctx);
    return true;
}

b8 check_if_stmt(CheckerContext* ctx, Stmt* stmt, Type* expected_type) {
    if (!check_if(ctx, stmt->if_stmt.if_stmt, expected_type)) {
        return false;
    }
    for (u32 i = 0; i < darray_length(stmt->if_stmt.else_ifs); i++) {
        if (!check_if(ctx, stmt->if_stmt.else_ifs[i], expected_type)) {
            return false;
        }
    }

    if (stmt->if_stmt.else_stmt) {
        scope_push(ctx);
        for (u32 i = 0; i < darray_length(stmt->if_stmt.else_stmt); i++) {
            check_stmt(ctx, stmt->if_stmt.else_stmt[i], expected_type);
        }

        stmt->if_stmt.else_scope = ctx->curr_scope;
        scope_pop(ctx);
    }

    return true;
}

b8 check_assign_stmt(CheckerContext* ctx, Stmt* stmt) {
    Operand* lhs = check_expr(ctx, stmt->assign.lhs, null);
    check_result(lhs);

    if (operand_has_rvalue_flag(lhs)) {
        error(stmt->location, "Cannot assign to an rvalue expression. \n");
    }

    if (operand_has_const_flag(lhs)) {
        error(stmt->location, "Cannot assign to a const. \n");
    }

    if (is_none_type(lhs->type)) {
        error(stmt->location, "Cannot assign to a none type expression. \n");
    }

    if (is_array_type(lhs->type)) {
        error(stmt->location, "Cannot assign to an array type expression. \n");
    }

    if (stmt->assign.op >= token_asign_start && stmt->assign.op <= token_asign_end) { // +=, *=, /=, &&=, ...
        if (!is_integer_type(lhs->type) && !is_float_type(lhs->type)) {

            error(stmt->location, "The " MAGENTA_COLOR "%s" RED_COLOR " assign operator only supported by integer and float types.\n", 
                token_string((TokenKind)stmt->assign.op));
        }
    }

    Operand* rhs = check_expr(ctx, stmt->assign.rhs, lhs->type);
    check_result(rhs);

    if (!type_eq(lhs->type, rhs->type)) {
        if (is_rawptr_type(lhs->type) && is_ptr_type(rhs->type)) {
            return true;
        } else if (is_any_type(lhs->type) && !is_any_type(rhs->type)) {
            if (operand_has_rvalue_flag(rhs)) {

                Stmt* generated = generate_temp_variable(ctx, stmt->assign.rhs);

                stmt->assign.rhs = expr_name_init(generated->location, generated->decl.name);
                stmt->assign.rhs->type = generated->decl.type;
            }

            stmt->assign.rhs = convert_to_any_type(ctx, stmt->assign.rhs);
            return true;
        }

        String lhs_name = type_get_name(lhs->type);
        String rhs_name = type_get_name(rhs->type);

        error(stmt->assign.rhs->location, "Expected " MAGENTA_COLOR "%.*s" RED_COLOR " type but got " MAGENTA_COLOR "%.*s" RED_COLOR ". \n", 
            (i32)lhs_name.length, lhs_name.buffer, (i32)rhs_name.length, rhs_name.buffer);
    }


    return true;
}


b8 check_for_stmt(CheckerContext* ctx, Stmt* stmt, Type* expected_type) {
    scope_push(ctx);

    ctx->is_checking_cond = true;
    check_stmt(ctx, stmt->for_stmt.init, null);

    Operand* cond = check_expr(ctx, stmt->for_stmt.cond, null);
    check_result(cond);

    if (!is_scalar_type(cond->type) && !is_none_type(cond->type)) {
        error(stmt->while_stmt.cond->location, "Invalid condition expression. \n");
    }

    check_stmt(ctx, stmt->for_stmt.post, null);
    ctx->is_checking_cond = false;



    for (u32 i = 0; i < darray_length(stmt->for_stmt.body); i++) {
        check_stmt(ctx, stmt->for_stmt.body[i], expected_type);
    }

    stmt->scope = ctx->curr_scope;
    scope_pop(ctx);
    return true;
}

b8 check_while_stmt(CheckerContext* ctx, Stmt* stmt, Type* expected_type) {
    scope_push(ctx);

    ctx->is_checking_cond = true;
    Operand* cond = check_expr(ctx, stmt->while_stmt.cond, null);
    ctx->is_checking_cond = false;
    check_result(cond);


    if (!is_scalar_type(cond->type)) {
        error(stmt->while_stmt.cond->location, "Invalid condition expression. \n");
    }

    for (u32 i = 0; i < darray_length(stmt->while_stmt.body); i++) {
        check_stmt(ctx, stmt->while_stmt.body[i], expected_type);
    }

    stmt->scope = ctx->curr_scope;
    scope_pop(ctx);
    return true;
}


b8 check_stmt(CheckerContext* ctx, Stmt* stmt, Type* expected_type) {
    ctx->curr_stmt = stmt;

    switch (stmt->kind) {
        case stmt_expr: {
            Operand* operand = check_expr(ctx, stmt->expr, null);
            check_result(operand);

            return true;
        } break;
        case stmt_decl: {
            return check_decl(ctx, stmt);
        } break;
        case stmt_if: {
            return check_if_stmt(ctx, stmt, expected_type);
        } break;
        case stmt_for: {
            return check_for_stmt(ctx, stmt, expected_type);
        } break;
        case stmt_while: {
            return check_while_stmt(ctx, stmt, expected_type);
        } break;
        case stmt_assign: { 
            return check_assign_stmt(ctx, stmt);
        } break;

        case stmt_break:
        case stmt_continue: {
            stmt->scope = ctx->curr_scope;
            return true;
        }break;

        case stmt_block: {
            scope_push(ctx);

            Stmt** block = stmt->block.body;
            for (u32 i =0 ; i < darray_length(block); i++) {
                if (!check_stmt(ctx, block[i], expected_type)) {
                    return false;
                }
            }

            stmt->scope = ctx->curr_scope;
            scope_pop(ctx);
            return true;
        } break;
        case stmt_return: {
            Operand* rhs = check_expr(ctx, stmt->expr, expected_type);
            check_result(rhs);

            if (!type_eq(expected_type, rhs->type)) {
                error(stmt->expr->location, "The returned type doesn't match the function return type. \n");
                return false;
            }

            stmt->scope = ctx->curr_scope;
            return true;
        } break;
        case stmt_defer: {
            u8 res = check_stmt(ctx, stmt->defer, null);
            check_result(res);

            env_defer(ctx->curr_scope, stmt->defer);
            stmt->scope = ctx->curr_scope;
            return true;
        } break;
    }

    printf(RED_COLOR "Unreachable code .\n" WHITE_COLOR);
    exit(69);
}

#undef check_result


void compile_file(CheckerContext* ctx, Loc location, String name);
void order_sym(CheckerContext* ctx, Sym* sym, Sym*** ordered);
void order_expr(CheckerContext* ctx, Expr* expr, Sym*** ordered);

void order_type_spec(CheckerContext* ctx, TypeSpec* type, Sym*** ordered) {
    if (!type) return;

    switch (type->kind) {
    case type_spec_name: {
        Sym* sym = env_get(ctx->curr_scope, type->name);
        if (!sym) {
            error(type->location, error_msg_undefined_symbol, 
                (i32)type->name.length, type->name.buffer);
        }

        order_sym(ctx, sym, ordered);
    } break; 
    case type_spec_array: {
        order_type_spec(ctx, type->array.base, ordered);
    } break;
    case type_spec_none:
    case type_spec_func:
    case type_spec_ptr: return;
    }
}

void order_expr(CheckerContext* ctx, Expr* expr, Sym*** ordered) {
    if (!expr) return;

    switch (expr->kind) {
        case expr_none: 
        case expr_int:
        case expr_float: 
        case expr_str: break;

        case expr_name: {
            Sym* sym = env_get(ctx->curr_scope, expr->name);
            if(!sym) {
                error(expr->location, error_msg_undefined_symbol,
                    (i32)expr->name.length, expr->name.buffer);
            }

            order_sym(ctx, sym, ordered);
        } break;
        case expr_cast: {
            order_type_spec(ctx, expr->cast.type, ordered);
            order_expr(ctx, expr->cast.expr, ordered);
        } break;
        case expr_ternary: {
            order_expr(ctx, expr->ternary.cond, ordered);
            order_expr(ctx, expr->ternary.then_expr, ordered);
            order_expr(ctx, expr->ternary.else_expr, ordered);
        } break;
        case expr_binary: {
            order_expr(ctx, expr->binary.lhs, ordered);
            order_expr(ctx, expr->binary.rhs, ordered);
        } break;
        case expr_unary: {
            order_expr(ctx, expr->unary.expr, ordered);
        } break;
        case expr_member: {
            order_expr(ctx, expr->member.expr, ordered);
        } break;
        case expr_index: {
            order_expr(ctx, expr->index.expr, ordered);
            order_expr(ctx, expr->index.index, ordered);
        } break;
        case expr_call: {
            order_expr(ctx, expr->call.callee, ordered);
            for (u32 i = 0; i < darray_length(expr->call.params); i++) {
                order_expr(ctx, expr->call.params[i], ordered);
            } 
        } break;
        case expr_compound: {
            order_type_spec(ctx, type_spec_name_init(expr->location, expr->compound.name), ordered);
            for (u32 i = 0; i < darray_length(expr->compound.items); i++) {
                order_expr(ctx, expr->compound.items[i].expr, ordered);
            } 

        } break;
        case expr_init_list: {
            for (u32 i = 0; i < darray_length(expr->init_list.items); i++) {
                order_expr(ctx, expr->init_list.items[i], ordered);
            } 
        } break;
        case expr_sizeof_expr:
        case expr_typeid_expr: {
            order_expr(ctx, expr->type_id.expr, ordered);
        } break;
        case expr_sizeof_type:
        case expr_typeid_type: {
            order_type_spec(ctx, expr->type_id.type, ordered);
        } break;

    }

    return;
}

void order_sym(CheckerContext* ctx, Sym* sym, Sym*** ordered) {
    switch (sym->order_state) {
    case sym_ordered: return;
    case sym_ordering: {
        error(sym->decl->location, "Cycle detected when resolving %.*s. \n", 
            (i32)sym->decl->decl.name.length, sym->decl->decl.name.buffer);
    } break;

    case sym_unordered: {
        if (!sym->decl) { 
            sym->order_state = sym_ordered; 
            break; 
        }

        sym->order_state = sym_ordering;
        switch (sym->decl->decl.kind) {
            case decl_const:
            case decl_var: {
                order_type_spec(ctx, sym->decl->decl.var.type, ordered);
                order_expr(ctx, sym->decl->decl.var.value, ordered);
                darray_push(*ordered, sym);
            } break;
            case decl_enum: {
                darray_push(*ordered, sym);
            } break;
            case decl_func: {
                darray_push_front(*ordered, sym);
            } break;
            case decl_union:
            case decl_struct: {
                for (u32 i = 0; i < darray_length(sym->decl->decl.compound.members); i++) {

                    order_type_spec(ctx, sym->decl->decl.compound.members[i].type, ordered);
                }

                darray_push(*ordered, sym);
            } break;
            case decl_import: {
            } break;
        }
        sym->order_state = sym_ordered;
    }
      break;
    }
}

Sym** order_syms(CheckerContext* ctx, Sym** syms) {
    Sym** ordered_syms = darray_init(Sym*, 2);

    u32 length = darray_length(syms);
    for (u32 i = 0; i < length; i++) {
        Sym* curr = syms[i];

        if (curr->state == sym_resolved) {
            continue;
        }
        if (curr && curr->decl) {
            order_sym(ctx, curr, &ordered_syms);
        }
    }

    return ordered_syms;
}


void collect_syms(CheckerContext* ctx, Stmt** stmts) {
    u32 length = darray_length(stmts);
    for (u32 i = 0; i < length; i++) {
        Stmt* curr = stmts[i];
        if (curr->kind == stmt_decl) {
            if (env_get_current(ctx->curr_scope, curr->decl.name)) {
                error(curr->decl.location, error_msg_symbol_redefinition,
                    (i32)curr->decl.name.length, curr->decl.name.buffer);
            }

            switch (curr->decl.kind) {
                case decl_var:{
                    Sym* sym = sym_var_init(curr->decl.name, null);
                    sym->decl = curr;
                    sym_set_state(sym, sym_unresolved);

                    env_push(ctx->curr_scope, sym);
                } break;
                case decl_const: {
                    Sym* sym = sym_const_init(curr->decl.name, null, null);
                    sym->decl = curr;
                    sym_set_state(sym, sym_unresolved);

                    env_push(ctx->curr_scope, sym);
                } break;
                case decl_func: {
                    Sym* sym = sym_func_init(curr->decl.name, null);
                    sym->decl = curr;
                    sym_set_state(sym, sym_unresolved);

                    env_push(ctx->curr_scope, sym);
                } break;
                case decl_enum:
                case decl_union:
                case decl_struct: {
                    Sym* sym = sym_type_init(curr->decl.name, null);
                    sym->decl = curr;
                    sym_set_state(sym, sym_unresolved);
                    env_push(ctx->curr_scope, sym);
                } break;
                case decl_import: {
                    compile_file(ctx, curr->decl.location, curr->decl.name);
                } break;
            }
        }
    }
}


void compile_file(CheckerContext* ctx, Loc location, String name) {
    u8* buffer = null;
    u64 size = 0;
    String file_path = name; 
    if (string_starts_with(file_path, ".")) {
        if (string_starts_with(file_path, "./")) {
            string_trim_start(&file_path, 2);
        }

        String curr_dir = get_curr_workign_dir(ctx);
        file_path = string_cat(curr_dir, file_path);

    } else {
        for (u32 i = 0; i < darray_length(ctx->settings->include_dirs); i++) {
            String curr = ctx->settings->include_dirs[i];
            if (!string_ends_with(curr, "/")) {
                curr = string_cat(curr, string_c("/"));
            }

            String full_path = string_cat(curr, file_path);

            if (file_exists((char*)full_path.buffer)) {
                file_path = full_path;
                goto start_compiling;
            }
        }

        error(location, "Failed to find the imported file. \n");
    }


start_compiling:

    ImportedSym* res = get_import(ctx, file_path);
    if (res) {
        if (res->state == sym_importing) {
            error(location, "Cycle detected when importing %.*s file. \n", 
                (i32)name.length, name.buffer);
        } else {
            return;
        } 
    }

    if (!open_file((char*)file_path.buffer, &buffer, &size)) {
        error(location, "Failed to find the imported file. \n");
    }

    Module* file = parse_module(string(buffer, size));
    if (!file) {
        error(location, "Failed to compile the builtin types. \n");
    }

    ImportedSym* sym = imported_sym_init(file_path);
    darray_push(ctx->imports, sym);

    push_curr_working_dir(ctx, create_curr_working_dir(file_path));

    collect_syms(ctx, file->statements);
    sym->state = sym_imported;

    pop_curr_working_dir(ctx);
}

void check_module(Module* module, Settings* settings) {
    CheckerContext* ctx = checker_context_init();
    ctx->settings = settings;
    darray_push(ctx->types, none_type);

    // setup the entry file working directory
    String start_working_dir = create_curr_working_dir(string_c(settings->entry_file));
    push_curr_working_dir(ctx, start_working_dir);
    compile_file(ctx, (Loc) {}, string_c("std/builtin"));

    collect_syms(ctx, module->statements);

    Sym** ordered_syms = order_syms(ctx, ctx->curr_scope->symbols);


    Sym** defered_syms = darray_init(Sym*, darray_length(ordered_syms));

    // we check the symbols backward because the sorting algorithm 
    // returns the syms in the reverse order.
    u32 length = darray_length(ordered_syms);
    while (length--) {

        // note(redone): this is a temporary fix
        // variables doesn't seem to be ordered correctly when the type 
        // is not specified, so I got around that by defering variables
        // and functions to be checked after the types are checked.
        if (ordered_syms[length]->kind == sym_var 
            || ordered_syms[length]->kind == sym_const 
            || ordered_syms[length]->kind == sym_func) 
        {
            darray_push(defered_syms, ordered_syms[length]);
            continue;
        }

        check_sym(ctx, ordered_syms[length]);
    }

    // we check the defered variables and functions
    for (u32 i = 0; i < darray_length(defered_syms); i++) {
        check_sym(ctx, defered_syms[i]);
    }


    // go over the defered types that couldn't be resolved during the 
    // checking pass and set their correct type.
    length = darray_length(ctx->defered_types);
    for (u32 i = 0; i < length; i++) {
        DeferedType curr = ctx->defered_types[i];
        curr.type->ptr.base = curr.sym->type;
    }

    module->ordered_syms = ordered_syms;
    module->scopes = ctx->scopes;
    module->types = ctx->types;
    module->string_table = ctx->string_table;
}
