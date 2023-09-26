#include "../checker.h"
#include "../utils/emitter.h"


typedef struct EnvStack {
    Env** stack;
    u32   curr;
} EnvStack;


typedef struct CContext {
    Emitter         emitter;

    Env*            curr_func_scope;
    EnvStack        loop_stack;
    Env*            curr_loop_scope;

    // used to store the global variables because globals and initialize 
    // them in the start of the main function because global variables
    // initialization is limited to constant exprssions in c.
    Decl**          global_decls;

    // 
    u32             indent_level;
    u32             func_ptr_idx;
    b8              did_generate_ptr;
    b8              is_type_spec;
    b8              is_cond_expr;
} CContext;


inline CContext* c_context_init() {
    CContext* ctx = (CContext*)malloc(sizeof(CContext));
    ctx->func_ptr_idx = 0;
    ctx->indent_level = 0;
    ctx->did_generate_ptr = false;
    ctx->is_type_spec = false;
    ctx->is_cond_expr = false;

    ctx->emitter = emitter_init();
    ctx->global_decls = darray_init(Decl*, 2);

    ctx->loop_stack.stack = darray_init(Env*, 2);
    memset(ctx->loop_stack.stack, 0, 2 * sizeof(Env*));
    ctx->loop_stack.curr = 0;

    return ctx;
}


void push_loop(CContext* ctx, Env* scope) {
    darray_push(ctx->loop_stack.stack, scope);
}

void pop_loop(CContext* ctx) {
    darray_pop(ctx->loop_stack.stack);
}

Env* loop_current(CContext* ctx) {
    return ctx->loop_stack.stack[darray_length(ctx->loop_stack.stack) - 1];
}


inline void generate_stmt(CContext* ctx, Stmt* stmt);
inline void generate_decl(CContext* ctx, Decl  decl);
inline void generate_expr(CContext* ctx, Expr* expr);
inline void generate_type(CContext* ctx, Type* type);
inline void generate_type_info(CContext* ctx, Type** types);


#define generate_formatted(...) (emit_formatted(&(ctx->emitter), __VA_ARGS__))
#define generate_tabs(count) for (u32 i = 0; i < count; i++) { generate_formatted("    "); }


inline void generate_defered_current(CContext* ctx, Env* scope) {
    u32 length = darray_length(scope->defered);
    while (length--) {
        generate_stmt(ctx, scope->defered[length]);
    }
}

inline void generate_deffered_to(CContext* ctx, Env* scope, Env* target) {
    Env* curr = scope;
    while (curr != target->parent) {
        generate_defered_current(ctx, curr);
        curr = curr->parent;
    }
}

inline void generate_defered(CContext* ctx, Env* scope) {
    Env * curr = scope;
    while (curr) {
        generate_defered_current(ctx, curr);
        curr = curr->parent;
    }
}

inline void generate_expr(CContext* ctx, Expr* expr) {
    switch (expr->kind) {
        case expr_name: {
            generate_formatted("%.*s", (i32)expr->name.length, expr->name.buffer); 
        } break;
        case expr_int: {
            generate_formatted("%llu", expr->integer); 
        } break;
        case expr_float: {
            generate_formatted("%f", expr->float_lit);
        } break;
        case expr_str: {
            generate_formatted("\"");

            for (u32 i = 0; i < expr->name.length; i++) {
                u8 c = expr->name.buffer[i];
                generate_formatted("\\x%.2x", c);
            }
            generate_formatted("\"");
        } break;
        case expr_binary: {
            generate_formatted("(");
            generate_expr(ctx, expr->binary.lhs);
            generate_formatted(" %s ", token_string((TokenKind)expr->binary.op));
            generate_expr(ctx, expr->binary.rhs);
            generate_formatted(")");
        } break;

        case expr_unary: {
            generate_formatted("(");
            generate_formatted("%s", token_string((TokenKind)expr->unary.op));
            generate_expr(ctx, expr->unary.expr);
            generate_formatted(")");
        } break;

        case expr_cast: {
            generate_formatted("(");
            generate_type(ctx, expr->type);
            generate_formatted(")");
            generate_expr(ctx, expr->cast.expr);
        } break;

        case expr_call: {
            generate_expr(ctx, expr->call.callee);
            generate_formatted("(");

            u32 args_count = darray_length(expr->call.params);
            for (u32 i = 0; i < args_count; i++) {
                Expr* arg = expr->call.params[i];
                generate_expr(ctx, arg);
                if (args_count - 1 > i) generate_formatted(", ");
            }

            generate_formatted(")");
        } break;

        case expr_ternary: {
            generate_expr(ctx, expr->ternary.cond);
            generate_formatted(" ? ");
            generate_expr(ctx, expr->ternary.then_expr);
            generate_formatted(" : ");
            generate_expr(ctx, expr->ternary.else_expr);
        } break;

        case expr_member: {
            if (is_enum_type(expr->member.expr->type)) {
                generate_formatted("__Internal_");
                generate_expr(ctx, expr->member.expr);
                generate_formatted("_");
                generate_formatted("%.*s", (i32)expr->member.name.length, expr->member.name.buffer);
            } else {
                generate_expr(ctx, expr->member.expr);

                if (is_ptr_type(expr->member.expr->type)) {
                    generate_formatted("->");
                } else {
                    generate_formatted(".");
                }

                generate_formatted("%.*s", (i32)expr->member.name.length, expr->member.name.buffer);
            }
        } break;
        case expr_index: {
            generate_expr(ctx, expr->index.expr);
            generate_formatted("["); 
            generate_expr(ctx, expr->index.index);
            generate_formatted("]");
        } break;

        case expr_compound: {
            generate_formatted("(%.*s) { ", (i32)expr->compound.name.length, expr->compound.name.buffer);
            for (u32 i = 0; i < darray_length(expr->compound.items); i++) {
                StructInitItem curr = expr->compound.items[i];
                generate_formatted(".%.*s = ", (i32)curr.name.length, curr.name.buffer);
                generate_expr(ctx, curr.expr);
                generate_formatted(", ");
            }

            generate_formatted("}");
        } break;
        case expr_init_list: {
            generate_formatted("{ ");

            u32 length = darray_length(expr->init_list.items);
            if (length == 0) {
                generate_formatted("0");
            }

            for (u32 i = 0; i < length; i++) {
                Expr* curr = expr->init_list.items[i];
                generate_expr(ctx, curr);
                generate_formatted(", ");
            }

            generate_formatted("}");
        } break;

        case expr_typeid_type:
        case expr_typeid_expr: {
            generate_formatted("%llu", expr->type->type_id);
        } break;

        case expr_sizeof_expr:
        case expr_sizeof_type: {
            generate_formatted("%u", expr->type->size);
        } break;

        case expr_none:
            break;
        }
}


inline void generate_struct_members(CContext* ctx, Decl decl) {
    TypeMember* items = items = decl.type->compound.members;
    u32 items_count = decl.type->compound.num_members;

    ctx->indent_level += 1;
    for (u32 i = 0; i < items_count; i++) {
        TypeMember curr = items[i];

        generate_tabs(ctx->indent_level);
        ctx->is_type_spec = true;
        generate_type(ctx, curr.type);
        ctx->is_type_spec = false;

        generate_formatted(" %.*s", (i32)curr.name.length, curr.name.buffer);
        generate_formatted(")");

        Type* type = decl.type->compound.members[i].type;
        // the the base type
        while (type->kind == type_ptr) {
            type = type->ptr.base;
        }

        while (type->kind == type_array) {
            generate_formatted("[%llu]", type->array.length);
            type = type->array.base;
        }

        generate_formatted(";\n");
    }
    ctx->indent_level -= 1;
}


inline void generate_func_args(CContext* ctx, Decl decl) {
    Argument* args = args = decl.func.args;
    u32 items_count = darray_length(args);

    for (u32 i = 0; i < items_count; i++) {
        Argument curr = args[i];

        ctx->is_type_spec = true;
        generate_type(ctx, decl.type->func.args[i]);
        ctx->is_type_spec = false;

        generate_formatted(" %.*s", (i32)curr.name.length, curr.name.buffer);
        generate_formatted(")");

        Type* type = decl.type->func.args[i];
        // the the base type
        while (type->kind == type_ptr) {
            type = type->ptr.base;
        }

        while (type->kind == type_array) {
            generate_formatted("[%llu]", type->array.length);
            type = type->array.base;
        }

        if (i < items_count - 1) {
            generate_formatted(", ");
        }
    }
}


inline void generate_type(CContext* ctx, Type* type) {
    if (!type) return;
    switch (type->kind) {
        case type_none: {
            generate_formatted("void");

            if (ctx->is_type_spec) {
                generate_formatted("(");
            }
        } break;
        case type_array: {
            generate_type(ctx, type->array.base);
        } break;
        case type_enum:
        case type_integer:
        case type_boolean:
        case type_union:
        case type_float:
        case type_struct: {
            generate_formatted("%.*s", (i32)type->name.length, type->name.buffer);

            if (ctx->is_type_spec) {
                generate_formatted("(");
            }
        } break;
        case type_ptr: {
            generate_type(ctx, type->ptr.base);
            generate_formatted("*");
        } break;
        case type_rawptr: {
            generate_formatted("void*");

            if (ctx->is_type_spec) {
                generate_formatted("(");
            }
        } break;
        case type_func: {
            generate_formatted("typedef ");
            generate_type(ctx, type->func.ret_type);
            generate_formatted("(*__internal_func_ptr%u)(", ctx->func_ptr_idx);

            for (u32 i = 0; i < type->func.num_args; i++) {
                generate_type(ctx, type->func.args[i]);

                if (i < type->func.num_args - 1) {
                    generate_formatted(", ");
                }
            }

            generate_formatted("); \n");
            generate_formatted("__internal_func_ptr%u", ctx->func_ptr_idx);
            ctx->func_ptr_idx += 1;
        } break;
    }
}

inline void generate_decl(CContext* ctx, Decl decl) {
    switch (decl.kind) {
    case decl_var:
    case decl_const: {
        ctx->is_type_spec = true;
        if (!ctx->is_cond_expr) {
            generate_tabs(ctx->indent_level);
        }

        generate_type(ctx, decl.type);
        generate_formatted(" %.*s", (i32)decl.name.length, decl.name.buffer);
        generate_formatted(")");

        Type* type = decl.type;

        // the the base type
        while (type->kind == type_ptr) {
            type = type->ptr.base;
        }

        while (type->kind == type_array) {
            generate_formatted("[%llu]", type->array.length);
            type = type->array.base;
        }
        ctx->is_type_spec = false;

        if (decl.var.value->kind != expr_none) {
            generate_formatted(" = ");
            generate_expr(ctx, decl.var.value);
        }

        if (!ctx->is_cond_expr) {
            generate_formatted(";\n");
        }
    } break;
    case decl_func: {
        ctx->curr_func_scope = decl.scope;

        generate_formatted("\n");
        generate_tabs(ctx->indent_level);
        Type* type = decl.type->func.ret_type;
        generate_type(ctx, type);

        // the the base type
        while (type->kind == type_ptr) {
            type = type->ptr.base;
        }

        while (type->kind == type_array) {
            generate_formatted("[%llu]", type->array.length);
            type = type->array.base;
        }

        generate_formatted(" %.*s (", (i32)decl.name.length, decl.name.buffer);
        generate_func_args(ctx, decl);
        generate_formatted(")");

        if (!decl.func.is_extern) {
            generate_formatted(" {\n");
            ctx->indent_level += 1;

            // initialize the global variables that need to be initialized.
            if (string_eq_cstr(decl.name, "main")) {

                for (u32 i = 0;i < darray_length(ctx->global_decls); i++) {
                    Decl* curr = ctx->global_decls[i];
                    generate_tabs(ctx->indent_level);
                    generate_formatted("%.*s = ", (i32)curr->name.length, curr->name.buffer);
                    generate_expr(ctx, curr->var.value);
                    generate_formatted(";\n");
                }


                generate_tabs(ctx->indent_level);
                generate_formatted("__internal_generate_type_informations();\n");
            }


            // generate the actual function body
            for (u32 i = 0; i < darray_length(decl.func.body); i++) {
                generate_stmt(ctx, decl.func.body[i]);
            }

            // generate the defered statements in case the function doesn't have 
            // return statement
            generate_defered_current(ctx, decl.scope);
            ctx->indent_level -= 1;

            generate_tabs(ctx->indent_level);
            generate_formatted("}\n\n");
        } else {
            generate_formatted(";\n");
        }
    } break;

    case decl_struct: {
        generate_formatted("\n");
        generate_tabs(ctx->indent_level);

        generate_formatted("struct %.*s {\n", (i32)decl.name.length, decl.name.buffer);
        generate_struct_members(ctx, decl);

        generate_tabs(ctx->indent_level);
        generate_formatted("};\n\n");
    } break; 

    case decl_union: {
        generate_formatted("\n");
        generate_tabs(ctx->indent_level);

        generate_formatted("union %.*s {\n", (i32)decl.name.length, decl.name.buffer);
        generate_struct_members(ctx, decl);

        generate_tabs(ctx->indent_level);
        generate_formatted("};\n\n");
    } break; 

    case decl_enum: {
        generate_formatted("\n");
        generate_tabs(ctx->indent_level);

        generate_formatted("enum %.*s {\n", (i32)decl.name.length, decl.name.buffer);
        ctx->indent_level += 1;
        for (u32 i = 0; i < darray_length(decl.enum_decl.items); i++) {
            String curr = decl.enum_decl.items[i].name;

            generate_tabs(ctx->indent_level);
            generate_formatted("__Internal_");
            generate_formatted("%.*s_%.*s,\n", (i32)decl.name.length, decl.name.buffer, 
                (i32)curr.length, curr.buffer);
        }
        ctx->indent_level -= 1;

        generate_tabs(ctx->indent_level);
        generate_formatted("};\n\n");
    } break;
    case decl_import: {
    } break;
    }
}

inline void generate_if(CContext* ctx, IfStmt* if_, const char* name) {
    generate_formatted("%s (", name);
    generate_expr(ctx, if_->cond);
    generate_formatted(") {\n");

    ctx->indent_level += 1;


    // genetate if statement body.
    for (u32 i = 0; i < darray_length(if_->then); i++) {
        generate_stmt(ctx, if_->then[i]);
    }

    // genarate the defered statements for this scope.
    generate_defered_current(ctx, if_->scope);

    ctx->indent_level -= 1;

    generate_tabs(ctx->indent_level);
    generate_formatted("}");
}


inline void generate_stmt(CContext* ctx, Stmt* stmt) {
    if (stmt->generated_stmts) {
        for (u32 i = 0; i < darray_length(stmt->generated_stmts); i++) {
            generate_stmt(ctx, stmt->generated_stmts[i]);
        }
    }

    switch (stmt->kind) {

    case stmt_expr: {
        if (!ctx->is_cond_expr) {
            generate_tabs(ctx->indent_level);
        }

        generate_expr(ctx, stmt->expr); 

        if (!ctx->is_cond_expr) {
            generate_formatted(";\n");
        }
    } break;

    case stmt_decl: {
        generate_decl(ctx, stmt->decl);
    } break;

    case stmt_if: {
        generate_formatted("\n");
        generate_tabs(ctx->indent_level);
        generate_if(ctx, stmt->if_stmt.if_stmt, "if");
        for (u32 i = 0; i < darray_length(stmt->if_stmt.else_ifs); i++) {
            generate_if(ctx, stmt->if_stmt.else_ifs[i], " else if");
        }
        if (stmt->if_stmt.else_stmt) {
            generate_formatted(" else {\n");

            ctx->indent_level += 1;


            for (u32 i = 0; i < darray_length(stmt->if_stmt.else_stmt); i++) {
                generate_stmt(ctx, stmt->if_stmt.else_stmt[i]);
            }

            // genarate the defered statements for this scope.
            generate_defered_current(ctx, stmt->if_stmt.else_scope);

            ctx->indent_level -= 1;


            generate_tabs(ctx->indent_level);
            generate_formatted("}");
        }
        generate_formatted("\n\n");
    } break;

    case stmt_for: {
        push_loop(ctx, stmt->scope);

        generate_formatted("\n");
        generate_tabs(ctx->indent_level);
        ctx->is_cond_expr = true;
        generate_formatted("for (");
        generate_stmt(ctx, stmt->for_stmt.init);
        generate_formatted(";");
        generate_expr(ctx, stmt->for_stmt.cond);
        generate_formatted(";");
        generate_stmt(ctx, stmt->for_stmt.post);
        ctx->is_cond_expr = false;

        generate_formatted(") {\n");
        ctx->indent_level += 1;


        // generate the while statement body
        for (u32 i = 0; i < darray_length(stmt->for_stmt.body); i++) {
            generate_stmt(ctx, stmt->for_stmt.body[i]);
        }

        // genetate the defered statements of this scope
        generate_defered_current(ctx, stmt->scope);

        ctx->indent_level -= 1;
        generate_tabs(ctx->indent_level);
        generate_formatted("}\n\n");

        pop_loop(ctx);
    } break;

    case stmt_while: {
        push_loop(ctx, stmt->scope);

        generate_formatted("\n");
        generate_tabs(ctx->indent_level);
        generate_formatted("while (");
        generate_expr(ctx, stmt->while_stmt.cond);
        generate_formatted(") {\n");
        ctx->indent_level += 1;


        // generate the while statement body
        for (u32 i = 0; i < darray_length(stmt->while_stmt.body); i++) {
            generate_stmt(ctx, stmt->while_stmt.body[i]);
        }

        // genetate the defered statements of this scope
        generate_defered_current(ctx, stmt->scope);

        ctx->indent_level -= 1;
        generate_tabs(ctx->indent_level);
        generate_formatted("}\n\n");

        pop_loop(ctx);
    } break;

    case stmt_block: {
        generate_formatted("\n");
        generate_tabs(ctx->indent_level);
        generate_formatted("{\n");
        ctx->indent_level += 1;


        // generate the block body
        for (u32 i = 0; i < darray_length(stmt->block.body); i++) {
            generate_stmt(ctx, stmt->block.body[i]);
        }

        // generate the defered statements.
        generate_defered_current(ctx, stmt->scope);

        ctx->indent_level -= 1;
        generate_tabs(ctx->indent_level);
        generate_formatted("}\n\n");
    } break;

    case stmt_assign: {
        generate_tabs(ctx->indent_level);
        generate_expr(ctx, stmt->assign.lhs);
        generate_formatted(" %s ", token_string((TokenKind)stmt->assign.op));
        generate_expr(ctx, stmt->assign.rhs);

        if (!ctx->is_cond_expr) {
            generate_formatted(";\n");
        }
    } break;

    case stmt_break: {
        generate_deffered_to(ctx, stmt->scope, loop_current(ctx));
        generate_tabs(ctx->indent_level);
        generate_formatted("break;\n");
    } break;

    case stmt_continue: {
        generate_deffered_to(ctx, stmt->scope, loop_current(ctx));
        generate_tabs(ctx->indent_level);
        generate_formatted("continue;\n");
    } break;

    case stmt_return: {
        generate_defered(ctx, stmt->scope);

        generate_tabs(ctx->indent_level);
        generate_formatted("return ");
        generate_expr(ctx, stmt->expr);
        generate_formatted(";\n");
    } break;
    case stmt_defer: {
    } break;

    }
}

inline void c_codegen_init(CContext* ctx) {
    generate_formatted("#include <stdint.h>\n");
    generate_formatted("typedef uint8_t     u8;\n");
    generate_formatted("typedef uint16_t    u16;\n");
    generate_formatted("typedef uint32_t    u32;\n");
    generate_formatted("typedef uint64_t    u64;\n");
    generate_formatted("typedef int8_t      i8;\n");
    generate_formatted("typedef int16_t     i16;\n");
    generate_formatted("typedef int32_t     i32;\n");
    generate_formatted("typedef int64_t     i64;\n");
    generate_formatted("typedef float       f32;\n");
    generate_formatted("typedef double      f64;\n");
    generate_formatted("typedef _Bool       bool;\n");
    generate_formatted("const bool false    = 0;\n");
    generate_formatted("const bool true     = 1;\n");
}


static const char* decl_kind_keyword(DeclKind kind) {
    switch (kind) {
    case decl_struct: return "struct";
    case decl_union: return "union";
    case decl_enum: return "enum";
    default: break;
    }
    return "";
}

void forward_declare_type(CContext* ctx, Decl* decl) {
    generate_formatted("typedef %s %.*s    %.*s;\n", 
        decl_kind_keyword(decl->kind),
        (i32)decl->name.length, decl->name.buffer,
        (i32)decl->name.length, decl->name.buffer);
}


void forward_declare_func(CContext* ctx, Decl* decl) {
    generate_type(ctx, decl->type->func.ret_type);
    generate_formatted(" %.*s(", (i32)decl->name.length, decl->name.buffer);

    u32 length = darray_length(decl->func.args);
    for (u32 i = 0; i < length; i++) {

        // ctx->is_type_spec = true;

        Type* type = decl->type->func.args[i];
        generate_type(ctx, type);

        // the the base type
        while (type->kind == type_ptr) {
            type = type->ptr.base;
        }

        while (type->kind == type_array) {
            generate_formatted("[%llu]", type->array.length);
            type = type->array.base;
        }

        if (i < length - 1) {
            generate_formatted(", ");
        }
    }
    generate_formatted(");\n");
}

inline const char* type_info_kind_string(TypeKind kind) {
    switch (kind) {
    case type_none: return "__Internal_TypeInfoKind_None";
    case type_func: return "__Internal_TypeInfoKind_Function";
    case type_integer: return "__Internal_TypeInfoKind_Integer";
    case type_boolean: return "__Internal_TypeInfoKind_Boolean";
    case type_float: return "__Internal_TypeInfoKind_Float";
    case type_rawptr: return "__Internal_TypeInfoKind_RawPointer";
    case type_ptr: return "__Internal_TypeInfoKind_Pointer";
    case type_array: return "__Internal_TypeInfoKind_Array";
    case type_struct: return "__Internal_TypeInfoKind_Struct";
    case type_union: return "__Internal_TypeInfoKind_Union";
    case type_enum: return "__Internal_TypeInfoKind_Enum";
    }

    return "__Internal_TypeInfoKind_None";
}

inline void generate_type_info(CContext* ctx, Type** types) {
    u64 types_length= darray_length(types);
    generate_formatted("\n\nvoid __internal_generate_type_informations() {\n");
    ctx->indent_level += 1;

    generate_tabs(ctx->indent_level); 
    generate_formatted("type_infos = (TypeInfo**)malloc(%llu * sizeof(TypeInfo*));\n", types_length);

    generate_tabs(ctx->indent_level); 
    generate_formatted("type_infos_count = %llu;\n", types_length);
    
    generate_tabs(ctx->indent_level);
    generate_formatted("char* member_name_buff = 0;\n\n");

    for (u64 i = 0; i < darray_length(types); i++) {
        Type* curr = types[i];
        u64 id = curr->type_id;

        switch (curr->kind) {
        case type_none: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_formatted("\n");
        } break;
        case type_integer: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->size = %u;\n", id, (u32)curr->size);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->alignment = %u;\n", id, (u32)curr->align);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->info.integer.is_signed= %u;\n", id, (u32)curr->integer.is_signed);
            generate_formatted("\n");
        } break;
        case type_boolean: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->size = %u;\n", id, (u32)curr->size);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->alignment = %u;\n", id, (u32)curr->align);
            generate_formatted("\n");
        } break;
        case type_float: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->size = %u;\n", id, (u32)curr->size);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->alignment = %u;\n", id, (u32)curr->align);
            generate_formatted("\n");
        } break;
        case type_rawptr: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->size = %u;\n", id, (u32)curr->size);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->alignment = %u;\n", id, (u32)curr->align);
            generate_formatted("\n");
        } break;
        case type_ptr: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->size = %u;\n", id, (u32)curr->size);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->alignment = %u;\n", id, (u32)curr->align);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->info.pointer.base = %u;\n", id, (u32)curr->ptr.base->type_id);
            generate_formatted("\n");
        } break;
        case type_func: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->size = %u;\n", id, (u32)curr->size);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->alignment = %u;\n", id, (u32)curr->align);

            if (curr->func.num_args != 0) {
                generate_tabs(ctx->indent_level);
                generate_formatted("type_infos[%llu]->info.function.arguments = (u64*)malloc(%u * sizeof(u64));\n", id, (u32)curr->func.num_args);
            } 
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->info.function.arguments_count = %u;\n", id, (u32)curr->func.num_args);

            for (u32 idx = 0; idx < curr->func.num_args; idx++) {
                generate_tabs(ctx->indent_level);
                generate_formatted("type_infos[%llu]->info.function.arguments[%u] = %llu;\n", id, idx, curr->func.args[idx]->type_id);
            }

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->info.function.ret_type = %llu;\n", id, curr->func.ret_type->type_id);

            generate_formatted("\n");
        } break;
        case type_array: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->size = %u;\n", id, (u32)curr->size);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->alignment = %u;\n", id, (u32)curr->align);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->info.array.length = %llu;\n", id, curr->array.length);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->info.array.base = %llu;\n", id, curr->array.base->type_id);

            generate_formatted("\n");
        } break;
        case type_union:
        case type_struct: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->size = %u;\n", id, (u32)curr->size);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->alignment = %u;\n", id, (u32)curr->align);

            if (curr->compound.num_members != 0) {
                generate_tabs(ctx->indent_level);
                generate_formatted("type_infos[%llu]->info.compound.members = (MemberTypeInfo*)malloc( %u * sizeof(MemberTypeInfo));\n", id, curr->compound.num_members);
            }
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->info.compound.members_count = %u;\n", id, curr->compound.num_members);

            for (u64 idx = 0; idx < curr->compound.num_members; idx++) {
                TypeMember* member = &curr->compound.members[idx];
                generate_tabs(ctx->indent_level);
                generate_formatted("member_name_buff = \"%.*s\";\n", (i32)member->name.length, member->name.buffer);
                generate_tabs(ctx->indent_level);
                generate_formatted("type_infos[%llu]->info.compound.members[%llu].name = string_from_buffer(member_name_buff, %llu);\n", id, idx, member->name.length);
                generate_tabs(ctx->indent_level);
                generate_formatted("type_infos[%llu]->info.compound.members[%llu].offset = %u;\n", id, idx, member->offset);
                generate_tabs(ctx->indent_level);
                generate_formatted("type_infos[%llu]->info.compound.members[%llu].type = %llu;\n", id, idx, member->type->type_id);
            }

            generate_formatted("\n");
        } break;
        case type_enum: {
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu] = (TypeInfo*)malloc(sizeof(TypeInfo));\n", id);

            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->kind = %s;\n", id, type_info_kind_string(curr->kind));
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->size = %u;\n", id, (u32)curr->size);
            generate_tabs(ctx->indent_level);
            generate_formatted("type_infos[%llu]->alignment = %u;\n", id, (u32)curr->align);

            generate_formatted("\n");
        } break;
        }
    }

    ctx->indent_level -= 1;
    generate_formatted("}\n\n");
}

inline void generate_string_table(CContext* ctx, Module* module) {
    generate_formatted("const char* __internal_string_table = \"");

    for (u64 i = 0; i < darray_length(module->string_table); i++) {
        u8 curr = module->string_table[i];
        generate_formatted("\\x%.2x", curr);
    }
    generate_formatted("\";\n");
}


inline u8* generate_c(Module* module) {

    CContext* ctx = c_context_init();
    c_codegen_init(ctx);

    u64 length = darray_length(module->ordered_syms);

    // forward declare all the types
    for (u64 i = 0; i < length; i++) {
        Sym* curr = module->ordered_syms[i];
        switch (curr->decl->decl.kind) {
            case decl_struct:
            case decl_union:
            case decl_enum: forward_declare_type(ctx,  &curr->decl->decl); break;
            default: break;
        }
    }

    // generate all the types
    for (int i = 0; i < length; i++) {
        Sym* curr = module->ordered_syms[i];
        switch (curr->decl->decl.kind) {
            case decl_struct:
            case decl_union:
            case decl_enum: generate_decl(ctx, curr->decl->decl); break;
            default: break;
        }
    }

    // forward declare all the functions
    for (u64 i = 0; i < length; i++) {
        Sym* curr = module->ordered_syms[i];
        switch (curr->decl->decl.kind) {
            case decl_func: forward_declare_func(ctx, &curr->decl->decl); break;
            default: break;
        }
    }


    generate_string_table(ctx, module);


    // generate all the variables 
    for (u32 i = 0; i < length; i++) {
        Sym* curr = module->ordered_syms[i];
        if (curr->decl->generated_stmts) {
            for (u64 i = 0; i < darray_length(curr->decl->generated_stmts); i += 1) {
                generate_stmt(ctx, curr->decl->generated_stmts[i]);
            }
        }

        switch (curr->decl->decl.kind) {
            case decl_const: 
            case decl_var: {
                if (curr->decl->decl.var.is_extern) {
                    generate_formatted("extern ");
                }
                ctx->is_type_spec = true;
                generate_tabs(ctx->indent_level);
                generate_type(ctx, curr->decl->decl.type);
                generate_formatted(" %.*s", (i32)curr->decl->decl.name.length, curr->decl->decl.name.buffer);
                generate_formatted(")");

                Type* type = curr->decl->decl.type;
                // the the base type
                while (type->kind == type_ptr) {
                    type = type->ptr.base;
                }

                while (type->kind == type_array) {
                    generate_formatted("[%llu]", type->array.length);
                    type = type->array.base;
                }
                ctx->is_type_spec = false;

                generate_formatted(";\n");
            
                if (curr->decl->decl.var.value->kind != expr_none) {
                    darray_push(ctx->global_decls, &curr->decl->decl);
                }
            } break;
            // case decl_const: generate_decl(ctx, curr->decl->decl); break;
            default: break;
        }
    }


    // generate the runtime type informations
    generate_type_info(ctx, module->types);


    // generate all the functions
    for (int i = 0; i < length; i++) {
        Sym* curr = module->ordered_syms[i];
        switch (curr->decl->decl.kind) {
            case decl_func: {
                if (curr->decl->decl.func.is_extern) {
                    continue;
                }

                generate_decl(ctx, curr->decl->decl);
            } break;
            default: break;
        }
    }

    return ctx->emitter.buffer;
}

#undef generate_formatted