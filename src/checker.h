#pragma once


#include "ast.h"
#include "parser.h"
#include "type.h"
#include "driver.h"


enum OperandFlag {
    operand_lvalue_flag = (1 << 0),
    operand_rvalue_flag = (1 << 1),
    operand_type_flag   = (1 << 2),
    operand_const_flag  = (1 << 3)
};

typedef enum ConstValKind {
    const_val_none,
    const_val_int,
    const_val_float,
    const_val_bool,
    const_val_array,
    const_val_compound,
} ConstValKind;

typedef struct ConstVal ConstVal;
struct ConstVal {
    ConstValKind kind;
    union {
        struct {
            String*      names;
            ConstVal**   values;
        };
        u64         int_val;
        f64         float_val;
        b8          bool_val;
    };
};


ConstVal* const_val_int_init(u64 val);
ConstVal* const_val_float_init(f64 val);
ConstVal* const_val_bool_init(b8 val);


typedef struct Operand {
    Type*     type;
    ConstVal* val;
    u32       flags;
} Operand;


Operand* operand_init(Type* type);
void operand_set_value(Operand* op, ConstVal* val);
void operand_set_type(Operand* op, Type* type);
void operand_set_lvalue(Operand* op);
void operand_set_rvalue(Operand* op);
void operand_free(Operand* op);




typedef enum SymState {
    sym_unresolved,
    sym_resolving,
    sym_resolved,
} SymState;

typedef enum SymKind {
    sym_none,
    sym_const,
    sym_var,
    sym_type,
    sym_func,
} SymKind;

typedef enum OrderState {
    sym_unordered,
    sym_ordering,
    sym_ordered,
} OrderState;


struct Sym {
    SymState    state;
    OrderState  order_state; 

    SymKind     kind;
    String      name;
    Type*       type;
    Stmt*       decl;
    ConstVal*   val;
};


Sym* sym_type_init(String name, Type* type);
Sym* sym_var_init(String name, Type* type);
Sym* sym_const_init(String name, Type* type, ConstVal* val);
void sym_set_state(Sym* sym, SymState state);


struct Env {
    Env*   parent;

    // all the symbols in this scope.
    Sym**  symbols;

    // list of the defered statements of this scope
    Stmt** defered; 

};


Env* env_init(Env* parent);
Sym* env_get_current(Env* env, String name);
Sym* env_get_current_cstr(Env* env, const char* name);
Sym* env_get(Env* env, String name);
Sym* env_get_cstr(Env* env, const char* name);
b8   env_push(Env* env, Sym* sym);



typedef struct DeferedType {
    Sym*  sym;
    Type* type;
} DeferedType;


typedef enum ImportedSymState {
    sym_importing,
    sym_imported,
} ImportedSymState;


typedef struct ImportedSym {
    String name;
    ImportedSymState state;
} ImportedSym;


ImportedSym* imported_sym_init(String name);



typedef struct CheckerContext {

    // all the types in the the program.
    Type** types;

    // all the scopes in the program
    Env** scopes; 

    Env* curr_scope;

    // used to store the current statment the checker working on
    // it's mainly used to make sure we store the generated variables
    // temp variables/statements in the correct correct statement.
    Stmt* curr_stmt;


    // used to store the types the couldn't be checked but can be 
    // checked later. 
    // for example when you encouter a pointer to type T while 
    // checking the type T. that shouldn't be a cyclic dependency 
    // because a pointer size is always 8. so we defer these
    // pointer types to set their actual type later when the 
    // checking is done.
    DeferedType* defered_types;

    // all the imported files in the program.
    // used to prevent compiling same file multiple times
    // and detect import cycles.
    ImportedSym** imports;

    // stack of the working directories of the files that
    // are being imported us
    String* curr_working_dirs_stack;

    // contains include dirs, lib dirs ...
    Settings* settings; 

    // used to store the literal strings defined in the program
    u8* string_table; 

    // true when checking a condition expression. it's used
    // to decide which scope to put the generated temporary 
    // variable in.
    b8 is_checking_cond; 

    // used to generate unique names for temp variables generated 
    // by the checker.
    u64 counter;

} CheckerContext;


CheckerContext* checker_context_init();
void scope_push(CheckerContext* ctx);
void scope_pop(CheckerContext* ctx);
ImportedSym* get_import(CheckerContext* ctx, String name);





void check_module(Module* module, Settings* setting);

