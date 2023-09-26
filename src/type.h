#pragma once

#include "utils/core.h"
#include "utils/string.h"
#include "utils/darray.h"

typedef struct Type Type;

typedef enum TypeKind {
    type_none,
    type_func,
    type_integer,
    type_boolean,
    type_float,
    type_rawptr,
    type_ptr,
    type_array,
    type_struct,
    type_union,
    type_enum,
} TypeKind;


inline const char* type_kind_string(TypeKind kind) {
    switch (kind) {
    case type_none: return "none";
    case type_func: return "func";
    case type_integer: return "integer";
    case type_boolean: return "boolean";
    case type_float: return "float";
    case type_rawptr: return "rawptr";
    case type_ptr: return "pointer";
    case type_array: return "array";
    case type_struct: return "struct";
    case type_union: return "union";
    case type_enum: return "enum";
    }
}


typedef struct TypeMember {
    String  name;
    Type*   type;
    u32     offset;
} TypeMember;

typedef TypeMember  TypeArg;


struct Type {
    TypeKind    kind;
    u32         align;
    u32         size;
    u64         type_id;
    String      name;
    union {
        struct {
            Type*       ret_type;
            Type**      args;
            u32         num_args;
        } func;
        struct {
            u32         size;
            b8          is_signed;
        } integer;
        struct {
            u32         size;
        } float_type;
        struct {
            TypeMember*  members;
            u32          num_members;
        } compound;
        struct {
            Type*       base;
        } ptr;
        struct {
            String*     items;
            u32         num_items;
        } enum_type;
        struct {
            Type* base;
            u64 length;
        } array;
    };
};

#define is_none_type(type) ((type)->kind == type_none)
#define is_integer_type(type) ((type)->kind == type_integer)
#define is_float_type(type) ((type)->kind == type_float)
#define is_func_type(type) ((type)->kind == type_func)
#define is_struct_type(type) ((type)->kind == type_struct)
#define is_union_type(type) ((type)->kind == type_union)
#define is_compound_type(type) ((type)->kind == type_struct || (type)->kind == type_union)
#define is_enum_type(type) ((type)->kind == type_enum)
#define is_ptr_type(type) ((type)->kind == type_ptr)
#define is_rawptr_type(type) ((type)->kind == type_rawptr)
#define is_array_type(type) ((type)->kind == type_array)
#define is_scalar_type(type) ((type)->kind >= type_integer && (type)->kind <= type_ptr)
#define is_boolean_type(type) ((type)->kind == type_boolean)
#define is_any_type(type) ((type)->kind == type_struct && string_eq_cstr((type)->name, "any"))
#define is_string_type(type) ((type)->kind == type_struct && string_eq_cstr((type)->name, "string"))
#define is_cstr_type(type) ((type)->kind == type_struct && string_eq_cstr((type)->name, "cstr"))


TypeArg type_member_init(String name, Type* type, u32 offset);
String type_get_name(Type* type);
Type* type_struct_get_member_type(Type* type, String name);
b8 type_castable(Type* from, Type* to);
b8 type_eq(Type* lhs, Type* rhs);

Type* type_none_init();
Type* type_func_init(Type** args, Type* ret_type);
Type* type_integer_init(String name, u32 size, b8 is_signed);
Type* type_float_init(String name, u32 size);
Type* type_struct_init(String name, TypeMember* members, u32 size, u32 align);
Type* type_union_init(String name, TypeMember* members, u32 size, u32 align);
Type* type_enum_init(String name, String* items);
Type* type_ptr_init(Type* base);
Type* type_array_init(Type* base, u64 length);
Type* type_rawptr_init();
Type* type_boolean_init();

String get_func_ptr_name(Type** args, Type* ret_type);


void print_type(Type* type);