#include "type.h"


u64 curr_type_id = 0;

TypeArg type_member_init(String name, Type* type, u32 offset) {
    TypeArg ret;
    ret.name = name;
    ret.type = type;
    ret.offset = offset;

    return ret;
}

String type_get_name(Type* type) {
    switch (type->kind) {
    case type_integer: 
    case type_boolean: 
    case type_float: 
    case type_struct:
    case type_union:
    case type_enum:
    case type_func:   return type->name;
    case type_rawptr: return string_c("rawptr");
    case type_ptr:    return string_c("pointer");
    case type_none:   return string_c("none");
    case type_array:  return string_c("array");
    }
}


b8 type_castable(Type* from, Type* to) {
    if (!from || !to) {
        return false;
    }
    if (type_eq(from, to)) {
        return true;
    }

    if (is_scalar_type(from) && is_scalar_type(to)) {
        return true;
    }

    if (is_enum_type(from) && is_integer_type(to)) {
        return true;
    }

    if (is_any_type(to)) {
        return true;
    }

    return false;
}


b8 type_eq(Type* lhs, Type* rhs) {
    if (!lhs || !rhs) {
        return false;
    }

    if (lhs->type_id == rhs->type_id) {
        return true;
    }

    if (lhs->kind != rhs->kind) {
        return false;
    }

    switch (lhs->kind) {
        case type_array: {
            return type_eq(lhs->array.base, rhs->array.base) 
                && (lhs->array.length >= rhs->array.length);
        } break;
        case type_func: {
            if (lhs->func.num_args != rhs->func.num_args) {
                return false;
            }

            if (!type_eq(lhs->func.ret_type, rhs->func.ret_type)) {
                return false;
            }

            for (u32 i = 0; i < lhs->func.num_args; i++) {
                Type* ltype = lhs->func.args[i];
                Type* rtype = rhs->func.args[i];

                if (!type_eq(ltype, rtype)) {
                    return false;
                }
            }

            return true;
        } break;
        case type_integer: {
            return lhs->integer.is_signed == rhs->integer.is_signed 
                    && lhs->integer.size == rhs->integer.size;
        }
        case type_none:
        case type_rawptr:
        case type_boolean: {
            return true;
        } break;
        case type_float: {
            return lhs->float_type.size == rhs->float_type.size;
        } break;
        case type_union:
        case type_struct: {
            if (!string_eq(lhs->name, rhs->name)) {
                return false;
            }
            if (lhs->compound.num_members != rhs->compound.num_members) {
                return false;
            }
            for (u32 i = 0; i < lhs->compound.num_members; i++) {
                TypeMember lmember = lhs->compound.members[i];
                TypeMember rmember = rhs->compound.members[i];

                if (!string_eq(lmember.name, rmember.name)) {
                    return false;
                }
                if (!type_eq(lmember.type, rmember.type)) {
                    return false;
                }
            }

            return true;
        } break;
        case type_ptr: {
            return type_eq(lhs->ptr.base, rhs->ptr.base);
        } break;
        case type_enum: {
            if (lhs->enum_type.num_items != rhs->enum_type.num_items) {
                return false;
            }

            for (u32 i = 0; i < lhs->enum_type.num_items; i++) {
                String lname = lhs->enum_type.items[i];
                String rname = rhs->enum_type.items[i];
                if (!string_eq(lname, rname)) {
                    return false;
                }
            }

            return true;
        } break;
    }

    return false;
}

String get_func_ptr_name(Type** args, Type* ret_type) {
    u8* buffer = darray_init(u8, 2);

    darray_push(buffer, 'f');
    darray_push(buffer, 'n');
    darray_push(buffer, '(');
    u32 num_args = darray_length(args);
    for (u32 i = 0; i < num_args; i++) {
        String name = type_get_name(args[i]);
        for (u32 i = 0; i < name.length; i++) {
            darray_push(buffer, string_at(name, i));
        }
        if (i < num_args - 1) {
            darray_push(buffer, ',');
            darray_push(buffer, ' ');
        }
    }
    darray_push(buffer, ')');
    darray_push(buffer, ':');
    darray_push(buffer, ' ');
    String ret_name = type_get_name(ret_type);
    for (u32 i = 0; i < ret_name.length; i++) {
        darray_push(buffer, string_at(ret_name, i));
    }

    return string(buffer, darray_length(buffer));
}

// String get_type_ptr_name(Type* base) {
//     u8* buffer = darray_init(u8, 2);
//     Type* curr = base;

//     darray_push(buffer, '*');
//     while (curr->kind == type_ptr) {
//         darray_push(buffer, '*');
//         printf("here \n");
//         curr = curr->ptr.base;
//     }

//     u32 name_len = base->name.length;
//     for (u32 i = 0; i < name_len; i++) {
//         darray_push(buffer, string_at(base->name, i));
//     }

//     return string(buffer, darray_length(buffer));
// }

// String get_type_array_name(Type* type) {
//     u8* buffer = darray_init(u8, 2);
//     Type* base = type;
//     while (base->kind == type_array) {
//         base = base->array.base;
//     }

//     u32 name_len = type->name.length;
//     for (u32 i = 0; i < name_len; i++) {
//         darray_push(buffer, string_at(type->name, i));
//     }
    
//     Type* curr = type;
//     darray_push(buffer, '[');
//     darray_push(buffer, ']');
//     while (curr != base) {
//         // todo(redon): generate the array size as well
//         darray_push(buffer, '[');
//         darray_push(buffer, ']');
//         curr = curr->array.base;
//     }

//     return string(buffer, darray_length(buffer));
// }



Type* type_none_init() {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));
    type->kind = type_none;
    type->size = 0;
    type->align = 0;

    return type;
}


Type* type_func_init(Type** args, Type* ret_type) {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->kind = type_func;
    type->func.args = (Type**)copy_darray_buffer(args);
    type->func.num_args = darray_length(args);
    type->func.ret_type = ret_type;
    type->size  = 8;
    type->align = 8;
    type->name = get_func_ptr_name(args, ret_type);
    type->type_id = curr_type_id++;

    darray_free(args);

    return type;
}

Type* type_integer_init(String name, u32 size, b8 is_signed) {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->kind = type_integer;
    type->name = name;
    type->size = size;
    type->align = size;
    type->integer.size = size;
    type->integer.is_signed = is_signed;
    type->type_id = curr_type_id++;

    return type;
}

Type* type_float_init(String name, u32 size) {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->kind = type_float;
    type->name = name;
    type->size = size;
    type->align = size;
    type->float_type.size = size;
    type->type_id = curr_type_id++;

    return type;
}

Type* type_boolean_init() {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->name = string_init_cstr("bool");
    type->kind = type_boolean;
    type->size = 1;
    type->align = 1;
    type->type_id = curr_type_id++;

    return type;
}

Type* type_struct_init(String name, TypeMember* members, u32 size, u32 align) {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->kind = type_struct;
    type->name = name;
    type->compound.members = (TypeMember*)copy_darray_buffer(members);
    type->compound.num_members = darray_length(members);
    type->type_id = curr_type_id++;
    type->size = size;
    type->align = align;

    darray_free(members);

    return type;
}

Type* type_union_init(String name, TypeMember* members, u32 size, u32 align) {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->kind = type_union;
    type->name = name;
    type->compound.members = (TypeMember*)copy_darray_buffer(members);
    type->compound.num_members = darray_length(members);
    type->type_id = curr_type_id++;
    type->size = size;
    type->align = align;

    darray_free(members);

    return type;
}

Type* type_enum_init(String name, String* items) {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->kind = type_enum;
    type->name = name;
    type->enum_type.items = (String*)copy_darray_buffer(items);
    type->enum_type.num_items = darray_length(items);
    type->size = 4;
    type->align = 4;
    type->type_id = curr_type_id++;

    darray_free(items);

    return type;
}

Type* type_struct_get_member_type(Type* type, String name) {
    for (u32 i =0 ; i < type->compound.num_members; i++) {
        TypeMember member = type->compound.members[i];
        if (string_eq(member.name, name)) {
            return member.type;
        }
    }

    return null;
}

Type* type_ptr_init(Type* base) {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->kind = type_ptr;
    type->name = string_c("pointer");
    type->ptr.base = base;
    type->size = 8;
    type->align = 8;
    type->type_id = curr_type_id++;

    return type;
}

Type* type_array_init(Type* base, u64 length) {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->kind = type_array;
    type->array.base = base;
    type->name = string_c("array");
    type->array.length = length;
    type->size = base->size * length;
    type->align = base->align;
    type->type_id = curr_type_id++;

    return type;
}

Type* type_rawptr_init() {
    Type* type = (Type*)global_allocator_alloc(sizeof(Type));
    memset(type, 0, sizeof(Type));

    type->kind = type_rawptr;
    type->size = 8;
    type->align = 8;
    type->type_id = curr_type_id++;

    return type;
}





void print_type(Type* type) {
    switch (type->kind) {
        case type_none: {
            printf("none");
        } break; 
        case type_array: {
            print_type(type->array.base);
            printf("[%llu]", type->array.length);
        } break;
        case type_func:
        case type_integer:
        case type_float:
        case type_boolean: {
            printf("%.*s", (i32)type->name.length, type->name.buffer);
        } break;
        case type_rawptr: {
            printf("rawptr");
        } break;
        case type_enum: {
            printf("%.*s", (i32)type->name.length, type->name.buffer);
            printf("{");
            for (u32 i = 0; i < type->enum_type.num_items; i++) {
                printf("%.*s", (i32)type->name.length, type->name.buffer);
                if (i < type->enum_type.num_items - 1) printf(", ");
            }
            printf("}");
        } break;
        case type_ptr: {
            Type* curr = type;
            while (curr && curr->kind == type_ptr) {
                printf("*");
                curr = curr->ptr.base;
            }
            if (curr) {
                printf("%.*s", (i32)curr->name.length, curr->name.buffer);
            }
        } break;
        case type_struct: {
            printf("struct {");
            for (u32 i = 0; i < type->compound.num_members; i++) {
                print_type(type->compound.members[i].type);
                if (i < type->compound.num_members - 1) printf(", ");
            }
            printf("}");
        } break;
        case type_union: {
            printf("union {");
            for (u32 i = 0; i < type->compound.num_members; i++) {
                print_type(type->compound.members[i].type);
                if (i < type->compound.num_members - 1) printf(", ");
            }
            printf("}");
        } break;
    }
}