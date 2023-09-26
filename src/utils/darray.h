#pragma once
#include "types.h"
#include "stdlib.h"
#include "string.h"



#define DARRAY_HEADER(ptr)  (((u8*)ptr - sizeof(DarrayHeader)))


typedef struct DarrayHeader {
    u64     capacity;
    u64     length;
    u32     type_size;

} DarrayHeader;


#define darray_init(type, cap) ((type*)__darray_init(cap, sizeof(type)))
#define darray_free(ptr) (free(DARRAY_HEADER((ptr))), ptr = NULL)
#define darray_top(ptr) (ptr[((DarrayHeader*)DARRAY_HEADER((ptr)))->length - 1])

#define darray_reset(ptr) \
{ \
    DarrayHeader* header = (DarrayHeader*)DARRAY_HEADER((ptr)); \
    header->length = 0; \
}


#define darray_reserve(ptr, size) \
{ \
    DarrayHeader* header = (DarrayHeader*)DARRAY_HEADER((ptr)); \
    if (header->length + size >= header->capacity) {\
        __typeof__((ptr)) new_ = (__typeof__((ptr)))__darray_resize((ptr), header->length + size + 1); \
        free(header); \
        (ptr) = new_; \
    } \
}


#define darray_pop(ptr) \
{ \
    DarrayHeader* header = (DarrayHeader*)DARRAY_HEADER((ptr)); \
    if (header->length > 0) \
        header->length--; \
}


#define darray_push(ptr, data) \
{ \
    DarrayHeader* header = (DarrayHeader*)DARRAY_HEADER((ptr)); \
    if (header->length >= header->capacity) { \
        __typeof__((ptr)) new_ = (__typeof__((ptr)))__darray_resize((ptr), header->capacity + header->capacity / 2); \
        free(header); \
        (ptr) = new_; \
        header = (DarrayHeader*)DARRAY_HEADER((ptr)); \
    } \
    (ptr)[header->length++] = (data); \
}

#define darray_push_front(ptr, data) \
{ \
    DarrayHeader* header = (DarrayHeader*)DARRAY_HEADER((ptr)); \
    __typeof__((ptr)) new_ = (__typeof__((ptr)))__darray_resize((ptr), header->capacity + 1); \
    new_[0] = (data); \
    for (u32 i = 0; i < header->length; i++) { \
        new_[i + 1] = (ptr)[i]; \
    } \
    free(header); \
    (ptr) = new_; \
    header = (DarrayHeader*)DARRAY_HEADER((ptr)); \
    header->length += 1; \
}




inline u64 darray_length(void* ptr) {
    DarrayHeader* header = (DarrayHeader*)DARRAY_HEADER(ptr);
    return header->length;
}

inline u64 darray_capacity(void* ptr) {
    DarrayHeader* header = (DarrayHeader*)DARRAY_HEADER(ptr);
    return header->capacity;
}

inline void* __darray_init(u64 capacity, u32 type_size) {
    u64 total_size = capacity * type_size + sizeof(DarrayHeader);
    void* buffer = malloc(total_size);

    DarrayHeader* header = (DarrayHeader*)buffer;
    header->type_size = type_size;
    header->capacity = capacity;
    header->length = 0;


    return (u8*)buffer + sizeof(DarrayHeader);
};


inline void* __darray_resize(void* ptr, u64 new_capacity) {
    DarrayHeader* old_header = (DarrayHeader*)DARRAY_HEADER(ptr);
    u64 old_size = old_header->type_size * old_header->capacity + sizeof(DarrayHeader);
    u64 new_size = old_header->type_size * new_capacity + sizeof(DarrayHeader);

    void* new_buffer = malloc(new_size);
    memcpy(new_buffer, old_header, old_size);
    

    DarrayHeader* new_header = (DarrayHeader*)new_buffer;
    new_header->capacity = new_capacity;
    new_header->type_size = old_header->type_size;
    new_header->length = old_header->length;


    // free(old_header);

    return (u8*)new_header + sizeof(DarrayHeader);
}

