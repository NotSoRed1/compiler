#pragma once


#include "./core.h"
#include <string.h>

typedef struct String {
    u8* buffer;
    u64 length;
} String;


#define string_length(string) ((string).length)
#define string(buffer, length) (string_init((buffer), (length)))
#define string_cpy(buffer, length) (string_init_copy((buffer), (length)))
#define string_c(cstr) (string_init_cstr((cstr)))
#define string_fmt(format, ...) (string_init_formated(format, __VA_ARGS__))
#define string_cat(str1, str2) (string_concat((str1), (str2)))

inline String string_init(u8* buffer, u64 length) {
    String result;
    result.buffer = buffer;
    result.length = length;

    return result;
}

inline String string_init_copy(u8* buffer, u64 length) {
    String result;

    u8* new_buff = (u8*)malloc(length + 1);
    memcpy(new_buff, buffer, length);
    buffer[length] = 0;

    result.buffer = new_buff;
    result.length = length;

    return result;
}

inline void string_replace(String* self, u8 target, u8 to) {
    for (u32 i = 0; i < self->length; i++) {
        if (self->buffer[i] == target) {
            self->buffer[i] = to;
        }
    }
}

inline String string_init_cstr(const char* cstr) {
    String result;
    result.buffer = (u8*)cstr;
    result.length = strlen(cstr);

    return result;
}

inline String string_init_formated(const char* format, ...) {
    va_list args;

    va_start(args, format);
    u32 length = vsnprintf(null, 0, format, args);
    va_end(args);
    if (length <= 0) {
        return string_c("");
    }

    char* buffer = (char*)malloc(length + 1);

    va_start(args, format);
    vsnprintf(buffer, length + 1, format, args);
    va_end(args);

    return string((u8*)buffer, length);
}

inline String string_concat(String lhs, String rhs) {
    u8* buffer = (u8*)malloc(lhs.length + rhs.length + 1);
    if (lhs.length) {
        memcpy(buffer, lhs.buffer, lhs.length);
    }
    u8* offset_buffer = buffer + lhs.length;

    if (rhs.length) {
        memcpy(offset_buffer, rhs.buffer, rhs.length);
    }
    buffer[lhs.length + rhs.length] = 0;

    return string(buffer, lhs.length + rhs.length);
}



inline b8 string_eq(String lhs, String rhs) {
    if (lhs.length != rhs.length) {
        return false;
    } 

    for (u32 i = 0; i < lhs.length; i++) {
        if (lhs.buffer[i] != rhs.buffer[i]) {
            return false;
        }
    }

    return true;
}


inline b8 string_eq_cstr(String lhs, const char* rhs) {
    u64 rhs_length = strlen(rhs);
    if (lhs.length != rhs_length) {
        return false;
    } 

    for (u32 i = 0; i < lhs.length; i++) {
        if (lhs.buffer[i] != rhs[i]) {
            return false;
        }
    }

    return true;
}


inline b8 string_starts_with(String str, const char* prefix) {
    u32 length = strlen(prefix);
    if (length > str.length) {printf("huh\n"); return false; }

    for (u32 i = 0; i < length; i++) {
        if (str.buffer[i] != prefix[i]) {
            return false;
        }
    }

    return true;
}

inline b8 string_ends_with(String str, const char* prefix) {
    u32 length = strlen(prefix);
    if (length > str.length) {printf("huh\n"); return false; }

    u64 str_length = str.length - 1;
    while (length--) {
        if (str.buffer[str_length - length] != prefix[length]) {
            return false;
        }
    }

    return true;
}

inline void string_trim_start(String* str, u32 length) {
    str->length -= length;
    str->buffer += length;
}

inline u8 string_at(String string, u64 index) {
    return string.buffer[index >= string.length ? string.length - 1 : index];
}

