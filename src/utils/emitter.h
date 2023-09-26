#include "core.h"
#include "darray.h"






typedef struct Emitter {
    u8* buffer;
} Emitter;


inline Emitter emitter_init() {
    Emitter res;
    res.buffer = darray_init(u8, 2);

    return res;
}

inline void emit_formatted(Emitter* e, const char* format, ...) {
    va_list args;

    va_start(args, format);
    u64 length = vsnprintf(null, 0, format, args);
    va_end(args);

    darray_reserve(e->buffer, length + 1);

    va_start(args, format);
    vsnprintf((char*)e->buffer + darray_length(e->buffer), length + 1, format, args);
    va_end(args);

    DarrayHeader* hearder = (DarrayHeader*)DARRAY_HEADER(e->buffer);
    hearder->length += length;
}



