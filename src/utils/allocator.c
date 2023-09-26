#include "allocator.h"

#define round_up_to_alignment(value, align)((value) + ((align) - 1) & ~((align) - 1));
#define ALLOCATOR_PAGE_SIZE 4 * 1024

Allocator* global_allocator = NULL;


uint8_t allocator_init(Allocator* allocator) {
    allocator->head = (AllocatorNode*)malloc(sizeof(AllocatorNode));
    if (!allocator->head) {
        return 0;
    }

    allocator->head->buffer = (uint8_t*)malloc(ALLOCATOR_PAGE_SIZE);
    if (!allocator->head->buffer) {
        return 0;
    }

    allocator->head->cursor = 0;
    allocator->head->capacity = ALLOCATOR_PAGE_SIZE;
    allocator->total_cap = global_allocator->head->capacity;
    allocator->head->next = NULL;

    return 1;
}


void* allocator_alloc(Allocator* allocator, uint64_t size) {
    if (allocator->head->cursor + size >= allocator->head->capacity) {
        uint64_t new_cap = ALLOCATOR_PAGE_SIZE;
        if (new_cap < size) {
            new_cap = size;
        }
        AllocatorNode* temp = allocator->head;
        allocator->head = (AllocatorNode*)malloc(sizeof(AllocatorNode));
        allocator->head->buffer = (uint8_t*)malloc(new_cap);
        allocator->head->cursor = 0;
        allocator->head->capacity = new_cap;
        allocator->head->next = temp;

        allocator->total_cap += new_cap;
    }

    void* result = &allocator->head->buffer[allocator->head->cursor];
    allocator->head->cursor += size;

    return result;
}


void allocator_free(Allocator* allocator) {
    AllocatorNode* it = allocator->head;

    while (it) {
        free(it->buffer);
        AllocatorNode* temp = it;
        it = it->next;
        free(temp);
    }

    allocator->total_cap = 0;
}



uint8_t global_allocator_init() {
    if (!global_allocator) {
        global_allocator = (Allocator*)malloc(sizeof(Allocator));
        return allocator_init(global_allocator);
    }

    return 0;
}


void* global_allocator_alloc(uint64_t size) {
    return allocator_alloc(global_allocator, size);
}


void global_allocator_free() {
    allocator_free(global_allocator);
}


