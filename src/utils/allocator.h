
#pragma once 
#include "stdint.h"
#include "stdlib.h"

typedef struct AllocatorNode {
    struct AllocatorNode* next;

    uint8_t*        buffer;
    uint64_t        cursor;
    uint64_t        capacity;
} AllocatorNode ;



typedef struct {
    AllocatorNode* head;
    uint64_t           total_cap;
} Allocator;


uint8_t allocator_init(Allocator* allocator);
void* allocator_alloc(Allocator* allocator, uint64_t size);
void allocator_free(Allocator* allocator);


uint8_t global_allocator_init();
void* global_allocator_alloc(uint64_t size);
void global_allocator_free();

