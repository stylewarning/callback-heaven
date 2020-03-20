#include "example.h"

#define FUNCTION_INDEX_GROUP_EXAMPLE_SIZE 2

static void **function_index_group_example;

void set_function_index_group_example(void **functions) {
    function_index_group_example = functions;
}


int add(int a, int b) {
    int ret;
    ret = ((int (*)(int, int))(function_index_group_example[0]))(a, b);
    return ret;
}


void print_factorial(int n) {
    ((void (*)(int))(function_index_group_example[1]))(n);
}

