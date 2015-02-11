#include "test.h"

#define FUNCTION_INDEX_GROUP_TEST_SIZE 2

static void **function_index_group_test;

void set_function_index_group_test(void **functions) {
    function_index_group_test = functions;
}


int add(int a, int b) {
    return ((int (*)(int, int))(function_index_group_test[0]))(a, b);
}


void print_factorial(int n) {
    ((void (*)(int))(function_index_group_test[1]))(n);
}

