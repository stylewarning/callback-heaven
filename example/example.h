/**
 * @file example.h
 *
 * An example group.
 */
#ifndef GROUP_EXAMPLE_HEADER_GUARD
#define GROUP_EXAMPLE_HEADER_GUARD

#include <stdint.h>
#include <stddef.h>


/** Return the integer a + b. */
int add(int a, int b);

/**
 * Print |n|! on standard output.
 *
 * The complete output will look something like
 *
 *     Factorial 5 = 120
 *
 * @param n the integer whose absolute value will be used to compute the factorial.
 */
void print_factorial(int n);


#endif /* GROUP_EXAMPLE_HEADER_GUARD */
