#include <stdio.h>
#include <stdlib.h>

extern long __dl_main();

void *dlib_malloc(long n) {
    return malloc(n);
}

void dlib_free(void *p) {
    free(p);
}

void dlib_print_num(long n) {
    printf("%ld", n);
}

void dlib_print_char(char c) {
    printf("%c", c);
}

void dlib_print_str(char *s) {
    printf("%s", s);
}

int main(int argc, char *argv[])
{
    return (int)__dl_main();
}
