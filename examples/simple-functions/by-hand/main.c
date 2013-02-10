#include <stdio.h>
#include <stdlib.h>

extern long __dl_main();

void *dlib_malloc(long n) {
    return malloc(n);
}

void dlib_free(void *p) {
    free(p);
}

long dlib_arr_get(void *ptr, long idx, long bsize) {
    switch (bsize) {
    case 8: long *p = (long *)ptr;
            return p[idx];
    case 4: int *p = (int *)ptr;
            return p[idx];
    case 1: unsigned char *p = (unsigned char *)ptr;
            return p[idx];
    }
    fprintf(stderr, "warning: bsize is not supported: %ld\n", bsize);
    return 0;
}

void dlib_arr_set(void *ptr, long d, long idx, long bsize) {
    if (bsize == 8) {
        ((long *)ptr)[idx] = d;
    } else if (bsize == 4) {
        ((int *)ptr)[idx] = (int)d;
    } else if (bsize == 1) {
        ((unsigned char *)ptr)[idx] = (unsigned char)d;
    } else {
        fprintf(stderr, "warning: bsize is not supported: %ld\n", bsize);
    }
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
