#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern long __dl_main();

void *dlib_malloc(long n) {
    char *buf = malloc(n);
    memset(buf, 0, n);
    return buf;
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

void dlib_print_bool(char b) {
    if (b) {
        printf("true");
        return;
    }
    printf("false");
}

// this should not be used for now because our string(or byte[]) is not NUL-ended.
// void dlib_print_str(char *s) {
//     printf("%s", s);
// }
void dlib_print_str(char *s, int len) {
    int i;
    for (i = 0; i < len; i++) {
        putchar(s[i]);
    }
}

long dlib_arr_get(void *buf, long idx, long bsize) {
    if (bsize == 1) {return ((char *)buf)[idx];}
    if (bsize == 2) {return ((short *)buf)[idx];}
    if (bsize == 4) {return ((int *)buf)[idx];}
    return ((long *)buf)[idx]; // eh...
}

void dlib_arr_set(void *buf, long v, long idx, long bsize) {
    if (bsize == 1) {((char *)buf)[idx] = v;}
    if (bsize == 2) {((short *)buf)[idx] = v;}
    if (bsize == 4) {((int *)buf)[idx] = v;}
    ((long *)buf)[idx] = v; // eh....
}

void dlib_memcpy(void *dst, const void *src, long s) {
    memcpy(dst, src, s);
}

int main(int argc, char *argv[])
{
    return (int)__dl_main();
}
