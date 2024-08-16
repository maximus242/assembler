#include <libguile.h>
#include <stdio.h>

// Declare the buffer1 variable from the shared object
extern int buffer1;

SCM get_buffer1() {
    return scm_from_int(buffer1);
}

void init_load_buffer(void) {
    scm_c_define_gsubr("get-buffer1", 0, 0, 0, get_buffer1);
}

void do_init(void *unused) {
    scm_c_define_gsubr("get-buffer1", 0, 0, 0, get_buffer1);
    scm_c_export("get-buffer1", NULL);
}

void init_load_buffer_module(void) {
    scm_c_define_module("load buffer", do_init, NULL);
}
