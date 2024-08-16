#include <libguile.h>
#include <dlfcn.h>

static void* lib_handle;
static float (*multiply_vectors)(float*, float*, float*, float*, int);

static SCM
float multiply_vectors(float* buffer1, float* buffer2, float* result, float* multiplier, int length) {
    for (int i = 0; i < length; i++) {
        result[i] = buffer1[i] * buffer2[i] * (*multiplier);
    }
    // Return some value, perhaps the sum of the result array
    float sum = 0;
    for (int i = 0; i < length; i++) {
        sum += result[i];
    }
    return sum;
}

static void
init_output(void* data)
{
    lib_handle = dlopen("/tmp/load_buffer.so", RTLD_LAZY);
    if (!lib_handle) {
        scm_misc_error("init_output", "Failed to load load_buffer.so: ~A", 
                       scm_list_1(scm_from_locale_string(dlerror())));
    }

    multiply_vectors = dlsym(lib_handle, "multiply_vectors");
    if (!multiply_vectors) {
        scm_misc_error("init_output", "Failed to find multiply_vectors function: ~A", 
                       scm_list_1(scm_from_locale_string(dlerror())));
    }

    scm_c_define_gsubr("multiply-vectors", 4, 0, 0, scheme_multiply_vectors);
}

void
scm_init_output(void)
{
    scm_c_define_module("output", init_output, NULL);
}
