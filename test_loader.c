#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

int main() {
    void *handle;
    float *buffer1, *buffer2, *result, *multiplier;
    char *error;

    // Open the shared object file
    handle = dlopen("./output.so", RTLD_NOW | RTLD_GLOBAL);
    if (!handle) {
        fprintf(stderr, "dlopen failed: %s\n", dlerror());
        return 1;
    }

    // Clear any existing error
    dlerror();

    // Try to load the symbols
    *(void **) (&buffer1) = dlsym(handle, "buffer1");
    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "dlsym error for buffer1: %s\n", error);
    } else {
        printf("buffer1 loaded successfully at %p\n", (void*)buffer1);
    }

    *(void **) (&buffer2) = dlsym(handle, "buffer2");
    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "dlsym error for buffer2: %s\n", error);
    } else {
        printf("buffer2 loaded successfully at %p\n", (void*)buffer2);
    }

    *(void **) (&result) = dlsym(handle, "result");
    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "dlsym error for result: %s\n", error);
    } else {
        printf("result loaded successfully at %p\n", (void*)result);
    }

    *(void **) (&multiplier) = dlsym(handle, "multiplier");
    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "dlsym error for multiplier: %s\n", error);
    } else {
        printf("multiplier loaded successfully at %p\n", (void*)multiplier);
    }

    // If we successfully loaded all symbols, try to access them
    if (buffer1 && buffer2 && result && multiplier) {
        printf("First value of buffer1: %f\n", buffer1[0]);
        printf("First value of buffer2: %f\n", buffer2[0]);
        printf("First value of result: %f\n", result[0]);
        printf("First value of multiplier: %f\n", multiplier[0]);
    }

    // Close the shared object
    dlclose(handle);
    return 0;
}
