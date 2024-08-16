#include <stdio.h>
#include <dlfcn.h>

int main() {
    void *handle;
    void (*main_func)();
    float *buffer1, *buffer2, *result, *multiplier;
    char *error;

    // Open the shared library
    handle = dlopen("./output.so", RTLD_LAZY);
    if (!handle) {
        fprintf(stderr, "Error opening library: %s\n", dlerror());
        return 1;
    }

    // Clear any existing error
    dlerror();

    // Try to load the main function
    main_func = (void (*)())dlsym(handle, "main");
    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "Error loading main function: %s\n", error);
    } else {
        printf("Calling main function from library:\n");
        (*main_func)();
    }

    // Try to access the global variables
    buffer1 = (float *)dlsym(handle, "buffer1");
    buffer2 = (float *)dlsym(handle, "buffer2");
    result = (float *)dlsym(handle, "result");
    multiplier = (float *)dlsym(handle, "multiplier");

    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "Error loading global variables: %s\n", error);
    } else {
        if (buffer1 && buffer2 && result && multiplier) {
            printf("Global variables found:\n");
            printf("buffer1[0] = %f\n", buffer1[0]);
            printf("buffer2[0] = %f\n", buffer2[0]);
            printf("result[0] = %f\n", result[0]);
            printf("multiplier[0] = %f\n", multiplier[0]);
        } else {
            fprintf(stderr, "Warning: Some global variables not found\n");
        }
    }

    // Close the library
    dlclose(handle);
    return 0;
}
