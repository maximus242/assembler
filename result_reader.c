#include <stdio.h>
#include <dlfcn.h>

typedef struct {
    float values[8];
} Result;

int main() {
    void *handle;
    void (*main_func)();
    Result *result;

    // Load the shared object
    handle = dlopen("./output.so", RTLD_LAZY);
    if (!handle) {
        fprintf(stderr, "Error loading shared object: %s\n", dlerror());
        return 1;
    }

    // Get the address of the main function
    *(void **)(&main_func) = dlsym(handle, "main");
    if (!main_func) {
        fprintf(stderr, "Error finding main function: %s\n", dlerror());
        dlclose(handle);
        return 1;
    }

    // Get the address of the result variable
    result = (Result *)dlsym(handle, "result");
    if (!result) {
        fprintf(stderr, "Error finding result variable: %s\n", dlerror());
        dlclose(handle);
        return 1;
    }

    // Call the main function
    main_func();

    // Print the result values
    for (int i = 0; i < 8; i++) {
        printf("result[%d] = %f\n", i, result->values[i]);
    }

    // Close the shared object
    dlclose(handle);
    return 0;
}
