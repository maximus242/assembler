#include <stdio.h>
#include <dlfcn.h>

int main() {
    void *handle;
    double *buffer1;
    char *error;

    // Open the shared object file
    handle = dlopen("./output.so", RTLD_LAZY | RTLD_GLOBAL);
    if (!handle) {
        fprintf(stderr, "Error loading library: %s\n", dlerror());
        return 1;
    }

    // Clear any existing error
    dlerror();

    // Try to get the address of the buffer1 symbol
    buffer1 = (double *) dlsym(handle, "buffer1");
    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "Error getting symbol: %s\n", error);
        dlclose(handle);
        return 1;
    }

    // If we get here, we successfully got the symbol
    printf("Successfully loaded buffer1. First value: %f\n", buffer1[0]);

    // Close the library
    dlclose(handle);
    return 0;
}
