#include <stdio.h>
#include <dlfcn.h>

int main() {
    // Attempt to open the shared library
    void *handle = dlopen("./output.so", RTLD_LAZY);
    
    if (!handle) {
        // If dlopen fails, print the error message
        fprintf(stderr, "Error opening library: %s\n", dlerror());
        return 1;
    }

    // If dlopen is successful, print a success message
    printf("Library opened successfully.\n");

    // Close the library after use
    dlclose(handle);

    return 0;
}
