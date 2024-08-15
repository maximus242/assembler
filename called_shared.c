#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

typedef int (*library_main_func)();

int main() {
    // Load the shared library
    void *handle = dlopen("./output.so", RTLD_LAZY);
    if (!handle) {
        fprintf(stderr, "Failed to load library: %s\n", dlerror());
        exit(EXIT_FAILURE);
    }

    // Get a pointer to the library_main function
    library_main_func library_main = (library_main_func)dlsym(handle, "library_main");
    if (!library_main) {
        fprintf(stderr, "Failed to locate library_main: %s\n", dlerror());
        dlclose(handle);
        exit(EXIT_FAILURE);
    }

    // Call the library_main function
    int result = library_main();

    // Print the result
    printf("Result from library_main: %d\n", result);

    // Clean up
    dlclose(handle);
    return 0;
}
