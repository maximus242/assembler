#include <stdio.h>
#include <dlfcn.h>

int main() {
    // Load the shared object
    void *handle = dlopen("./output.so", RTLD_LAZY);
    if (!handle) {
        fprintf(stderr, "Error loading shared object: %s\n", dlerror());
        return 1;
    }

    // Get the function from the shared object
    void (*perform_operations)() = dlsym(handle, "perform_operations");
    if (!perform_operations) {
        fprintf(stderr, "Error finding symbol: %s\n", dlerror());
        dlclose(handle);
        return 1;
    }

    // Call the function
    perform_operations();

    // Close the shared object
    dlclose(handle);

    return 0;
}
