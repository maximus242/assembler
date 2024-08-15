#include <stdio.h>
#include <dlfcn.h>

int main() {
    void *handle;
    int (*library_main)();
    char *error;

    handle = dlopen("./output.so", RTLD_LAZY);
    if (!handle) {
        fprintf(stderr, "Error loading library: %s\n", dlerror());
        return 1;
    }

    dlerror(); // Clear any existing error

    library_main = (int (*)()) dlsym(handle, "library_main");
    if ((error = dlerror()) != NULL) {
        fprintf(stderr, "Error finding library_main function: %s\n", error);
        dlclose(handle);
        return 1;
    }

    printf("Library main function symbol loaded\n");
    printf("Attempting to call library main function\n");

    int result = (*library_main)();

    printf("Loader: The library_main function returned: %d\n", result);

    dlclose(handle);
    return 0;
}
