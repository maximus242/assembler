#include <stdio.h>

static int recursion_count = 0;
#define MAX_RECURSION 3

static int recursive_function(int count) {
    printf("This is the recursive function (recursion level: %d)\n", count);
    
    if (count >= MAX_RECURSION) {
        printf("Maximum recursion reached. Returning final value.\n");
        return 42 * (count + 1);
    }
    
    int result = recursive_function(count + 1);
    
    printf("Returning from recursion level %d with result: %d\n", count, result);
    
    return result;
}

__attribute__((visibility("default")))
int library_main() {
    printf("This is the library_main function\n");
    return recursive_function(0);
}

// Alias for library_main
__attribute__((visibility("default")))
int main() {
    return library_main();
}
