// simd_ops_wrapper.c
#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void* load_binary(const char* filename, size_t* size) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    *size = ftell(file);
    fseek(file, 0, SEEK_SET);

    void* buffer = malloc(*size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return NULL;
    }

    if (fread(buffer, 1, *size, file) != *size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return NULL;
    }

    fclose(file);
    return buffer;
}

typedef void (*simd_ops_func)(float*, float*, float*, float*);

simd_ops_func load_simd_ops() {
    size_t size;
    void* code = load_binary("simple_executable", &size);
    if (!code) {
        return NULL;
    }

    void* executable_memory = mmap(0, size, PROT_READ | PROT_WRITE | PROT_EXEC,
                                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (executable_memory == MAP_FAILED) {
        perror("mmap failed");
        free(code);
        return NULL;
    }

    memcpy(executable_memory, code, size);
    free(code);

    return (simd_ops_func)executable_memory;
}

void simd_ops_wrapper(float* buffer1, float* buffer2, float* result, float* multiplier) {
    static simd_ops_func simd_ops = NULL;
    if (!simd_ops) {
        simd_ops = load_simd_ops();
        if (!simd_ops) {
            fprintf(stderr, "Failed to load SIMD ops\n");
            return;
        }
    }
    simd_ops(buffer1, buffer2, result, multiplier);
}
