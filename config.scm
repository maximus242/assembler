(define-module (config)
  #:export (
    ;; ELF Header Constants
    elf-header-size
    code-offset
    alignment
    elf-magic
    elf-class-64
    elf-data-lsb
    elf-version-current
    elf-osabi-linux
    elf-type-shared
    elf-machine-x86-64

    ;; Section Header Constants
    section-header-size
    num-sections
    shstrtab-index
    sht-null
    sht-progbits
    sht-symtab
    sht-strtab
    sht-rela
    sht-dynamic
    sht-nobits
    sht-dynsym
    sht-hash
    shf-write
    shf-alloc
    shf-execinstr

    ;; Program Header Constants
    program-header-size
    num-program-headers
    pt-load
    pt-dynamic
    pt-phdr
    pt-gnu-relro
    pf-x
    pf-w
    pf-r

    ;; Other Constants
    dynamic-entry-size
    num-dynamic-entries
    got-entry-size
    text-addr
    shstrtab-addr
    shstrtab-size
    section-offset

    ;; Elf Layout Constants
    word-size
    double-word-size
    plt-entry-size
  ))

;; ELF Header Constants
(define elf-header-size 64)
(define code-offset #x1000)
(define alignment #x1000)
(define elf-magic #x464c457f)
(define elf-class-64 2)
(define elf-data-lsb 1)
(define elf-version-current 1)
(define elf-osabi-linux 3)
(define elf-type-shared 3)
(define elf-machine-x86-64 #x3e)

;; Section Header Constants
(define section-header-size 64)
(define num-sections 20)
(define shstrtab-index 13)
(define sht-null 0)
(define sht-progbits 1)
(define sht-symtab 2)
(define sht-strtab 3)
(define sht-rela 4)
(define sht-hash 5)
(define sht-dynamic 6)
(define sht-nobits 8)
(define sht-dynsym 11)
(define shf-write #x1)
(define shf-alloc #x2)
(define shf-execinstr #x4)

;; Program Header Constants
(define program-header-size 56)
(define num-program-headers 5)
(define pt-load 1)
(define pt-dynamic 2)
(define pt-phdr 6)
(define pt-gnu-relro #x6474e552)
(define pf-x #b001)
(define pf-w #b010)
(define pf-r #b100)

;; Other Constants
(define dynamic-entry-size 16)
(define num-dynamic-entries 16)
(define got-entry-size 8)
(define text-addr #x1000)
(define shstrtab-addr #x3f94)
(define shstrtab-size 108)
(define section-offset #x2000)

;; Elf Layout Constants
;; Constants for alignment and sizes
(define word-size 8)
(define double-word-size 16)
(define plt-entry-size 16)
