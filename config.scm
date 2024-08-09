(define-module (config)
  #:export (elf-header-size
            entry-point
            code-offset
            alignment
            dynamic-entry-size
            got-entry-size
            section-header-size
            program-header-size
            num-sections
            shstrtab-index
            elf-magic
            elf-class-64
            elf-data-lsb
            elf-version-current
            elf-osabi-linux
            elf-type-shared
            elf-machine-x86-64
            sht-null
            sht-progbits
            sht-symtab
            sht-strtab
            sht-rela
            sht-dynamic
            sht-nobits
            sht-dynsym
            shf-write
            shf-alloc
            shf-execinstr
            pt-load
            pt-dynamic
            pt-phdr
            pt-gnu-relro
            pf-x
            pf-w
            pf-r
            num-program-headers
            text-addr
            shstrtab-addr
            shstrtab-size))

(define elf-header-size 64)
(define entry-point #x1000)
(define code-offset #x1000)
(define alignment #x1000)
(define dynamic-entry-size 16)
(define got-entry-size 8)
(define section-header-size 64)
(define program-header-size 56)
(define num-sections 14)
(define shstrtab-index 13)

(define elf-magic #x464c457f)
(define elf-class-64 2)
(define elf-data-lsb 1)
(define elf-version-current 1)
(define elf-osabi-linux 3)
(define elf-type-shared 3)
(define elf-machine-x86-64 #x3e)

;; New constants from section-headers
(define sht-null 0)
(define sht-progbits 1)
(define sht-symtab 2)
(define sht-strtab 3)
(define sht-rela 4)
(define sht-dynamic 6)
(define sht-nobits 8)
(define sht-dynsym 11)

(define shf-write #x1)
(define shf-alloc #x2)
(define shf-execinstr #x4)

;; New constants from program-headers
(define pt-load 1)
(define pt-dynamic 2)
(define pt-phdr 6)
(define pt-gnu-relro #x6474e552)

(define pf-x #b001)
(define pf-w #b010)
(define pf-r #b100)

(define num-program-headers 5)

(define text-addr #x1000)
(define shstrtab-addr #x3f94)
(define shstrtab-size 108)
