.global foo

.text
foo:
    mov $0, %eax # move into 32 bit a register, this is 0 extended to the full width of a
    retq         # return
