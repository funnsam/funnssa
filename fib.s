.global main
main:
pushq %rbp
movq %rsp, %rbp
.L0:
movl $1, %esi
xor %edi, %edi
movl $1000, %ecx
movl %edi, %edx
movl %esi, %eax
.L1:
movl %eax, %r8d
movl %eax, %esi
add %edx, %esi
cmpl %ecx, %eax
setb %dil
jb .L3
.L2:
xor %eax, %eax
leave
ret
.L3:
movl %r8d, %edx
movl %esi, %eax
jmp .L1
