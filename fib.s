.set F0, main
.set F1, print
.global main
main:
pushq %rbp
movq %rsp, %rbp
.L0:
movl $1, %ecx
xor %esi, %esi
movl $1000, %r9d
movl %esi, %edx
movl %ecx, %r8d
.L1:
movl %r8d, %esi
add %edx, %esi
movl %esi, %ecx
sub %edx, %ecx
movl %esi, %edi
movl %ecx, %eax
cmpl %r9d, %esi
setb %sil
jb .L3
.L2:
xor %eax, %eax
leave
ret
.L3:
movl %eax, %edx
movl %edi, %r8d
jmp .L1
