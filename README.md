http://www.felixcloutier.com/x86/
```
+ -> addl op1, op2
- -> subl op1, op2
* -> imull op1, op2
     'op2' has to be register
     
/, % ->
     'idiv' takes eax and divides it on an argument
   op2 / op1:
     mov op2, eax
     cltd          // puts appropriate bits to EDX register
     idiv op1
   /:
     mov eax, op2
   %:
     mov edx, op2

&& ->
   xor eax, eax
   andl op1, op1 // 'op1' has to be register
   setne al
   andl op2, op2 // 'op2' has to be register
   setne ah
   and ah, al
   xor ah, ah
   mov eax, op2
  
|| ->
   xor eax, eax
   orl  op1, op2
   setne al
   mov eax, op2

<, <=, ==, !=, >=, > ->
  xor eax, eax
  cmp op1, op2 // Result to EFLAGS 
  set(**) al
  mov eax, op2
```

```ocaml
type instructions =
...
| X86Set of string * string
```
