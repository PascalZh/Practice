assume cs:codesg
datasg segment
    db "Beginner's All-purpose Symbolic Instruction Code.",0
datasg ends
codesg segment
start: mov ax,datasg
       mov ds,ax
       mov si,0
       call letterc
       
       mov ax,4c00h
       int 21h
       
letterc:
       mov ch,0
s:     mov cl,[si]
       jcxz endlet
       cmp cl,97
       jb s0
       cmp cl,122
       ja s0
       sub byte ptr [si],32
s0:    inc si
       jmp short s
endlet:ret

codesg ends
end start
