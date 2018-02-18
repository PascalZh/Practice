assume cs:codesg
stacksg segment
        db 128 dup (0)
stacksg ends
codesg segment
start:  mov ax,stacksg
        mov ss,ax
        mov sp,128

        mov ax,0
        mov es,ax
        mov di,400H

        mov ax,cs
        mov ds,ax
        mov si,offset do7c

        mov cx,offset do7cend-offset do7c
        cld
        rep movsb

        mov ax,0
        mov ds,ax
        cli
        mov word ptr [7CH*4],400H
        mov word ptr [7CH*4+2],0
        sti
        ; setup finished, write your code here:
        mov bx,0b800h
        mov es,bx
        mov byte ptr es:[160*1+2],'a'

        mov ah,3
        int 7ch

        mov ah,2
        mov al,2
        int 7ch

        mov ah,1
        mov al,3
        int 7ch

        mov ah,0
        int 7ch

        mov ax,4c00h
        int 21h


do7c:   jmp short do7cstart
        subprog dw sub1,sub2,sub3,sub4
do7cstart:
        push bx

        cmp ah,3
        ja do7cret
        mov bl,ah
        mov bh,0
        add bx,bx
        call word ptr subprog[bx]
do7cret:pop bx
        iret

sub1:   push bx
        push cx
        push es

        mov bx,0b800h
        mov es,bx
        mov bx,0
        mov cx,2000
 sub1s: mov byte ptr es:[bx],' '
        add bx,2
        loop sub1s

        pop es
        pop cx
        pop bx
        ret

sub2:   push bx
        push cx
        push es

        mov bx,0b800h
        mov es,bx
        mov bx,1
        mov cx,2000
 sub2s: and byte ptr es:[bx],11111000b
        or es:[bx],al
        add bx,2
        loop sub2s

        pop es
        pop cx
        pop bx
        ret

sub3:   push bx
        push cx
        push es

        mov cl,4
        shl al,cl
        mov bx,0b800h
        mov es,bx
        mov bx,1
        mov cx,2000
 sub3s: and byte ptr es:[bx],10001111b
        or es:[bx],al
        add bx,2
        loop sub3s

        pop es
        pop cx
        pop bx
        ret

sub4:   push cx
        push si
        push di
        push es
        push ds

        mov si,0b800h
        mov es,si
        mov ds,si
        mov si,160
        mov di,0
        cld
        mov cx,24

 sub4s: push cx
        mov cx,160
        rep movsb
        pop cx
        loop sub4s
        
        mov cx,80
        mov si,0
 sub4s1:mov byte ptr [160*24+si],' '
        add si,2
        loop sub4s1

        pop ds
        pop es
        pop di
        pop si
        pop cx
        ret

do7cend:nop

codesg ends
end start
