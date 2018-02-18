;assume cs:codeseg,ds:dataseg,ss:stackseg
;dataseg segment
    ;dw 0123h,0456h,0789h,0876h,0abch,0defh,0fedh,0cbah
;dataseg ends
;stackseg segment
    ;dw 0,0,0,0,0,0,0,0
;stackseg ends

;codeseg segment
;start:  mov ax,stackseg
        ;mov ss,ax
        ;mov sp,16

        ;mov ax,dataseg
        ;mov ds,ax

        ;push ds:[0]
                ;push ds:[2]
        ;pop ds:[2]
        ;pop ds:[0]

        ;mov ax,4c00h
        ;int 21h

;codeseg ends

;end start

; show_str test program:
assume cs:code,ds:data,ss:stack
data segment
	db 'a','b','c','d','e','f','g',0
	db 8 dup(0)
data ends
stack segment
	dw 8 dup(0)
	dw 8 dup(0)
stack ends
code segment

show_str:push ax
	push es;B800H
	push bx;row
	push di;col

	mov al,dl; initiating di
	dec al
	mov ah,2;dlx2,because it costs 2 bytes to show a character
	mul ah
	mov di,ax

	mov ax,0B800H; initiating  es
	mov es,ax

	mov al,0A0H; initiating bx
	dec dh
	mul dh
	mov bx,ax

	mov dl,cl; use dl to store color

	mov ch,0
	p:mov cl,[si]
	jcxz end_str
	mov al,[si]
	mov es:[bx+di],al
	mov es:[bx+di+1],dl
	inc si
	inc di
	inc di
	jmp short p
	end_str:pop di
	pop bx
	pop es
	pop ax
	ret

start:	mov ax,data
	mov ds,ax
	mov ax,stack
	mov ss,ax
	mov ax,20H
	mov sp,ax

	mov dh,2
	mov dl,2
	mov cl,8
	mov si,0
	call show_str
	mov ax,1111H
	push ax
	push ax
	push ax
	push ax
	push ax

	mov ax,4C00H
	int 21H
code ends
end start
