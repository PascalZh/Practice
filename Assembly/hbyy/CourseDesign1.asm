assume cs:code
stack segment
	db 32 dup(0)
stack ends

data segment
	db '1975','1976','1977','1978','1979','1980','1981','1982'
	db '1983','1984','1985','1986','1987','1988','1989','1990'
	db '1991','1992','1993','1994','1995'

	dd 16,22,382,1356,2390,8000,16000,24485
	dd 50065,97479,140417,197514,345980,590832,803212,1183000
	dd 1843000,2759000,3753000,4649000,5937000

	dw 3,7,9,13,28,38,180,220
	dw 472,732,1003,1420,2238,2738,4023,5632
	dw 8224,11583,14430,15238,17832
data ends

table segment
	;4x10 per row
	db 21 dup ('1975      16        3         5         ')
table ends

code segment
start:	mov ax,stack
	mov ss,ax
	mov sp,32
	mov ax,table
	mov ds,ax

	mov ax,data
	mov es,ax

	mov cx,21
tloop:
	dec cx;use cx as an index, so it must start from 0
	mov al,40
	mul cl;get the address of table
	mov bx,ax
	mov al,4
	mul cl;get first part of data
	mov bp,ax

	push cx;year
	mov si,0
	mov cx,4
	year:
	mov al,es:[bp+si]
	mov [bx+si],al
	inc si
	loop year
	pop cx

	add bp,84;84=4x21, use bp to address second part
	mov ax,es:[bp]
	mov dx,es:[bp+2]
	mov si,bx
	add si,10
	call dtoc

	push cx
	mov ax,0
	mov al,2
	mul cl
	mov di,168
	add di,ax;use di to address third part
	mov ax,es:[di]
	mov dx,0
	mov si,bx
	add si,20
	call dtoc
	pop cx

	push cx
	mov ax,es:[bp]
	mov dx,es:[bp+2]
	mov cx,es:[di]
	call divdw
	mov si,bx
	add si,30
	call dtoc
	pop cx

	mov byte ptr [bx+39],0;add 0 to the last of the row
	;show the string
	inc cx
	push cx
	mov dh,cl
	mov dl,1
	mov cl,8
	mov si,bx
	call show_str
	pop cx

	loop tloop

	mov ax,4C00H
	int 21H

; Arguments: dh - row number
;            dl - column number
;            cl - color
;            ds:si - the first address of the string(C-style)
show_str:
        push si
        push dx
        push cx
        push ax
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
        pop cx
        pop dx
        pop si
	ret
; Arguments: ax,dx - dividend
;           cx    - divisor
; Return:   ax,dx - quotient
;           cx    - remainder
divdw:	push bx
	push ax

	mov ax,dx
	mov dx,0
	div cx
	mov bx,ax
	pop ax
	div cx
	mov cx,dx
	mov dx,bx
	pop bx
	ret
; Arguments: ax,dx - the digit number you want to turn to a string
;            ds:si - where you want to store the string
dtoc:	push ax
        push dx
        push si
        push cx
	push 0
dtoclp:	mov cx,10
	call divdw
	add cx,'0'
	push cx
	mov cx,ax
	jcxz dtoclpend
	jmp dtoclp
dtoclpend:

dtocpop:pop cx
	jcxz dtocpopend
	mov [si],cl
	inc si
	jmp dtocpop
dtocpopend:

	pop cx
        pop si
        pop dx
        pop ax
	ret
code ends
end start
