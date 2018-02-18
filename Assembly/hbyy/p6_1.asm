; 1:
;assume cs:code
;code segment
	;dw 0123h,0456h,0789h,0abch,0defh,0fedh,0cbah,0987h
;start:	mov ax,0
		;mov ds,ax
		;mov bx,0

		;mov cx,8
	;s:	mov ax,[bx]
		;mov cs:[bx],ax
		;add bx,2
		;loop s

		;mov ax,4c00h
		;int 21h
;code ends
;end start

; 2: using stack
assume cs:code
code segment
	dw 1111h,1111h,1111h,1111h,1111h,1111h,1111h,1111h
	dw 0,0,0,0,0,0,0,0
	start:	mov ax,cs	; mov ss,cs(not correct)
			mov ss,ax
			mov ax,20h	;initiating the stack
			mov sp,ax
			mov ax,0	;set ds:[bx] to 0:0
			mov ds,ax
			mov bx,0

			mov cx,8
		s:	push [bx]
			pop cs:[bx]
			add bx,2
			loop s

			mov ax,4c00h
			int 21h
code ends
end start
