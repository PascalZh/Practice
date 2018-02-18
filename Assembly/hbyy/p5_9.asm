assume  cs:code
code segment
    mov ax,0ffffh
    mov ds,ax

    mov ax,4c00h
    int 21h
code ends
end
