org 0x7C00
bits 16

%define ENDL 0x0D, 0x0A

start:
	jmp main
;
; Prints a string to the screen.
;Params:
;	ds:si points to string
;
puts:
	; registers to be modified
	push si
	push ax

.loop:
	lodsb     ; loads next char in al
	or al, al ; checks if next char is null
	jz .done

	
	mov ah, 0x0e               ; call BIOS intrupt
	mov bh, 0
	int 0x10

	jmp .loop
	
.done:
	pop ax
	pop si
	ret 
main:
	; setup data segments
	mov ax, 0            ; cant write to ds/es directly
	mov ds, ax
	mov es, ax
	
	; setup stack
	mov ss, ax
	mov sp, 0x7C00      ;stack grows downwards from where we load it
	
	;print message
	mov si, msg_hello
	call puts
	
	hlt
.halt:
	jmp .halt


msg_hello: db 'hello world!', ENDL, 0


times 510-($-$$) db 0
dw 0AA55h
