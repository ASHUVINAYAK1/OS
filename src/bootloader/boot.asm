org 0x7C00
bits 16

%define ENDL 0x0D, 0x0A

; 
; FAT12 Header
;
jmp short start
nop
bdb_oem:                     db "MSWIN4.1"           ; 8 bytes
bdb_bytes_per_sector:        dw 512                 ; 2 bytes
bdb_sector_per_cluster:      db 1                   ; 1 byte
bdb_reserved_sectors:        dw 1                   ; 2 bytes
bdb_fat_count:               db 2                   ; 1 byte
bdb_dir_entries_count:       dw 0E0h                ; 2 bytes
bdb_total_sectors:           dw 2880                ; 2880 * 512 = 1.44MB
bdb_media_descriptor_type:   db 0F0h                ; F0 = 3.5" 1.44MB floppy
bdb_sectors_per_fat:         dw 9                   ; 9 sectors per FAT
bdb_sectors_per_track:       dw 18                  ; 18 sectors per track
bdb_heads:                   dw 2                   ; 2 heads
bdb_hidden_sectors:          dd 0                   ; 4 bytes
bdb_total_sectors_large:     dd 0                   ; 4 bytes

; extended boot record
ebr_drive_number:            db 0                   ; 0x00 floppy, 0x80 hard drive
                             db 0                   ; reservved
ebr_signature:               db 29h                 ; 0x29
ebr_volume_id:               db 12h, 34h, 56h, 78h  ; serial number
ebr_volume_label:            db "NANOBYTE OS	"   ; 11 bytes
ebr_system_id:               db "FAT12   "          ; 8 bytes

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
	
	; read something from the floppy
	; BIOS should set dl to the boot drive number
	mov [ebr_drive_number], dl

	mov ax, 1           ; LBA= 1, second sector from the disk
	mov cl, 1           ; read 1 sector
	mov bx, 0x7E00      ; data after the bootloader
	call disk_read


	
	;print message
	mov si, msg_hello
	call puts
	
	cli
	hlt


;
; Error handling
;
floppy_error:
	mov si, msg_read_failed
	call puts
	jmp wait_key_and_reeboot

wait_key_and_reeboot:
	mov ah, 0
	int 16h                                      ; wait for key press
	jmp 0FFFFh:0                                 ; reboot
.halt:
	cli                                          ; disable interrupts
	hlt

;
; Floppy routines
;

;
; converts an LBA address to CHS
; Parameters:
; 	ax: LBA address
;  Returns:
; 	dh: head
;   cx [bits 0-5]: sector number
;   cx [bits 6-15]: cylinder number
;
lba_to_chs:

	push ax
	push dx


	xor dx, dx                            ; dx = 0
	div word [bdb_sectors_per_track]      ; ax = LBA / SPT
	                                      ; dx = LBA % SPT
	inc dx                                ; dx = (LBA % SPT) + 1 (sector number)
	mov cx, dx							  ; cx = sector number

	xor dx, dx                            ; dx = 0
	div word [bdb_heads]                   ; ax = LBA / heads (cylinder number)

	mov dh, dl                            ; dh = head
	mov ch, al                            ; ch = cylinder low 8 bits
	shl ah, 6                             ; shift ah 6 bits to the left
	or ch, ah                             ; ch = cylinder high 8 bits

	pop ax
	mov dl, al 						  ; restore dl
	pop ax
	ret




;
; Read a sector from the floppy
; Parameters:
; 	es:bx: address to store the sector
;   ax: LBA address
;   dl: drive number
;   cl: number of sectors to read (up to 128)
;

disk_read:
    push ax
    push bx
    push cx
    push dx
    push di

    push cx                 ; save sectors to read
    call lba_to_chs         ; convert LBA to CHS
    pop cx                  ; restore sectors to read

    mov ah, 02h             ; BIOS read sectors function
    mov di, 3               ; retry count
.retry:
    pusha
    stc                     ; set carry flag
    int 13h                 ; BIOS disk read
    jnc .done               ; jump if no error

    popa
    call disk_reset         ; reset disk if error

    dec di
    test di, di
    jnz .retry              ; retry if retries left
.fail:
	; all attempts are exhausted
	jmp floppy_error


.done:
	popa

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	ret

; 
; Reset the floppy disk controller
; Parameters:
; dl: drive number
;
disk_reset:
	pusha
	mov ah, 0
	stc
	int 13h
	jc floppy_error
	popa
	ret

msg_hello: db 'hello world!', ENDL, 0
msg_read_failed: db 'Read from disk failed', ENDL, 0

times 510-($-$$) db 0
dw 0AA55h
