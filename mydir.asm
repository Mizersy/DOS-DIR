data segment
    org 80h
    psp     dw 0ffh dup(?)
    ddd     db 'diretory:$'
    head    db 'name           size      type      date       time    ',0dh,0ah,24h
    space   db '        ',24h
    dir     db 'xxx       <dir>   ',24h
    fn    db 200h dup(?)
    dta     db 128 dup(?)
    msg     db 'press any key to continue...',0dh,0ah,24h
    newline db 0dh,0ah,24h
    count   db ?
    show    db 0
    base_10     dw      10
    used_32bits_high    dw      0
    used_32bits_low     dw      0
    var_32bits_high     dw      0
    var_32bits_low     dw      0
    quotidient_32bits_high      dw      0
    quotidient_32bits_low       dw      0
    negate_mask         equ      0FFFFh
    lowest_signed_32bits_high        dw     8000h
    lowest_signed_32bits_low         dw     0000h
    lowest_signed_32bits_string      db     '-2147483648$'
    qhigh       dw      0
    rhigh       dw      0
    qlow        dw      0
    rlow        dw      0
    qhigh_redundant     dw      0
    rhigh_redundant     dw      0
    q_0         dw      0
    qhigh0      equ     0h
    rhigh0      equ     0h
    qhigh1      equ     1999h
    rhigh1      equ     6h
    qhigh2      equ     3333h
    rhigh2      equ     2h
    qhigh3      equ     4CCCh
    rhigh3      equ     8h
    qhigh4      equ     6666h
    rhigh4      equ     4h
    qhigh5      equ     8000h
    rhigh5      equ     0h
    qhigh6      equ     9999h
    rhigh6      equ     6h
    qhigh7      equ     0B333h
    rhigh7      equ     2h
    qhigh8      equ     0CCCCh
    rhigh8      equ     8h
    qhigh9      equ     0E666h
    rhigh9      equ     4h
    filenum     db      0
    dirnum      db      0
    filespace   db      ' File(s)    $'
    dirspace    db      ' Dir(s)     $'
    bytes       db      ' Bytes.',0dh,0ah,24h
    bytesfree   db      ' Bytes free.',0dh,0ah,24h
data ends

stack1  segment   stack
       dw  0ffh  dup(0)
stack1  ends

main    segment
        assume cs:main,ds:data,ss:stack1
        org 100h
cmdln equ byte ptr es:[82h] ;command line data

;-------------------------------------------------------------------------------
p1      proc near
        push ds
        mov ax,data
        mov ds,ax
        pop psp
        mov es,psp
        mov ax,stack1
        mov ss,ax
        mov cl,byte ptr es:[80h]
        cmp cl,0               ;command line?
        jne l4                 ;yes
no_dir:
        mov ah,19h             ;no
        int 21h
        add al,'a'
        mov byte ptr[fn],al
        mov word ptr[fn+1],5c3ah
        mov ah,47h             ;current directory but no driver,so have to
        lea si,fn[3]           ;use 19h to add myself
        sub al,'a'
        mov dl,al
        int 21h

        lea di,fn[3]      ;to point di to the end of the diretory

l7:
        mov al,[di]
        cmp al,0
        je l6
        inc di
        jmp l7

l4:
        lea si,cmdln              
        lea di,fn
        cld
        xor ch,ch
        dec cl
        ;rep movsb

        lea di,fn
        lea bx,cmdln
        lea bp,fn

repeat:

        mov al,es:[bx]
        mov ds:[bp],al
        inc bx
        inc bp
        inc di
        dec cl
        jnz repeat

l6:
        mov cl,[di-1]                ;end by '\' ?
        cmp cl,92                   ;92-->'\'
        je l8                        ;yes
        ;mov byte ptr[di],92         ;no
        ;inc di

        cmp byte ptr[di-1],'W'
        jne notW
        mov al,1
        mov [show],al
notW:
        cmp byte ptr[di-1],92           ;'\'
        je l8
        dec di
        jz no_dir
        jmp notW

l8:
        mov word ptr[di],0a0dh       ;end by $ for printing
        mov byte ptr[di+2],24h
        mov ah,09h                   ;print "directory:*:\*\*\"
        ;lea dx,ddd
        mov dx,offset ddd
        int 21h
        mov ah,09h
        lea dx,fn
        int 21h
        mov ah,09h
        mov al,[show]
        cmp al,1
        je  w_head
        lea dx,head
        int 21h
w_head:
        

        mov word ptr[di],2e2ah        ;add '*.* ' at the end
        mov word ptr[di+2],002ah

l5:
        lea dx,dta
        mov ah,1ah
        int 21h
        mov ah,4eh                   ;first fdt matched
        mov cx,16h                   ;all file
        lea dx,fn
        int 21h
        
        call outp              ;print first file imformation
        mov cl,1
        mov count,cl
        ;-------------
        ;mov ah,4ch
        ;int 21h 
        ;-----------
l2:
        mov ah,4fh                   ;next fdt
        int 21h
        jc l0                        ;end when no file any more

        call outp              ;print next file imformation
        mov cl,count
        inc cl
        mov count,cl

        cmp cl,14h
        ja pause
pause_end:
        jmp l2

pause:
        mov dl,[show]
        cmp dl,1
        je pause_end
        mov   ah,9h;move 9 into ah (9 is the interrupt for output)
        lea   dx,msg;move the message into dx
        int   21h  ;call dos function
        mov   ah,7h;move 7 into ah (7 is the interrupt for input)
        mov   dx,00h;get the actual input
        int   21h  ;dos interrupt
        mov cl,0
        mov count,cl
        jmp pause_end

l0:     ;int 20h                       ;end
        mov dl,[show]
        cmp dl,0
        je not_newline

        mov ah,2                     ;new line
        mov dl,0dh
        int 21h
        mov ah,2
        mov dl,0ah
        int 21h

not_newline:
        lea dx,[space]
        mov ah,09h
        int 21h
        mov al,[filenum]
        xor ah,ah
        mov [var_32bits_low],ax
        xor al,al
        mov [var_32bits_high],ax
        call showspace

        lea dx,filespace
        mov ah,09h
        int 21h

        mov ax,[used_32bits_high]
        mov [var_32bits_high],ax
        mov ax,[used_32bits_low]
        mov [var_32bits_low],ax
        call showspace
        lea dx,bytes
        mov ah,09h
        int 21h


        lea dx,[space]
        mov ah,09h
        int 21h

        mov al,[dirnum]
        xor ah,ah
        mov [var_32bits_low],ax
        xor al,al
        mov [var_32bits_high],ax
        call showspace

        lea dx,[dirspace]
        mov ah,09h
        int 21h
        ;-----------------------get free space
        mov dl,fn[0]

        sub dl,'a'
        mov ah,36h
        int 21h

        mul bx
        mov bx,cx
        xor cx,cx

        call mymul32

        mov var_32bits_high,cx
        mov [var_32bits_low],bx
        call showspace

        lea dx,bytesfree
        mov ah,09h
        int 21h

        mov ah,4ch
        int 21h


p1      endp

pspace proc near
        push dx
        push ax
        mov dl, ' '
        mov ah, 2h
        int 21h
        pop ax
        pop dx
pspace endp

printax proc near
        cmp ax,0
        je pret
        push bx
        push cx
        push ax
        push dx

        xor cx,cx
        ;jmp lable1
b11:    xor dx,dx
lable1: mov si,10
        div si

        push dx
        inc cx
        cmp ax,0
        jne b11

b22:     pop dx
        add dl,30h
        mov ah,2
        int 21h
        loop b22

        pop dx
        pop ax
        pop cx
        pop bx
pret:        ret
printax endp

printax_hex proc near
        mov cx,4        ; print 4 hex digits (= 16 bits)
.print_digit:
        rol ax,4   ; move the currently left-most digit into the least significant 4 bits
        mov dl,al
        and dl,0Fh  ; isolate the hex digit we want to print
        add dl,'0'  ; and convert it into a character..
        cmp dl,'9'  ; ...
        jbe .ok     ; ...
        add dl,7    ; ... (for 'A'..'F')
.ok:            ; ...
        push ax    ; save EAX on the stack temporarily
        mov ah,2    ; INT 21H / AH=2: write character to stdout
        int 21h
        pop ax     ; restore EAX
        loop .print_digit
        ret
printax_hex endp

;----------------------------print the file imfomation--------------------
outp    proc near
        push bx
        push dx
        push ax
        push cx
        mov cx,16
        lea bx,dta[1eh]       ;print name
o0:
        mov dl,[bx]
        cmp dl,0
        je o1
        mov ah,2
        int 21h
        dec cx
        inc bx
        jmp o0


o1:
        mov ah,02h            ;print space between name and size
        mov dl,20h
        int 21h
        loop o1
        call b2d              ;print size and dir type
        ;----judge is W
        mov dl,[show]
        cmp dl,1
        je w_file
        call datm             ;print date and time


        mov ah,2                     ;new line
        mov dl,0dh
        int 21h
        mov ah,2
        mov dl,0ah
        int 21h
w_file:
        pop cx
        pop ax
        pop dx
        pop bx
        ret
outp    endp
;------------------------------print size-------------------------------------
b2d     proc near
        push cx
        push ax
        push dx
        push si


        xor cx,cx
        lea si,dta[1ah]
        mov ax,[si]
        call add_used_space
        call add_filenum
        cmp ax,0
        je b4
        ;----judge is W
        mov dl,[show]
        cmp dl,1
        je b5
        xor cx,cx
b1:
        xor dx,dx
        mov si,10
        div si

        push dx
        inc cx
        cmp ax,0
        jne b1
        mov si,cx

b2:
        pop dx
        add dl,30h
        mov ah,2
        int 21h
        loop b2

        mov cx,10
        sub cx,si
b3:
        mov ah,02h
        mov dl,20h
        int 21h
        loop b3

        ;----judge is W
        mov dl,[show]
        cmp dl,1
        je b5

        mov ah,9
        lea dx,space
        int 21h
        jmp b5

b4:
        call add_dirnum
        call sub_filenum
        ;----judge is W
        mov dl,[show]
        cmp dl,1
        je b5
        mov ah,09h
        lea dx,dir
        int 21h

b5:
        pop si
        pop dx
        pop ax
        pop cx
    ret
b2d     endp

add_filenum proc near
        push ax
        mov al,[filenum]
        inc al
        mov [filenum],al
        pop ax
        ret
add_filenum endp

add_dirnum proc near
        push ax
        mov al,[dirnum]
        inc al
        mov [dirnum],al
        pop ax
        ret
add_dirnum endp

sub_filenum proc near
        push ax
        mov al,[filenum]
        dec al
        mov [filenum],al
        pop ax
        ret
sub_filenum endp

;------------------------------print date and time-----------------------------------------------
datm    proc near
        push ax
        push bx
        push cx
        push dx
        push di


        lea bx,dta[18h];data
        mov di,[bx]
        mov cx,7;year
        xor bx,bx
d1:
        shl di,1
        rcl bx,1
        loop d1
        add bx,1980
        call bi2de
        mov ah,2
        mov dl,"."
        int 21h

        mov cx,4;month
        xor bx,bx
d2:
        shl di,1
        rcl bx,1
        loop d2
        call bi2de
        mov ah,2
        mov dl,"."
        int 21h

        mov cx,5;day
        xor bx,bx
d3:
        shl di,1
        rcl bx,1
        loop d3
        call bi2de
        mov cx,3
d7:
        mov ah,2
        mov dl," "
        int 21h
        loop d7

;time
        lea bx,dta[16h]
        mov di,[bx]
        mov cx,5;hour
        xor bx,bx
d5:
        shl di,1
        rcl bx,1
        loop d5
        call bi2de
        mov ah,2
        mov dl,":"
        int 21h
        mov cx,6;min
        xor bx,bx
d6:
        shl di,1
        rcl bx,1
        loop d6
        call bi2de


    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
datm    endp

;---------------------------------binary2decimal-----------------------------
bi2de    proc near
        push ax

        cmp bx,9
        ja bi0
        mov ah,2
        mov dl,'0'
        int 21h
bi0:
        xor cx,cx
        mov ax,bx
bi1:
        xor dx,dx
        mov si,10
        div si

        push dx
        inc cx
        cmp ax,0
        jne bi1

bi2:
        pop dx
        add dl,30h
        mov ah,2
        int 21h
        loop bi2

        pop ax
        ret
bi2de    endp

;----------------multiplies dx:ax x cx:bx return dx:ax:cx:bx
mymul32    proc     near

         push si
         push di
         mov      si,dx       ;save op1hi in si
         mov      di,ax       ;save op1lo in di
         mul      bx          ;op1l0 x op2lo
         push     ax          ;save 1st (32 bit) pp. on stack
         push     dx
;
         mov      ax,si       ;op1hi in ax
         mul      bx          ;op1hi x op2l0
         pop      bx          ;add 2nd (48 bit) pp. to pp1
         add      ax,bx
         adc      dx,0
         push     ax
         mov      bx,dx       ;pp1 + pp2 in bx:tos:tos+2
;
         mov      ax,di       ;op1lo in ax
         mul      cx          ;op1lo x op2hi
         pop      di          ;add 3rd (48 bit) pp. to pp1 + pp2
         add      di,ax
         push     di
         mov      di,0
         adc      bx,dx
         adc      di,0        ;pp1+pp2+pp3 in di:bx:tos:tos+2
;
         mov      ax,si       ;op1hi in ax
         mul      cx          ;op1hi x op2hi
         add      ax,bx       ;add 4th (64 bit) pp. to pp1+pp2+pp3
         adc      dx,di
         pop      cx
         pop      bx          ;final product in dx:ax:cx:bx
;
         pop      di
         pop      si
         ret
mymul32    endp

showspace proc near
    push ax
    push bx
    push dx
    push cx
    mov     ax,0
    mov     bx,0        ;bx: quotidient_32bits_high
    mov     dx,0        ;dx: quotidient_32bits_low
    mov     cx,0        ;counter = 0
;16bits or 32bits ?
    mov     ax,var_32bits_high
    cmp     ax,0
    jne     _32bits_routine
    jmp     _16bits_routine

;;;
_32bits_routine:
    mov     cx,0
;if == -2147483648 (-2^31)
    mov     ax,var_32bits_high
    cmp     ax,lowest_signed_32bits_high
    jne     check_if_neg
    mov     ax,var_32bits_low
    cmp     ax,lowest_signed_32bits_low
    jne     check_if_neg
;then
    lea     dx,lowest_signed_32bits_string
    mov     ah,9
    int     21h
    jmp     return_to_dos
;if < 0
check_if_neg:
    mov     ax,var_32bits_high
    cmp     ax,0
    jnl      preparations
;then print "-" ...
    mov     ah,2
    mov     dl,'-'
    int     21h
;... and negate number
    ;---------xor 0ffffffff , + 1----------
    mov     ax,var_32bits_high
    xor     ax,negate_mask
    mov     var_32bits_high,ax
    mov     ax,var_32bits_low
    xor     ax,negate_mask
    inc     ax
    mov     var_32bits_low,ax
    jnc     preparations
    mov     ax,var_32bits_high
    inc     ax
    mov     var_32bits_high,ax
preparations:
    mov     ax,var_32bits_high
    mov     quotidient_32bits_high,ax
    mov     ax,var_32bits_low
    mov     quotidient_32bits_low,ax
while_32bits:
; while >0 do
    mov     ax,quotidient_32bits_high
    cmp     ax,0
    jne     div_high_part
    mov     ax,quotidient_32bits_low
    cmp     ax,0
    jne     div_high_part
    jmp     print_char
div_high_part:
;divide high part
    mov     dx,0
    mov     ax,quotidient_32bits_high
    div     base_10
    mov     qhigh,ax
    mov     rhigh,dx
;case rhigh
    mov     ax,rhigh
    cmp     ax,0
    je      _rhigh0
    cmp     ax,1
    je      _rhigh1
    cmp     ax,2
    je      _rhigh2
    cmp     ax,3
    je      _rhigh3
    cmp     ax,4
    je      _rhigh4
    cmp     ax,5
    je      _rhigh5
    cmp     ax,6
    je      _rhigh6
    cmp     ax,7
    je      _rhigh7
    cmp     ax,8
    je      __rhigh8
    cmp     ax,9
    je      __rhigh9
_rhigh0:
    mov     ax,qhigh0
    mov     qhigh_redundant,ax
    mov     ax,rhigh0
    mov     rhigh_redundant,ax
    jmp     _aftercase

_rhigh1:
    mov     ax,qhigh1
    mov     qhigh_redundant,ax
    mov     ax,rhigh1
    mov     rhigh_redundant,ax
    jmp     _aftercase
__rhigh8:
    jmp _rhigh8
__rhigh9:
    jmp _rhigh9
_rhigh2:
    mov     ax,qhigh2
    mov     qhigh_redundant,ax
    mov     ax,rhigh2
    mov     rhigh_redundant,ax
    jmp     _aftercase
_rhigh3:
    mov     ax,qhigh3
    mov     qhigh_redundant,ax
    mov     ax,rhigh3
    mov     rhigh_redundant,ax
    jmp     _aftercase
_rhigh4:
    mov     ax,qhigh4
    mov     qhigh_redundant,ax
    mov     ax,rhigh4
    mov     rhigh_redundant,ax
    jmp     _aftercase
_rhigh5:
    mov     ax,qhigh5
    mov     qhigh_redundant,ax
    mov     ax,rhigh5
    mov     rhigh_redundant,ax
    jmp     _aftercase
_rhigh6:
    mov     ax,qhigh6
    mov     qhigh_redundant,ax
    mov     ax,rhigh6
    mov     rhigh_redundant,ax
    jmp     _aftercase
_rhigh7:
    mov     ax,qhigh7
    mov     qhigh_redundant,ax
    mov     ax,rhigh7
    mov     rhigh_redundant,ax
    jmp     _aftercase
_rhigh8:
    mov     ax,qhigh8
    mov     qhigh_redundant,ax
    mov     ax,rhigh8
    mov     rhigh_redundant,ax
    jmp     _aftercase
_rhigh9:
    mov     ax,qhigh9
    mov     qhigh_redundant,ax
    mov     ax,rhigh9
    mov     rhigh_redundant,ax
    jmp     _aftercase
_aftercase:
;divide low part
    mov     ax,0
    mov     q_0,ax
    mov     dx,0
    mov     ax,quotidient_32bits_low
    div     base_10
    mov     qlow,ax
    mov     rlow,dx
    mov     ax,rlow
    add     ax,rhigh_redundant
;if remainder >= 10
    cmp     ax,base_10
    jl      after_if
    sub     ax,base_10
    mov     dx,1
    mov     q_0,dx
after_if:
    mov     rlow,ax
    mov     ax,q_0
    add     ax,qlow
    mov     qlow,ax
    jnc     label1
    mov     ax,qhigh
    inc     ax
    mov     qhigh,ax
label1:
    mov     ax,qlow
    add     ax,qhigh_redundant
    mov     qlow,ax
    jnc     label2
    mov     ax,qhigh
    inc     ax
    mov     qhigh,ax
label2:
;push remainder to stack
    mov     ax,rlow
    push    ax
    inc     cx
    mov     ax,qhigh
    mov     quotidient_32bits_high,ax
    mov     ax,qlow
    mov     quotidient_32bits_low,ax
    jmp     while_32bits

;;;
_16bits_routine:
    mov     ax,var_32bits_low
    mov     bx,0   ;bx: quotient
    mov     cx,0
while_loop:
    cmp     ax,0
    je      print_char
    mov     dx,0
    div     base_10
    mov     bx,ax ;ax stores quotidient
    mov     ax,dx ;dx stores remainder
;push remainder
    push    ax
;counter = counter + 1
    inc     cx
;numerator = quotidient
    mov     ax,bx
    jmp     while_loop
print_char:
    cmp     cx,0
    je      return_to_dos
    pop     ax
;because at this point 0 <= ax <= 9, setting ah = 2 does not change the results
    mov     ah,2
    mov     dl,al
    add     dl,30h   ;0-> '0',1->'1',....
    int     21h
    dec     cx
    jmp     print_char

return_to_dos:

    pop cx
    pop dx
    pop bx
    pop ax
    ret
showspace endp

;--------input:ax----------
add_used_space proc near
        push ax
        push bx
        push cx
        push dx

        mov dx,[used_32bits_low]
        add dx,ax
        jnc add_finish
        mov bx,[used_32bits_high]
        inc bx
        mov [used_32bits_high],bx

add_finish:
        mov [used_32bits_low],dx

        pop dx
        pop cx
        pop bx
        pop ax
        ret
add_used_space endp

main    ends
        end p1
