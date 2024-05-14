IDEAL
MODEL small
STAcK 100h
DATASEG
; --------------------------
; Your variables here
; --------------------------
getFuncAmount db 6 dup(?)
getA db 6 dup (?)
getB db 6 dup (?)
getC db 6 dup (?)
xKitzon dw ?
yKitzon dw ?
currentM dw ?
holder dw 0
holder2 dw 0
funcAmount db 0
aPar db 0
bPar db 0
cPar db 0
setColor db 66

amountOfFunc db 	'enter how many functions to write: ','$'
needPostiveNum db 	'needs positive number              ','$'
enterA db			'enter A-number between 5 and -5:   ','$'
enterANotZero db	'number cant be 0:                  ','$'
enterB db 			'enter B-number between 20 and -20: ','$'
enterC db 			'enter C-number between 70 and -70: ','$'
CODESEG

proc deleteText
    push dx
	push cx
	push ax
	
	mov cx, 040
	removeAllTextLoop: ;backspace only moves the curser 1 step back, you need to do backspace+space+backspace to remove it

		
		mov dl, 08
		mov ah, 2
		int 21h
		loop removeAllTextLoop
		
	pop ax
	pop cx
	pop dx
	ret
	
    endp deleteText
	
macro negByteToWord ;turns the number in al to a word by checking if its negative and if so than add FF to ah
    local LocNegativeA
    cmp al, 0
    jnl LocNegativeA
    mov ah, 0ffh
    LocNegativeA:
    endm negByteToWord

proc turnToNum
	push bp
	mov bp, sp
	locStringNum equ [bp + 4] 	;location of the string (getA)
	locIntNum equ [bp + 6]  	;location of the final num (numA)
	push dx
	push cx
	push bx
	push ax
	
	mov bx, locStringNum
	cmp [byte ptr bx+2], '-'		
	jne positive1
	mov dx, 1
	inc bx ;skips '-'
	positive1:
	xor ax, ax ;if one dig
	cmp [byte ptr bx+3], 0Dh ;is double digit
	je oneDigit
	
	mov ah, [byte ptr bx+2]
	sub ah, '0' ;tens digits
	mov al, [byte ptr bx+3]
	sub al, '0' ;ones digits
	jmp afterOneTwoDig
	oneDigit:
	mov al, [byte ptr bx+2]
	sub al, '0' ;ones digits
	afterOneTwoDig:
	mov bx, locIntNum
	mov [byte ptr bx], 0
	add [byte ptr bx], al
	
	mov al, ah
	mov cl, 10
	add10times:
		mul cl
		add [byte ptr bx], al
		
	cmp dx, 1
	jne positive2
	neg [byte ptr bx]
	positive2:
	pop ax
	pop bx 
	pop cx
	pop dx
	pop bp
	ret 4
endp turnToNum

proc writeGraph
	push bp
	mov bp, sp

	mov cx,320
	mov bh,0h
	mov dx,100
	mov al, 66h
	Row:
	mov ah,0ch
	int 10h
	loop Row

	mov cx,160
	mov bh,0h
	mov dx,200
	mov al, 66h
	collum:
	mov ah,0ch
	int 10h
	dec dx
	cmp dx, 0
	ja collum

	pop bp 
	ret 
endp writeGraph

proc findM
	push bp
	mov bp, sp
	MlocA equ [bp + 4]
	MlocB equ [bp + 6]
	MlocX equ [bp + 8]
	MoffsetToPlaceM equ [bp + 10]
	push dx
	push cx
	push bx
	push ax
	
	mov ax, MlocA
	negByteToWord
	

	mov dx, MlocX
	mov bl, dl 
	
	imul bl
	mov bl, 2
	imul bl
	
	push ax
	mov ax, MlocB
	negByteToWord
	pop bx
	add ax, bx
	cmp ax, 0
	jg setMtoPositive
		neg ax
	setMtoPositive:
	mov bx, MoffsetToPlaceM
	mov [bx], ax
	
	
	pop ax
	pop bx
	pop cx
	pop dx
	pop bp 
	ret 8
endp findM


proc findXKitzon
	push bp
	mov bp, sp
	XKlocA equ [bp + 4]
	XKlocB equ [bp + 6]
	KoffsetOfX equ [bp + 8]
	push dx
	push cx
	push bx
	push ax
	
	mov bx, XKlocA
	shl bl,1 
	mov ax, XKlocB
	
	negByteToWord

	
	XKnegativeA:
	idiv bl
	
	mov bx, KoffsetOfX
	xor ah, ah
	
	neg al
	negByteToWord
	
	KIfnegative:
	mov [bx], ax
	
	pop ax
	pop bx
	pop cx
	pop dx
	pop bp 
	ret 6
endp findXKitzon

proc findYKitzon
	push bp
	mov bp, sp
	YKlocA equ [bp + 4]
	YKlocB equ [bp + 6]
	YKlocC equ [bp + 8]
	YKitzonX equ [bp + 10] ;x of kitzon 
	KoffsetOfY equ [bp + 12] 
	push dx
	push cx
	push bx
	push ax
	
	mov ax, YKitzonX;x is only a byte
	xor ah, ah
	mov bl, al
	imul bl ;A * num in ax
	xor ah, ah ;can only be max 100 or -100 which is only a byte
	mov bx, YKlocA
	xor bh, bh
	imul bl
	mov [holder], ax
	
	mov ax, YKitzonX
	xor ah, ah
	mov bx, YKlocB
	xor bh, bh
	
	imul bl
	push ax
	mov ax, YKlocC
	negByteToWord
	mov cx, ax
	pop ax
	add ax, cx
	add ax,[holder]
	
	mov bx, KoffsetOfY
	mov [bx], ax ; ax has Y
	
	
	pop ax
	pop bx
	pop cx
	pop dx
	pop bp 
	ret 10
endp findYKitzon

proc writeFunc

	push bp
	mov bp, sp
	localA equ [byte ptr bp + 4]
	localB equ [byte ptr bp + 6]
	localc equ [byte ptr bp + 8]

	mov bl, 0
	cmp localA, 0
	jnl aPositiveGraph
		mov bl, 1
		neg localA
	
	
	
	
	
	aPositiveGraph:
	
	
	
	
	
	
	
	xor ax, ax	
	push offset xKitzon
	mov al, [bPar]
	push ax
	mov al, [aPar]
	push ax
	 
	call findXKitzon
	
	xor ax, ax
	push offset yKitzon
	push [xKitzon]
	mov al, [cPar]
	push ax
	mov al, [bPar]
	push ax
	mov al, [aPar]
	push ax
	call findYKitzon
	
	mov cx, 160 ;locX
	
	add cx, [xKitzon]
	mov bh,0h ;const
	mov dx,100 ;locY
	
	sub dx, [yKitzon]
	
	
	
	mov [holder], cx;the kitzon location, will host the current location of x
	sub [holder], 160
	writePositive:
		xor ax, ax
		push offset currentM
		
		push [holder]
		mov al, localB
		push ax
		mov al, localA
		push ax
		call findM 
		mov si, [currentM]
		
		cmp si, 0
		jne writeOnY ;if si is 0
		mov si, 1
		
		writeOnY:
			mov al, [setColor] ;color
			mov ah, 0ch
			int 10h
			
			push cx
			
			;writes on the opposite side of func
			sub cx, [xKitzon]
			mov [holder2], cx
			mov cx, [xKitzon]
			sub cx, [holder2]
			mov ah, 0ch
			int 10h
			
			pop cx
		
			cmp bl, 0
			je writeDown
				inc dx ;goes 1 pixel up
				jmp afterDownOrUp
			
			writeDown:
				dec dx ; goes 1 pixel down
			afterDownOrUp:
			
			cmp dx, 200
			je outer
			cmp dx, 0
			je outer
			
			dec si
			cmp si,0
			ja writeOnY
		jmp after
		
			
		after:
		inc [holder]

		mov si, 2
		writeForward:
			inc cx
			dec si
			cmp si, 0
			jne writeForward
			
		cmp cx, 0
		jl outer

		jmp writePositive
		outer:
	
	;;;;;pop [word ptr 0]
	pop bp 
	ret 4
endp writeFunc

start:
	mov ax, @data
	mov ds, ax
; --------------------------
; Your code here
; --------------------------
graphicMode:
	mov ax, 13h
	int 10h
	call writeGraph

mov dx, offset amountOfFunc
mov ah, 9h
int 21h

enterFuncs: ;skips first block
mov ah, 0Ah
mov dx, offset getFuncAmount
mov bx, dx
mov [byte ptr bx], 4
int 21h

push offset funcAmount
push offset getFuncAmount
call turnToNum

call deleteText

cmp [funcAmount], 0
jg gotFuncAmount

mov dx, offset needPostiveNum
mov ah, 9h
int 21h
jmp enterFuncs

gotFuncAmount:

MainFuncsLoop:
		askForA:
		mov dx, offset enterA
		mov ah, 9h
		int 21h
		afterAskForA:

		mov ah, 0Ah
		mov dx, offset getA
		mov bx, dx
		mov [byte ptr bx], 4
		int 21h
		
		call deleteText
		
		push offset aPar
		push offset getA
		call turnToNum

		cmp [aPar], 5
		jg askForA
		cmp [aPar], -5
		jl askForA
		cmp [aPar], 0
		jne askForB
		
		AisZero:
		mov dx, offset enterANotZero
		mov ah, 9h
		int 21h
		
		
		cmp [aPar], 0
		je afterAskForA

		askForB:
		mov dx, offset enterB
		mov ah, 9h
		int 21h

		mov ah, 0Ah
		mov dx, offset getB
		mov bx, dx
		mov [byte ptr bx], 4
		int 21h
		
		call deleteText

		push offset bPar
		push offset getB
		call turnToNum

		cmp [bPar], 20
		jg askForB
		cmp [bPar], -20
		jl askForB

		jmp askForC
		
		middleJmp:
		jmp MainFuncsLoop ; since jmp in line 508 too large
		
		askForC:
		mov dx, offset enterC
		mov ah, 9h
		int 21h

		mov ah, 0Ah
		mov dx, offset getC
		mov bx, dx
		mov [byte ptr bx], 4
		int 21h

		call deleteText
		
		push offset cPar
		push offset getC
		call turnToNum

		cmp [cPar], 70
		jg askForC
		cmp [cPar], -70
		jl askForC

		
		xor ax, ax
		mov al, [cPar];c
		push ax
		mov al, [bPar];b
		push ax
		mov al, [aPar];a
		push ax
		call writeFunc
		
	add [setColor], 3 ;to change color for each func
	
	dec [funcAmount]
	cmp [funcAmount], 0
jg middleJmp
 
exit:
	mov ax, 4c00h
	int 21h
END start
