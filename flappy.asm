;Printing a snake at random position and start moving

[org 0x0100]
jmp start
COUNT : dd 0;
FLAG_END_GAME: db 0;
FLAG_PRINT_TOWER: db 0;
FLAG_REMOVE_OBSTACLE: db 0;

BIRD_FACE: db '(")>'
BOUNDARY_SYMBOL: db '-'
OBSTACLE_SYMBOL: db '|'
GAME_OVER_TEXT: db '(-_-) GAME-OVER (-_-)'
GAME_START_TEXT: db '<(") FLAPPY BIRD (")>'
DEVELOPER_INFORMATION: db 'F219075 WALEED ASIF | F219113 SOBAN RAFIQ'
INSTRUCTION_INFORMATION: db 'PRESS SPACE BAR TO JUMP AND AVOID ALL THE OBSTACLES'
SCORE_TEXT: db 'TOTAL SCORE IS: ';

GAME_SCORE: dw 0;
TOP_OBSTACLES_POS: dw 25,40,60;
BOTTOM_OBSTACLES_POS: dw 35,55,70;

;code to clear the screen
clearscreen:
    push es
    push ax
    push di
    push cx
    mov ax,0xb800 ; video memory address
    mov es,ax
    mov ax,0x0720 ; color code and space ASCII
    mov di,0
    nextchar:
        mov [es:di],ax
        add di,2
        cmp di,4000
        jne nextchar

    ;popping all values
    pop cx
    pop di
    pop ax
    pop es
    ret

RANDOM_NUMBER:
	push ax;
	push dx;
	push cx;

	mov ah, 00h  ; interrupts to get system time        
	int 1AH      ; CX:DX now hold number of clock ticks since midnight      

	mov  ax, dx
	xor  dx, dx
	mov  cx, [bp-2]  ;upper number
	div  cx       ; here dx contains the remainder of the division - from 0 to 9

	add  dx, 0  ; to ascii from '0' to '9'
	mov bx,dx;
	
	pop cx;
	pop dx;
	pop ax;
ret;

DELAY: ;a large loop for DELAYing next move
	mov dword[COUNT],200000
	
	DELAY_LOOP:
		dec dword[COUNT];
		cmp dword[COUNT],0;
	jne DELAY_LOOP;
	
ret;



PRINT_BIRD:
	push ax;
	push di;
	push es;
	push bx;
	push si;
	push cx;
	
	call SCORE;
	
	mov [bp-2],bx; prev Y POS for resetting
	
	mov ax,[bp+6]; x pos
	mov [bp-4],ax;
	call CALCULATE_POS;
	
	
	;for cancel blinking
	mov ax, 1003h
	mov bx, 0
	int 10h
	
	mov cx,4; counter of bird length
	mov ax,0x0720;
	clearprev:
		mov word [es:di],ax; clearing previous position of bird
		add di,2;
	loop clearprev;

	mov ax,[bp+4]; Y POS
	mov [bp-2],ax;

	call CALCULATE_POS;

	mov cx,4;
	mov si,BIRD_FACE;
	mov ah,0xf;
	
	cld
	nextword:
		lodsb
		stosw
		
	loop nextword;

	pop cx
	pop si
	pop bx
	pop es
	pop di
	pop ax
ret

CALCULATE_POS:
	push cx
	push dx
	push ax
	mov cx,[bp-2] ;Y POS
		mov ax,80
		mul cx
		add ax,[bp-4] ;x pos
		shl ax,1

	mov di,ax
		mov ax,0xb800
		mov es,ax

	pop ax;
	pop dx;
	pop cx;
ret;

boundary:
	push si;
	push di;
	push cx;
	push es;
	push ax;
	push dx;
	
	xor dx,dx; reseting for outerloop;
	
	xor ax,ax;
	mov [bp-2],ax; Y POS setting at top
	mov [bp-4],ax; x pos
	
	call CALCULATE_POS;
	
	TOP_BOTTOM:
		;top
		mov cx,80;
		mov ax,[BOUNDARY_SYMBOL];
		mov ah,47h;
	
		cld
		INNER_TB:
			stosw
			
		loop INNER_TB;
	
		;bottom
		mov ax,24
		mov [bp-2],ax;Y POS setting at bottom
		mov ax,0
		mov [bp-4],ax;X pos
		call CALCULATE_POS;
	
	inc dx;
	cmp dx,2;
	jne TOP_BOTTOM
	
	xor dx,dx
	xor ax,ax;
	mov [bp-2],ax; Y POS setting at top
	mov [bp-4],ax; x pos
	
	call CALCULATE_POS;
	
	LEFT_RIGHT:
		;left
		mov cx,25;
		mov ax,[BOUNDARY_SYMBOL];
		mov ah,47h;

		INNER_RL:
			mov [es:di],ax
			add di,160;
		loop INNER_RL
			
		;right
		mov ax,0
		mov [bp-2],ax;Y POS setting at bottom
		mov ax,79
		mov [bp-4],ax;x pos
		call CALCULATE_POS;
		
		mov cx,25;
		mov ax,[BOUNDARY_SYMBOL];
		mov ah,47h;
	inc dx;
	cmp dx,2;
	jne LEFT_RIGHT

	pop dx
	pop ax
	pop es
	pop cx
	pop di
	pop si

ret;

DEATH_BY_BOUNDARY:
	push ax
	
	mov ax,[bp+4]; Y POS
	cmp ax,0;
	jle SET_FLAG
	cmp ax,24
	jge SET_FLAG
	
	mov ax,[bp+6]; x pos
	cmp ax,0;
	jle SET_FLAG
	cmp ax,79
	jge SET_FLAG
	
	jmp NO_SET_FLAG
	
	SET_FLAG:
		mov byte[FLAG_END_GAME],1;

	NO_SET_FLAG:
	pop ax;
ret


CREATE_TOWER:
	push di;
	push ax;
	push cx;
	push es;
	push bx;
	
	cmp byte[FLAG_PRINT_TOWER],1
	jne BOTTOM_TOWERS;
	
	topTowers:
		mov word[bp-2],1; Y POS
		call CALCULATE_POS;
		
		TOWER_TOP_LOOP:
			mov [es:di],ax
			add di,160;
		loop TOWER_TOP_LOOP
		jmp EXIT_TOWER
	
	BOTTOM_TOWERS:
		mov word[bp-2],23; Y POS
		call CALCULATE_POS;
		
		TOWER_BOTTOM_LOOP:
			mov [es:di],ax
			sub di,160;
		loop TOWER_BOTTOM_LOOP
		jmp EXIT_TOWER
	
	EXIT_TOWER:
		
	pop bx;
	pop es;
	pop cx;
	pop ax;
	pop di;
		
ret

CREATE_OBSTACLES:
	push cx;
	push bx;
	push ax;
	
	cmp byte[FLAG_REMOVE_OBSTACLE],1
	je REMOVE;
	
	CREATE:
		mov ax,[OBSTACLE_SYMBOL];
		mov ah,7h;
	jmp top;
	
	REMOVE:
		mov ax,0x0720;
top:
	mov byte[FLAG_PRINT_TOWER],1
	
	mov bx,[TOP_OBSTACLES_POS];
	mov word[bp-4],bx; X pos
	mov cx,9;
	call CREATE_TOWER
	
	mov bx,[TOP_OBSTACLES_POS+2];
	mov word[bp-4],bx;
	mov cx,7; size
	call CREATE_TOWER
	
	mov bx,[TOP_OBSTACLES_POS+4];
	mov word[bp-4],bx;
	mov cx,10;
	call CREATE_TOWER
	
	
bottom:
	mov byte[FLAG_PRINT_TOWER],0
	
	mov bx,[BOTTOM_OBSTACLES_POS];
	mov word[bp-4],bx;
	mov cx,10;
	call CREATE_TOWER
	
	mov bx,[BOTTOM_OBSTACLES_POS+2];
	mov word[bp-4],bx;
	mov cx,9;
	call CREATE_TOWER
	
	mov bx,[BOTTOM_OBSTACLES_POS+4];
	mov word[bp-4],bx;
	mov cx,7;
	call CREATE_TOWER

	pop ax;
	pop bx;
	pop cx;

ret

MOVE_OBSTACLES:
	push bx;
	push ax;
	push si;
	push ds;
	
	mov cx,3;
	mov bx,TOP_OBSTACLES_POS;
	mov si,0;

	LOOP_MOV_OBSTACLES_TOP:
		mov ah,[bx+si];
		cmp ah,8; from this pos obstacles will re appear
		jge MOVE_BACKWARD_TOP;
		
		mov ah,70;
		jmp CONT_MOVING_TOP
		
		MOVE_BACKWARD_TOP:
			sub ah,1;
			jmp CONT_MOVING_TOP
		
		CONT_MOVING_TOP:
			mov [bx+si],ah;
			add si,2;
	
	loop LOOP_MOV_OBSTACLES_TOP;
	
	mov cx,3;
	mov bx,BOTTOM_OBSTACLES_POS;
	mov si,0;

	LOOP_MOV_OBSTACLES_BOTTOM:
		mov ah,[bx+si];
		cmp ah,8; from this pos obstacles will re appear
		jge MOVE_BACKWARD_BOTTOM;
		
		mov ah,70;
		jmp CONT_MOVING_BOTTOM
		
		MOVE_BACKWARD_BOTTOM:
			sub ah,1;
			jmp CONT_MOVING_BOTTOM
		
		CONT_MOVING_BOTTOM:
			mov [bx+si],ah;
			add si,2;
	
	loop LOOP_MOV_OBSTACLES_BOTTOM;
	
	
	
	pop dx;
	pop si;
	pop ax;
	pop bx;
ret


CALLING_FOR_MOVE_OBSTACLES:
	mov byte[FLAG_REMOVE_OBSTACLE],0;
	call CREATE_OBSTACLES;
	
	
	mov byte[FLAG_REMOVE_OBSTACLE],1;
	call CREATE_OBSTACLES;
	
	call MOVE_OBSTACLES;
	
	mov byte[FLAG_REMOVE_OBSTACLE],0;
	call CREATE_OBSTACLES;

ret

CHECK_COLLISION:
	push ax;
	push cx;
	push es;
	push di;
	
	mov ax,[bp+4]; Y POS
	mov [bp-2],ax;

	mov ax,[bp+6]; x pos
	mov [bp-4],ax;
	
	call CALCULATE_POS
	
	mov ax,[OBSTACLE_SYMBOL];
	mov ah,7h;
	mov cx,5;
	
	CHECK_NEXT:
		cmp ax,[es:di];
		je END_GAME
		add di,2;
	loop CHECK_NEXT

	jmp END_SUB
	
	END_GAME:
		mov byte[FLAG_END_GAME],1;
	
	END_SUB:
	pop di;
	pop es;
	pop cx;
	pop ax;
	
ret

SCORE:
	push ax;
	push cx;
	push bx;
	push si;
	push dx;
	push es;
	push di;
	
	mov ax,[bp+6]; x pos
	add ax,2;
	mov bx,TOP_OBSTACLES_POS;
	mov cx,3;
	mov si,0;
	
	LOOP_SCORE:
		cmp ax,[bx+si];
		je INC_SCORE;
		jmp END_SCORE
		
		INC_SCORE:	
		add word[GAME_SCORE],1;
		
		END_SCORE:
		add si,2;
		
	loop LOOP_SCORE;
	
	mov bx,BOTTOM_OBSTACLES_POS;
	mov cx,3;
	mov si,0;
		
	LOOP_SCORE_BOTTOM:
		cmp ax,[bx+si];
		je INC_SCORE_BOTTOM;
		jmp END_SCORE_BOTTOM
		
		INC_SCORE_BOTTOM:	
		add word[GAME_SCORE],1;
		
		END_SCORE_BOTTOM:
		add si,2;
	loop LOOP_SCORE_BOTTOM;
	

	mov word[bp-2],22; Y POS
	mov word[bp-4],76;
	
	mov ah,02h;
	mov bh,0h;
	mov dh,17h; row
	mov dl,4Eh; column
	int 10h
	
	mov ax,[GAME_SCORE];
	call PRINT_SCORE;
	
	pop di;
	pop es;
	pop dx;
	pop si;
	pop bx;
	pop cx;
	pop ax;
ret	

PRINT_SCORE:         
     push cx;
	 push dx;
	 push bx;
	 push ax;

    ;initialize count
    mov cx,0
    mov dx,0
    LOOP_PRINT:
        ; if ax is zero
        cmp ax,0
        je PRINT_CHAR    
         
        ;initialize bx to 10
        mov bx,10       
         
        ; extract the last digit
        div bx                 
         
        ;push it in the stack
        push dx             
         
        ;increment the count
        inc cx             
         
        ;set dx to 0
        xor dx,dx
        jmp LOOP_PRINT
    PRINT_CHAR:
        ;check if count
        ;is greater than zero
        cmp cx,0
        je END_PRINT
         
        ;pop the top of stack
        pop dx
         
        ;add 48 so that it
        ;represents the ASCII
        ;value of digits
        add dx,48
         
        ;interrupt to print a
        ;character
        mov ah,02h
        int 21h
         
        ;decrease the count
        dec cx
        jmp PRINT_CHAR
		
END_PRINT:
	pop ax;
	pop bx;
	pop dx;
	pop cx;
	
ret


GAME_OVER:
	push ax;
	push bx;
	push cx;
	push es;
	push di;
	push si;
	
	call boundary;
	
	mov si,0;
	
	mov ah,0xE;
	
	LOOP_GAME_OVER:
		mov al,[bx+si]
		mov [es:di],ax;
		add si,1;
		add di,2;
	loop LOOP_GAME_OVER;
	
	pop si;
	pop di;
	pop es;
	pop cx;
	pop bx;
	pop ax;
	
ret;

scrollup: 
push bp
mov bp,sp
push ax
push cx
push si
push di
push es
push ds
mov ax, 80 ; load chars per row in ax
mul byte [bp+4] ; calculate source position
mov si, ax ; load source position in si
push si ; save position for later use
shl si, 1 ; convert to byte offset
mov cx, 2000 ; number of screen locations
sub cx, ax ; count of words to move
mov ax, 0xb800
mov es, ax ; point es to video base
mov ds, ax ; point ds to video base
xor di, di ; point di to top left column
cld ; set auto increment mode
rep movsw ; scroll up
mov ax, 0x0720 ; space in normal attribute
pop cx ; count of positions to clear
rep stosw ; clear the scrolled space
pop ds
pop es
pop di
pop si
pop cx
pop ax
pop bp
ret 2

FLAPPY_BIRD:
   
    push bp
    mov bp ,sp
	sub sp,4;
    push di
    push es
    push ax
    push bx
    push dx

	
	;STARTUP TEXT
	mov ax,5; 
	mov [bp-2],ax; Y POS
	mov ax,28;
	mov [bp-4],ax; X pos
	
	call CALCULATE_POS;
	mov cx,21;
	mov bx,GAME_START_TEXT;
	call GAME_OVER;
	mov ah,00h
	INT 16h;
	
	;DEVELOPER INFO
	mov ax,10; 
	mov [bp-2],ax; Y POS
	mov ax,19;
	mov [bp-4],ax; X pos
	
	call CALCULATE_POS;
	mov cx,41;
	mov bx,DEVELOPER_INFORMATION;
	call GAME_OVER;
	
	mov ah,00h
	INT 16h;
	
	;INSTRUCTIONS
	mov ax,15; 
	mov [bp-2],ax; Y POS
	mov ax,14;
	mov [bp-4],ax; X pos
	
	call CALCULATE_POS;
	mov cx,51;
	mov bx,INSTRUCTION_INFORMATION;
	call GAME_OVER;
	
	mov ah,00h
	INT 16h;
	
	
	call clearscreen
	call boundary;
  
	mov ax,[bp+4]; Y POS
	mov [bp-2],ax;

	mov ax,[bp+6]; x pos
	mov [bp-4],ax;
	
	call CALCULATE_POS ;setting es:di and setting pos
	
	mov bx,2; first time boundary clear kar rhi thi isliye bx mein 2 rakhwaya hai

	MOVING:
		
		call PRINT_BIRD;  
		
		mov ah,01h ;interupts to check if space key is pressed
		INT 16h;
		jz KEY_PRESSED
		
		mov ah,00h
		INT 16h;
		
	KEY_PRESSED:
		mov cx, [bp+4] ; ypos
		
		cmp ah,39h; SCANCODE OF SPACE BAR
		je UP;
		
		cmp cx, 25;
		
		;jg RESET;
		jb DOWN;

		;RESET:
			;mov cx,4;
			;jmp MOVE_BIRD
		DOWN:
			add cx,1;
			jmp MOVE_BIRD
		UP:
			sub cx,2;
		
	MOVE_BIRD:
		mov bx,[bp+4]; moving old value of y to clear from there
		
		mov [bp+4],cx ;Y POS
		
		call DEATH_BY_BOUNDARY;
		call CALLING_FOR_MOVE_OBSTACLES;
		call CHECK_COLLISION;
		call DELAY
		
		cmp byte[FLAG_END_GAME],1 ; LOOP TILL FLAG IS 0
		
	jne MOVING;
	
	call clearscreen;
	
	mov ax,12; 
	mov [bp-2],ax; Y POS
	mov ax,28;
	mov [bp-4],ax; X pos
	
	call CALCULATE_POS;
	mov cx,21;
	mov bx,GAME_OVER_TEXT;
	call GAME_OVER;
	
	;SCORE OF GAME AT END
	mov ax,15; 
	mov [bp-2],ax; Y POS
	mov ax,30;
	mov [bp-4],ax; X pos
	
	call CALCULATE_POS;
	mov cx,16;
	mov bx,SCORE_TEXT;
	call GAME_OVER;
	
	mov ah,02h;
	mov bh,0h;
	mov dh,0xF; row
	mov dl,2Eh; column
	int 10h
	
	mov ax,[GAME_SCORE];
	call PRINT_SCORE;
	
	
	mov ah,00h
	INT 16h;
	
    pop dx
    pop bx
    pop ax
    pop es
    pop di
	mov sp, bp
    pop bp
    ret 4


start:
call clearscreen

;LOCATION OF BIRD AT START

push 12 ;COLUMN
push 15 ;ROW

call FLAPPY_BIRD

mov ax,0x4c00
int 0x21