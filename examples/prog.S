    .text	
    .org 0
_reset:
    b start

    .org 60
    .dc.w 0xaffe
    .org 64
start:
    ldr r0, [pc, #-12]
loop:
    add r0, r0, #1
    b   loop

