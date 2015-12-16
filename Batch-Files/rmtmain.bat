asm86 rmtmain.asm m1 ep db
asm86 eventq.asm m1 ep db
asm86 cs.asm m1 ep db  
asm86 int.asm m1 ep db 
asm86 int2.asm m1 ep db
asm86 timer0.asm m1 ep db 
asm86 converts.asm m1 ep db
asm86 queue.asm m1 ep db
asm86 display.asm m1 ep db
asm86 keypad.asm m1 ep db
asm86 segtable.asm m1 ep db
asm86 serial.asm m1 ep db

link86 rmtmain.obj, eventq.obj, cs.obj, int.obj, int2.obj, timer0.obj, converts.obj TO temp.lnk
link86 queue.obj, display.obj, keypad.obj, segtable.obj, serial.obj TO orary.lnk 
link86 temp.lnk, orary.lnk TO remote.lnk
loc86 remote.lnk TO remote NOIC AD(SM(CODE(4000h), data(400h), stack(7000h))) 
