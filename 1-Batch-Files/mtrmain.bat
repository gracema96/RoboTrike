asm86 mtrmain.asm m1 ep db
asm86 eventq.asm m1 ep db
asm86 queue.asm m1 ep db 
asm86 cs.asm m1 ep db  
asm86 int.asm m1 ep db 
asm86 int2.asm m1 ep db
asm86 timer1.asm m1 ep db
asm86 serial.asm m1 ep db  
asm86 converts.asm m1 ep db
asm86 motors.asm m1 ep db  
asm86 parinit.asm m1 ep db 
asm86 trigtbl.asm m1 ep db
asm86 parser.asm m1 ep db

link86 mtrmain.obj, eventq.obj, queue.obj, cs.obj, int.obj, int2.obj, timer1.obj TO temp.lnk
link86 serial.obj, converts.obj, motors.obj, parinit.obj, trigtbl.obj, parser.obj TO orary.lnk 
link86 temp.lnk, orary.lnk TO mtrmain.lnk 
loc86 mtrmain.lnk TO mtrmain NOIC AD(SM(CODE(4000h), data(400h), stack(7000h)))

