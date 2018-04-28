# Calculadora em notacao polonesa reversa (posfixa)  

ansic: ansic.yy.o ansic.tab.o 
	gcc ansic.yy.o ansic.tab.o -o ansic -lfl -lm

ansic.yy.o: ansic.yy.c ansic.tab.c
	gcc -c ansic.yy.c

ansic.tab.o: ansic.tab.c
	gcc -c ansic.tab.c

ansic.yy.c: ansic.l ansic.tab.h
	flex -o ansic.yy.c ansic.l

ansic.tab.c ansic.tab.h: ansic.y
	bison --defines=ansic.tab.h -o ansic.tab.c ansic.y

clean:
	rm *.o *.c *.h ansic
