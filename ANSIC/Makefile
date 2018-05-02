ansic: ansic.yy.o y.tab.o 
	gcc ansic.yy.o y.tab.o -o ansic -lfl

ansic.yy.o: ansic.yy.c y.tab.c y.tab.h
	gcc -c ansic.yy.c

y.tab.o: y.tab.c y.tab.h
	gcc -c y.tab.c

ansic.yy.c: ansic.l y.tab.h
	flex -o ansic.yy.c ansic.l

y.tab.c y.tab.h: ansic.y
	bison -v --defines=y.tab.h -o y.tab.c ansic.y

clean:
	rm *.o *.c *.h ansic
