CML: CML.yy.o y.tab.o 
	gcc CML.yy.o y.tab.o -o CML -lfl

CML.yy.o: CML.yy.c y.tab.c y.tab.h
	gcc -c CML.yy.c

y.tab.o: y.tab.c y.tab.h
	gcc -c y.tab.c

CML.yy.c: CML.l y.tab.h
	flex -o CML.yy.c CML.l

y.tab.c y.tab.h: CML.y
	bison -v --defines=y.tab.h -o y.tab.c CML.y

clean:
	rm *.o *.c *.h CML
