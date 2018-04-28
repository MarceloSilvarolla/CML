# Calculadora em notacao polonesa reversa (posfixa)  

calcpr: calcpr.yy.o calcpr.tab.o 
	gcc calcpr.yy.o calcpr.tab.o -o calcpr -lfl -lm

calcpr.yy.o: calcpr.yy.c calcpr.tab.c
	gcc -c calcpr.yy.c

calcpr.tab.o: calcpr.tab.c
	gcc -c calcpr.tab.c

calcpr.yy.c: calcpr.l calcpr.tab.h
	flex -o calcpr.yy.c calcpr.l

calcpr.tab.c calcpr.tab.h: calcpr.y
	bison --defines=calcpr.tab.h -o calcpr.tab.c calcpr.y

clean:
	rm *.o *.c *.h calcpr
