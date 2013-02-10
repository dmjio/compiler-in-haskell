main: main.dl.o main.o
	cc -g -Wall -o main main.dl.o main.o

main.dl.o: main.dl.s
	as -o main.dl.o main.dl.s

main.o: main.c
	cc -g -Wall -c main.c
