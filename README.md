### Why

Just for fun.

---

### What is the toy language like

The language is:

1. compiled;
2. object oriented (you could just use the imperative part);
3. (not implemented yet) garbage collected

As valgrind doesn't work on my Mac, I decided to leave the gc part unfinished.

However, you can see that I implemented incref/decref in Object, the universal base class.

---
### Known bugs

The biggest bugs are:

1. public/private/protected are all ignored.
2. memory leak (because gc is not implemented, obviously)
	
There are also some smaller bugs which I tagged FIXME in comment.

---

### How to compile… the compiler itself and the example codes?

	$ # the compiler
	$ ./build.sh
	$
	$ # the examples
	$ mkdir -p example/oop/build
	$ ./dlc example/oop > example/oop/build/main.dl.s
	$ cp src/Runtime/main.c example/oop/build
	$ cd example/oop/build
	$ cc -c main.dl.s
	$ cc -c main.c
	$ cc main.o main.dl.o
	
Notice that the compiler is targeded on Intel x86-64 Mac only.

---

### Why don't you fix all the bugs?

Because:

1. I have a new project to work on.
2. I'm pretty sure I know how to fix them, so that's not interesting anymore.

Also, This compiler is just a TOY; it's implementation is UGLY. I don't care.

---

### Anything else you want to say?

Mike, I don't have to take your class to figure out how to write a compiler. You see that?

Alan, thanks for the awesome quarter! :)