.PHONY: all clean
all: asteroids2d.class

asteroids2d.class: asteroids2d.scm
	java kawa.repl --main -C $^

clean:
	rm *.class
