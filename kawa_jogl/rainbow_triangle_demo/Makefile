.PHONY: all clean
all: rainbow_triangle_demo.class

rainbow_triangle_demo.class: rainbow_triangle_demo.scm
	java kawa.repl --main -C $^

clean:
	rm *.class
