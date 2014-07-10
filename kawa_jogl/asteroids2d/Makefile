.PHONY: all clean run
all: asteroids_util.class asteroids2d.class

run:
	java RunWithRecursiveJar asteroids2d

asteroids_util.class: asteroids_util.scm
	java RunWithRecursiveJar kawa.repl -C asteroids_util.scm

asteroids2d.class: asteroids_util.class identityshader.vert identityshader.frag asteroids2d.scm
	java RunWithRecursiveJar kawa.repl --main -C asteroids2d.scm

clean:
	rm *.class
