.PHONY: all clean
all: asteroids_util.class asteroids2d.class

asteroids_util.class: asteroids_util.scm
	java kawa.repl -C asteroids_util.scm

asteroids2d.class: asteroids_util.class identityshader.vert identityshader.frag asteroids2d.scm
	java kawa.repl --main -C asteroids2d.scm

clean:
	rm *.class
