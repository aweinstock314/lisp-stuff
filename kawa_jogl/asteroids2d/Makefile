.PHONY: all clean run
all: asteroids_util_general.class asteroids_util_math.class asteroids_util_opengl.class asteroids2d.class

run:
	java asteroids2d

asteroids_util_general.class: asteroids_util_general.scm
	java kawa.repl -C asteroids_util_general.scm

asteroids_util_math.class: asteroids_util_math.scm
	java kawa.repl -C asteroids_util_math.scm

asteroids_util_opengl.class: asteroids_util_opengl.scm
	java kawa.repl -C asteroids_util_opengl.scm

asteroids2d.class: asteroids_util_general.class asteroids_util_math.class asteroids_util_opengl.class identityshader.vert identityshader.frag asteroids2d.scm
	java kawa.repl --main -C asteroids2d.scm

clean:
	rm *.class
