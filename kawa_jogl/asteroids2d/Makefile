.PHONY: all clean run jar
all: asteroids_util_opengl.class asteroids2d.class
jar: asteroids2d_distribution.jar

scheme_util_general.class: ../scheme_util/scheme_util_general.scm
	cd ../scheme_util && make scheme_util_general.class
	cp -t . ../scheme_util/scheme_util_general*.class
scheme_util_math.class: ../scheme_util/scheme_util_math.scm
	cd ../scheme_util && make scheme_util_math.class
	cp -t . ../scheme_util/scheme_util_math*.class

run:
	java asteroids2d

asteroids_util_opengl.class: scheme_util_general.class scheme_util_math.class asteroids_util_opengl.scm
	java kawa.repl -C asteroids_util_opengl.scm

asteroids2d.class: scheme_util_general.class scheme_util_math.class asteroids_util_opengl.class identityshader.vert identityshader.frag asteroids2d.scm
	java kawa.repl --main -C asteroids2d.scm

launcher.class: launcher.java
	javac launcher.java

asteroids2d_distribution.jar: asteroids2d.class launcher.class launcher_manifest.mf
	mkdir buildtmp
	cp -t buildtmp  ../classloaders/jogl-kawa7956-bundle.jar ../classloaders/*.class *.class launcher_manifest.mf
	cd buildtmp && jar cmf launcher_manifest.mf asteroids2d_distribution.jar jogl-kawa7956-bundle.jar *.class
	mv buildtmp/asteroids2d_distribution.jar .
	rm -r buildtmp

clean:
	rm *.class asteroids2d_distribution.jar
