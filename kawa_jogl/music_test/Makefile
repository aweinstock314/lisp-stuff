.PHONY: all run clean
all: music_test.class

run: music_test.class
	java music_test

scheme_util_general.class: ../scheme_util/scheme_util_general.scm
	cd ../scheme_util && make scheme_util_general.class
	cp -t . ../scheme_util/scheme_util_general*.class
scheme_util_math.class: ../scheme_util/scheme_util_math.scm
	cd ../scheme_util && make scheme_util_math.class
	cp -t . ../scheme_util/scheme_util_math*.class

music_test.class: scheme_util_general.class scheme_util_math.class music_test.scm
	java kawa.repl --main -C music_test.scm

clean:
	rm *.class
