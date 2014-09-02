.PHONY: all clean
all: scheme_util_general.class scheme_util_math.class scheme_util_networking.class

scheme_util_general.class: scheme_util_general.scm
	java kawa.repl -C scheme_util_general.scm

scheme_util_math.class: scheme_util_math.scm
	java kawa.repl -C scheme_util_math.scm

scheme_util_networking.class: scheme_util_networking.scm
	java kawa.repl -C scheme_util_networking.scm

clean:
	rm *.class
