.PHONY: all run clean
all: music_test.class music_keyboard.class

SCHEME_UTIL_PATH=../scheme_util
include $(SCHEME_UTIL_PATH)/scheme_util_include.mk

run: music_test.class
	java music_test

music_test.class: scheme_util_general.class scheme_util_math.class music_test.scm
	java kawa.repl --main -C music_test.scm

music_keyboard.class: scheme_util_general.class scheme_util_math.class music_keyboard.scm
	java kawa.repl --main -C music_keyboard.scm

clean:
	rm *.class
