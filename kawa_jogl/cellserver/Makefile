.PHONY: all run clean
all: cellserver.class

SCHEME_UTIL_PATH=../scheme_util
include $(SCHEME_UTIL_PATH)/scheme_util_include.mk

run: cellserver.class
	java cellserver

cellserver.class: scheme_util_general.class scheme_util_math.class cellserver.scm
	java kawa.repl --main -C cellserver.scm

clean:
	rm *.class
