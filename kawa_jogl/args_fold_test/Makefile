.PHONY: all clean

all: args_fold_test.class

SCHEME_UTIL_PATH=../scheme_util
include $(SCHEME_UTIL_PATH)/scheme_util_include.mk

args_fold_test.class: scheme_util_general.class args_fold_test.scm
	java kawa.repl --main -C args_fold_test.scm

clean:
	rm *.class
