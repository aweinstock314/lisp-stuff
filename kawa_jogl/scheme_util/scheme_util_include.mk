scheme_util_general.class: $(SCHEME_UTIL_PATH)/scheme_util_general.scm
	cd $(SCHEME_UTIL_PATH)/ && make scheme_util_general.class
	cp -t . $(SCHEME_UTIL_PATH)/scheme_util_general*.class
scheme_util_math.class: $(SCHEME_UTIL_PATH)/scheme_util_math.scm
	cd $(SCHEME_UTIL_PATH)/ && make scheme_util_math.class
	cp -t . $(SCHEME_UTIL_PATH)/scheme_util_math*.class
