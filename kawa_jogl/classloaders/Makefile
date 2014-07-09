.PHONY: all clean
all: RecursiveJarClassloader.class RunWithClassloader.class

RecursiveJarClassloader.class: RecursiveJarClassloader.java
	javac RecursiveJarClassloader.java

RunWithClassloader.class: RunWithClassloader.java
	javac RunWithClassloader.java

clean:
	rm *.class
