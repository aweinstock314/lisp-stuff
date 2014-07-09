.PHONY: all clean
all: RecursiveJarClassloader.class RecursiveJarUtil.class RunWithClassloader.class RunWithRecursiveJar.class

RecursiveJarClassloader.class: RecursiveJarUtil.class RecursiveJarClassloader.java
	javac RecursiveJarClassloader.java

RecursiveJarUtil.class: RecursiveJarUtil.java
	javac RecursiveJarUtil.java

RunWithClassloader.class: RunWithClassloader.java
	javac RunWithClassloader.java

RunWithRecursiveJar.class: RecursiveJarUtil.class RunWithRecursiveJar.java
	javac RunWithRecursiveJar.java

clean:
	rm *.class
