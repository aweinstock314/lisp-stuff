.PHONY: all clean
all: RecursiveJarClassloader.class RecursiveJarUtil.class RunWithClassloader.class RunWithRecursiveJar.class runnable-jogl-kawa-bundle.jar

RecursiveJarClassloader.class: RecursiveJarUtil.class RecursiveJarClassloader.java
	javac RecursiveJarClassloader.java

RecursiveJarUtil.class: RecursiveJarUtil.java
	javac RecursiveJarUtil.java

RunWithClassloader.class: RunWithClassloader.java
	javac RunWithClassloader.java

RunWithRecursiveJar.class: RecursiveJarUtil.class RunWithRecursiveJar.java
	javac RunWithRecursiveJar.java

runnable-jogl-kawa-bundle.jar: RecursiveJarUtil.class RunWithRecursiveJar.class runnable-bundle-manifest.mf jogl-kawa-bundle.jar
	jar cfm runnable-jogl-kawa-bundle.jar runnable-bundle-manifest.mf jogl-kawa-bundle.jar RecursiveJarUtil.class RunWithRecursiveJar.class RunWithRecursiveJar\$$1.class

clean:
	rm *.class
