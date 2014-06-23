# This file is intended to be sourced via ". setclasspath.sh", rather than invoked via "chmod +x setclasspath.sh ; ./setclasspath.sh"
function locate_jar
{
    locate $1 | head -n 1
}
function add_to_classpath
{
    export CLASSPATH="$(locate_jar $1)":$CLASSPATH
}
# back up the classpath, unless we've already done that
if test "$OLD_CLASSPATH" = ""; then
    export OLD_CLASSPATH="$CLASSPATH"
fi
export CLASSPATH=.
add_to_classpath "jogl-all.jar"
add_to_classpath "jogl-all-natives-linux-i586.jar"
add_to_classpath "gluegen-rt.jar"
add_to_classpath "gluegen-rt-natives-linux-i586.jar"
add_to_classpath "kawa-1.14.jar"
