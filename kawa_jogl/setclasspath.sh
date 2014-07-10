# This file is intended to be sourced via ". setclasspath.sh", rather than invoked via "chmod +x setclasspath.sh ; ./setclasspath.sh"
#function locate_jar
#{
#    locate $1 | head -n 1
#}
#function add_to_classpath
#{
#    export CLASSPATH="$(locate_jar $1)":$CLASSPATH
#}
# back up the classpath, unless we've already done that
if test "$OLD_CLASSPATH" = ""; then
    export OLD_CLASSPATH="$CLASSPATH"
fi
# robustly get the absolute location of this script
pushd . > /dev/null
cd $(dirname ${BASH_SOURCE[0]})
export SCRIPTPATH=$(pwd)
popd > /dev/null
# Java uses different classpath seperators on Windows and GNU/Linux, so
# check for presence of cygpath (a cygwin utility) to determine if on windows
if test "$(which cygpath)" = "";
then
function add_to_classpath
{
    export CLASSPATH="$1":$CLASSPATH
};
else
function add_to_classpath
{
    export CLASSPATH="$(cygpath -w "$1");$CLASSPATH"
};
fi
export CLASSPATH=.
add_to_classpath "${SCRIPTPATH}/classloaders/runnable-jogl-kawa7956-bundle.jar"
