This directory contains programs using GNU Kawa's scheme 
dialect that also depend on the JOGL OpenGL bindings for 
java. "set_classpath.sh" is intended to be sourced rather 
than executed conventionally, it searches for the relevant 
jars, and adds them to the classpath environment variable. 
It uses the 'locate' command, so if it can't find the right 
jars immediately after installing JOGL, either run 
'updatedb' as root, or wait for cron to update the database 
used by locate.

Websites of dependencies:
http://www.gnu.org/software/kawa/
http://jogamp.org/

Descriptions of subprojects:
rainbow_trangle_demo:
A simple "hello world" style program that instantiates an 
OpenGL window and displays a triangle in it, and arranges 
for the triangle to be recolored randomly 5 times per 
second.

asteroids2d:
Asteroids game.

classloaders:
Some code in java, depending on just the standard library, 
that should make it trivial to use a hierarchical jar 
structure to distribute dependencies (as opposed to 
"messier" solutions such as distributing seperate jars, or 
manually flattening the dependencies into a single jar.)

music_test:
A demo that plays a simple sweeping tune with a background 
drum pattern via the javax.sound.midi package. Also 
contains a keyboard that plays midi via the same mechanism 
based on keys pressed.

args_fold_test:
A simple wrapper around the SRFI-37 "args-fold" command 
that does command line processing.

scheme_util:
A bunch of functions and macros that were written in the 
process of writing asteroids that seem to be useful in 
general.
