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
