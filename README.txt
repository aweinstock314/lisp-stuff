Descriptions of the various things:

netprog_chatserver:
This was a homework assignment for the Spring 2014 Network 
Programming course at RPI. The assignment description is at 
"http://www.cs.rpi.edu/~goldsd/spring2014-csci4220-projects.php"
 (project 3). We were given a list of languages that were 
acceptable to use without asking, and informed that we had 
the option to request permission to use a language not on 
the list. I asked permission to use scheme, and was granted 
it. During the summer (on June 17th, 2014, immediately 
prior to the start of an RCOS meeting) I asked Dr. 
Goldschmidt for permission to post it, which he granted.

heapsort.cl:
Contains an example implementation of heapsort written in 
both common lisp, and C++. This was written in to show 
that, while lisps do enable functional programming, it's 
also straightforward to transcribe C++ code directly (or 
write in that style). Also contains a definition of the 
C-style for loop as a macro, which expands to a tagbody/go 
construct (Lisp has GOTOs!). Heapsort was probably chosen 
since sorting algorithms seem to be the tier just above 
"hello world" for example programs.

chaining_macros.cl:
Two different macros for flattening nested function calls. 
These were both practice writing macros, and a response to 
a comment about nesting depth in lisp. (In retrospect, let* 
is probably cleaner and more general than both of the 
macros defined here, with the additional benefit of being 
built-in, and thus available everywhere).

with_list_collector.cl:
CL's loop macro's "collect" clause is very convenient 
sometimes, but isn't available seperately from loop. 
list-collector and with-list-collector provide that feature 
seperately. The earlier sketches seem horribly messy (even 
though they work) compared to the finished version.
