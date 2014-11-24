plml
====

Prolog-Matlab bridge for SWI Prolog

 Authors (2004--2012)
    Samer Abdallah
    Centre for Digital Music, 
    Queen Mary, University of London,

    Christophe Rhodes
    Centre for Computational Creativity
    Goldsmiths College, University of London


##OVERVIEW

PLML is a foreign interface that enables Matlab to be used as a computational
engine from within SWI Prolog. The basic idea is that instead of using
the standard is/2 operator to evaluate a certain class of terms, we can
use the ===/2 operator to get Matlab to evaluate a (much richer) class of
terms, eg 

	?- float(A)===trace(eye(3)).

	A = 3.0

We can also get Matlab to perform actions with side effects, like
making sounds and graphics; obviously these do not fit into the declartive
semantics of Prolog and have to be dealt with under the procedural semantics.
If you want to execute a Matlab command in an imperative way and see the
textual output, use the ??/1 operator, eg

	?- ??disp(`hello).
	>> hello



The interface works by using the Matlab Engine API, which starts up a Matlab
process on the end of a pipe. The Matlab process can be on another machine,
and multiple Matlab engines can be started on the same or different machines.
Matlab expressions are sent down the pipe and executed. Matlab's textual
output comes back through the pipe. In addition, Matlab variables can be
transferred directly between the Matlab engine's memory space and SWI's
memory space.


### Expression language

Expressions to evaluate are given in a sublanguage of terms which is
similar to but not exactly the same as Matlab. In particular, Prolog
syntax cannot accommodate the single quoted Matlab strings, (since these
are equivalent to unquoted Prolog atoms),
the Matlab syntax of matrices, (eg [1 2; 3 4]), and the Matlab syntax
for slicing arrays (eg A(:,3:4)) if A is a Prolog variable.
Strings are handled using the q/1 or, if the SWI Prolog flag back_quotes
is set to symbol_char, `/1 functor, ie `hello or q(hello)
both evaluate to the Matlab string 'hello'. Arrays can be given either as 
flat lists, which are interpreted as horizontal concatenation as in Matlab:

	?- ??[1,2,3].
	>> ans = 1 2 3

	?- ??[eye(2),magic(2)].
	>> ans =
		1  0  1  3
		0  1  4  2

or as nested listed for multidimensional arrays using the arr/1 functor,
where the innermost nesting corresponds to the FIRST Matlab dimensions

	?- ??arr([1,2,3]).
	>> ans = 
		1
		2
		3

	?- ??arr([[1,2],[3,4]]).
	>> ans =
		1  3
		2  4

Cell arrays can be specified in a similar way using braces or the cell/1 functor.


To help with accessing array elements, see the Matlab functions general/paren,
general/row, and general/col in the matlab directory.
		


### Return values

The results of computations can handled in several ways:

1.	Keep the result in a Matlab workspace variable in the engine's memory
	space. The names of these variables are allocated automatically
	and stored in a Prolog atom. The atoms have a garbage collection
	callback which means that the Matlab workspace variable is (eventually)
	deleted if the Prolog atom goes out of scope.

	?- A===2+2, ??disp(A).
	>> 4                  % matlab textual output

	A = ws(<ml:t_2311>)   % Prolog blob pointing to Matlab variable t_2311
	
	

2. Convert the result to a prolog atom or term. The type of the resulting
	prolog term depends on the *right hand side* of the ===/2 operator:

	?- int(A)===2+2.
	A = 4

	?- float(A)===2+2.
	A = 4.0


	There are other types for strings and atoms:

	?- atom(A) === q(hello).   % q/1 means quote as Matlab string
	A = hello.

	?- string(A) === `hello.   % `/1 is shorthand for q/1
	A = "hello".


	You can also get the result as a Matlab binary array on the Prolog side:

	?- mx(A)===eye(4).   % identity matrix
	A = <#0239c3a0>      % Prolog blob handle (with garbage collection)
	

	I haven't completely settled on the best way of handling arrays as
	self-contained Prolog terms, but you can do this:

	?- array(A)===magic(3).
	A = [[8.0, 3.0, 4.0], [1.0, 5.0, 9.0], [6.0, 7.0, 2.0]]::[[3, 3]]

	As you can see, multidimensional arrays are returned as nested lists, and the
	size of the array is given after the :: as [[3,3]].


3.	Store the result to a MAT file and return a Prolog term which points to the
	file. The names are generated automatically. This allows for persistence
	of values which are referred to by stable names that can be stored, eg
	in a database:

	?- mat(A)===fft(buffer(wavread('somefile.wav'),256,128)).

	A = mat:d0608/m48598|x  % dynamically generated unique locator

	This relies on the mechanism provided by the functions in matlab/db.
	A certain directory is designed the root of a 'matbase' (MAT file database).
	The default is ~/var/matbase (ie under the user's home directory).
	The Matlab function dbroot returns or sets this directory:

	?- ??dbroot.
	>> 
	ans = 

	/Users/samer/matbase

	?- ??dbroot(q('/usr/share/lib/matbase')).  % switch to shared matbase
	>> 
	ans =

	/usr/share/lib/matbase

	In this case, the locator mat:d0608/m48598|x refers to a Matlab variable called
	'x' (it's always 'x') in the file /usr/share/lib/matbase/d0608/m48598.mat.
	A new directory is created each month, and the filenames are chosen dynamically
	to avoid clashes with existing files.
	


### Debugging/tracing

To help with debugging, you can issue the command:

	?- debug(plml).

which will cause each Matlab expression to be printed in its Matlab form 
before execution.
I'm afraid the best documentation is the code itself, but I do intend to
produce a manual once some of the more embarrassing aspects of the system
are resolved!



##BUILDING/INSTALLATION

Before you start, you need a working SWI Prolog installation and
a Matlab installation. Then the `pack_install/1` facility of SWI
Prolog should be enough to build the C++ code foreign library.

###RESOLVING LINKS TO MATLAB LIBRARIES ON MAC OS X

As it stands, there is a minor difficulty when attempting to load
the plml.dylib foreign library: because of the way the
Matlab application is designed, plml.dylib ends up looking for 
various Matlab libraries (libeng, libmx, etc.) using _relative_
paths, not absolute paths, which means that if you simply start
SWI Prolog and load the plml module, the foreign library fails to
load. There are two solutions to this:

1. Set the DYLD_FALLBACK_LIBRARY_PATH environment variable _before_
	starting SWI Prolog. A shell script for doing this, called `swiplml`
	can be found in the `scripts` directory of this package. If you 
	copy or link it into your `PATH`, and use `swiplml` instead of
	`swipl`, it should work fine.

2. A minor adjustment of some the Matlab libraries can made, changing
	them to use the `@rpath` mechanism of `dyld` means that using
	the `-rpath` option while linking plml.dylib solves the problem.
	Although some of Matlab's installed files are altered, this does
	not seem to affect the normal running of Matlab.

	The `fixdylibs` pseudo-target of this package's Makefile will
	do this for you, but as it may require root priviledges, it uses
	`sudo` and therefore requires that you have administrator rights.
	The changes are made using `scripts/fixdylibs`, which will also
	make backups of your original Matlab libraries if they do not
	yet exist.

###SANITY CHECK

If the installation is ok and you have followed one of the two options
above, then you should be able to start SWI Prolog and do the following:

	?- use_module(library(plml)).
	% ...
	Yes.

	?- ml_open(ml).  % ml is the name assigned to the Matlab engine instance
	Matlab engine (ml) open.

	Yes

	?- float(A)===2*pi.
	A = 6.28319 

	?- A:float===2*pi.
	A = 6.28319 

If you have a working X-server available, then the following should result
in a straight line plot.

	?- ?? figure(1).
	% ...
	?- ?? plot(1:10).


##CHANGES

12/04 - Using Prolog blobs with garbage collection to handle
		Matlab workspace temporary variables. Works but code is
		still a little messy. Would like to unify variable handling-
		the important functions are alloc, free, get, put.
		Also, garbage collection seems to be rather difficult to 
		provoke.

2005-08-08
	Handle Matlab errors in mlEXEC by setting lasterr() before and
		checking it after engEvalString.  If we do get a Matlab
		error, then throw a Prolog exception, because nothing
		else is safe in general.  CSR.

2005-08-09
	Be a little more paranoid in handling workspace variables, both
		in terms of checking matlab engine error codes and for
		bounds checking of our own functions such as uniquevar.

2005-09-26
	Added matbase_mat/1 to enumerate all mat objects actually in
	the file system pointed to by the matlab function dbroot.

2005-11-11
	Now sending very long Matlab commands via a char array
	Progress in Prolog-side mxArray support:
	done:
		MXINFO - get size and type of array
		MXSUB2IND - convert multi-dim subscript to linear index
		MXGETFLOAT - get one element of numeric array (must be real)
		MXGETLOGICAL - get element of logical or 0|1 array
		MXGETCELL - get sub mxArray from cell array
		MXGETREALS - get reals part of all elements as flat Prolog list
		MXCREATENUMERIC - create double array
		MXCREATECELL - create cell array
		MXCREATESTRING - create char array from string or atom
		MXPUTFLOAT - write one element of double array
		MXPUTFLOATS - write list of elements to double array
		MXPUTCELL  - put an mxArray into a cell array
		MXCOPYNOGC - deep copy of array, return NON-MANAGED mx ref
		MXNEWREFGC - return memory managed ref to array, ie will be GCed
	to do:
		Imaginary parts?
		Reading list of fields from a structure
		Getting cell array contents as a list
		tidy up: error checking and function names
		possibly reduce the amount of bounds checking to improve speed?
		-> need to do proper profiling!

2006-11-28
	Errors generated on the matlab side (ie errors in user functions
	rather than the mechanisms of this library) throw exceptions of
	the form mlerror(Engine,Message) instead of just atomic messages.
	
	Some changes to plml.pl - see header in that file for details.

2008-11-25
	Moved declaration of \ operator to ops.pl
	Changed interface and implementation of ml_open to use list of options.
	Changed build procedure to use build script and two makefiles.

2010-02-25
	Replaced use of mxFree with mxDestroyArray to release resources 
	obtained using engGetVariable - this was causing a malloc error
	in Matlab 7.9. Also replaced -nojvm with -noawt option  when starting
	Matlab, as -nojvm is no longer supported. Apparently they're going
	to withdraw support for X11 graphics at some point. I hate them.
	I'm not 'upgrading' any more.

2010-03-19
	Removed dependency on flists

2010-05-30
	Merged hostname module into utils.

2012-01
	Big overhaul of Prolog part to simplify and speed up.
	Removed unused Matlab functions from matlab/general.
	Version 1!

2012-02
	Some changes to mlExec and mlWSAlloc to get around a problem that
	was bedevilling the triserver project, which was using a Matlab
	engine in a separate thread, receiving requests on a message queue,
	and calling this library with call_with_time_limit. Something to
	do with this combination (threads, signals etc) was causing the
	Prolog system to lock up hard on returning to Prolog system
	after a failing call to engGetVariable (which was getting stuck and
	timing out).

	I still don't know what causes the lock-up, but I was able to detect
	some signs that it was coming *before* calling engGetVariable by looking
	at the Matlab engine output buffer.

	Eventual work around is not to use engGetVariable to get uniquevar
	variable names (mlWSAlloc) or lasterr (mlExec) at all, but simply
	to scrape these out of the output buffer. This seems to be a few
	milliseconds faster too.

	other changes: removed ml_debug/1 - would rather have a flag to
	enable printfs in the C++ source.

2012-02-06
	Yes, finally, got to the bottom of the locking up bug.
	The problem was mutlithreaded access to the Matlab engine even when
	making explicit calls only in one thread IF this is a secondary thread.
	The reason? Garbage collection in the main thread. Have now added
	a mutex to protect the Matlab engine (currently one global mutex).
	If garbage collector is run on a workspace variable blob while the 
	mutex is locked, it fails immediately without blocking. The blob
	is not reclaimed but will be can be reclaimed the next time gc is run.

2012-02-09
	Now using library(debug) for debugging messages. Added more helpful
	message if foreign library fails to load.

RELEASE NOTES for version 0.2

	- Added option to enable Matlab's JVM
	- Now closing Matlab engines properly at halt
	- Added support for valid but non-evaluable expressions
	- Fixed bug when returning integers from Matlab
	- Errors in user's Matlab functions now generate mlerror(_,_) expections

##ACKNOWLEDGMENTS

This work was partially supported by UK EPSRC grants GR/S84750/01 and
GR/S82213/01.

