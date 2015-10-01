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


## OVERVIEW

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

### Caveats

There are some limitations to what you can do with this Matlab interface.

#### Restrictions on Matlab code
The communication protocol between the Matlab Engine library and the
Matlab process is somewhat flawed. if the Matlab computation outputs the
characters `flushed_stdout` at any point, the conversation between the
engine library and Matlab is irretrievable broken: the library gets out
of step with the stream coming from Matlab and there is no way to get back
into step. You have to close and reopen the Matlab instance and reopen.

#### Ctrl-C handling (UNIX only)
In Matlab, you can use Ctrl-C to interrupt a stuck or long running computation.
Matlab respons to the INT signal generated when you press Ctrl-C at the 
terminal. What happens when you press Ctrl-C during a computation when using this plml
library depends on which thread the call was made. If it is the foreground
thread, the INT signal interrupts BOTH the Matlab computation (because the Matlab 
process is in the same Unix process group) AND the current
Prolog thread, which will be in the depths of a call to the Matlab Engine API.
This causes the call to return an error, but the Engine API loses synchonisation
with the input stream from Matlab and the conversation is irretrievably broken.
I'm afraid there's nothing we can do about this without interposing a filter
a filter between the Matlab process and the engine library.

If the Matlab call is made in a background thread, Ctrl-C interrupts the foreground
Prolog thread, but not the engine API call. The INT signal is still sent to 
the Matlab process, so it is interrupted and the engine API call returns early.

The same effect can be obtained by sending an INT signal directly to the Matlab
process. You can use the Matlab function `feature` to get Matlab's process ID.

However, there is still a problem with detecting that Matlab has been interrupted.
In some cases, there is no observable difference between completion and 
interruption (try pressing Ctrl-C while running `pause(10)` in Matlab directly).
In others, Matlab will print a message to stderr, and then stop with no other
observable effect, in particular `lasterr` will not return anything useful.
The engine API does not observe Matlab's stderr, and so there is no way to
detect that a Matlab call has been interrupted. To get round this, plml adds
the command `disp("}")` to the end of each Matlab execution. It then checks
to see if "}\n" appears at the end of the output. If not, we conclude that the
computation was interrupted. This is fine unless the call produces so much output
that it doesn't fit into the output buffer. In this case, the check is
skipped, and so checking for interruptions will not work.
An 'output truncated' message is also printed on stderr.


## BUILDING/INSTALLATION

Before you start, you need a working SWI Prolog installation and
a Matlab installation. Then the `pack_install/1` facility of SWI
Prolog should be enough to build the C++ code foreign library.

### RESOLVING LINKS TO MATLAB LIBRARIES ON MAC OS X

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

### SANITY CHECK

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

### BUILD OPTIONS

There are two configuration options in the Makefile. If you enable DEBUG mode
and rebuild, then the library will print a series of cryptic characters as
it aquires and releases the mutexes protecting the WS variable release queue
and the Matlab engine.

If you enable NOLOCK, the mutex protecting the Matlab engine will be disabled.
You might want this if you are sure that the library is going to be called in a
single threaded way, but probably any performance gain will be miniscule.

## ACKNOWLEDGMENTS

This work was partially supported by UK EPSRC grants GR/S84750/01 and
GR/S82213/01.

