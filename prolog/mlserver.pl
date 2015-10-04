/* Part of plml
	Copyright 2014-2015 Samer Abdallah, University of London
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(mlserver,
		[	start_matlab/0
      ,  start_matlab/1
      ,  stop_matlab/0
      ,  ml_interrupt/0
      ,  ml_async/2
      ,  ml_async/3
      ,  (??)/1
      ,  (???)/1
      ,  (===)/2
		]).

% !!! when should we garbage_collect_atoms?
:- set_prolog_flag(double_quotes,string).

:- reexport(library(plml), except([(??)/1, (???)/1, (===)/2])).
:- use_module(library(settings)).
:- use_module(library(memo)).

:- initialization catch(mutex_create(_,[alias(mlclient)]),Ex,print_message(warning,Ex)).

:- setting(exec_timeout,number,1800,'Matlab command execution timeout in seconds').
:- setting(eval_timeout,number,1800,'Matlab expression evaluation timeout in seconds').

:- dynamic current_matlab_thread/2.

start_matlab :- start_matlab([]).
start_matlab(Opts) :-
   (  current_thread(matlab,running) 
   -> debug(mlserver(control),'Matlab server thread already running',[])
   ;  (current_thread(matlab,_) -> thread_join(matlab,_); true),
      thread_create(ml_server(Opts),_,[alias(matlab)])
   ).

stop_matlab :-
   (  current_matlab_thread(TID,_) -> kill_thread(TID)
   ;  debug(mlserver(control),'No Matlab thread running.',[])
   ).


%% +Returns:ml_vals(A) === +Expr:ml_expr(A) is det.
%  Asynchronous Matlab evaluation. Anything printed by Matlab code is output
%  to the current_output stream of the Matlab server thread.
[X1|Xs]===Expr :- !, setting(eval_timeout,T), ml_async(eval(Expr,[X1|Xs]),T).
X===Expr :- !, setting(eval_timeout,T), ml_async(eval(Expr,[X]),T).

%% ??(+Cmd:ml_stmt) is det.
%  Asynchronous Matlab execution. Anything printed by Matlab code is captured
%  and printed to the current output of this thread (in blue).
?? Cmd :- 
   setting(exec_timeout,T), 
   ml_async(exec(Cmd),T,O), 
   ansi_format([fg(blue)],'~s',[O]).

%% ???(+Expr:ml_expr(bool)) is det.
%  Asynchronous Matlab test. Equivalent to bool(1)===Expr.
??? Expr :- bool(1)===Expr.

   
kill_thread(Thread) :-
   (  current_thread(Thread,running)
   -> thread_send_message(Thread,quit),
      debug(mlserver(control),'waiting for thread ~w to die.\n',[Thread]),
      catch( call_with_time_limit(5,
         (thread_join(Thread,RC), writeln(exit_code(RC)))),
         time_limit_exceeded,
         debug(mlserver(control),'timeout waiting for thread ~w.\n',[Thread]))
   ;  true
   ).


%% ml_server(Opts) is det.
ml_server(Opts) :-
   setup_call_cleanup(ml_open(ml,localhost,Opts), ml_run, ml_close(ml)).
   
ml_run :- 
   thread_self(Self),
   ml_eval(ml,feature("getpid"),[int],[PID]),
   setup_call_cleanup( 
      assert(current_matlab_thread(Self,PID)),
      ml_server_loop,
      retract(current_matlab_thread(Self,PID))).

ml_server_loop :-
	thread_get_message(Msg),
	(	Msg=quit -> debug(mlserver(server),'Matlab server thread terminating.', [])
	;	Msg=req(Client,ID,Req,Reply)
   -> debug(mlserver(server),'handling ~w: ~W',[ID,Req,[quoted(true),max_depth(8)]]),
      memo:reify(mlserver:handle_request(Req),Status), !,
      debug(mlserver(server),'result is ~q',[Status]),
      thread_send_message(Client,resp(ID,Status,Reply)),
		ml_server_loop
	).

handle_request(exec(Cmd))     :- ml_exec(ml,Cmd).
handle_request(eval(E,Ts,Vs)) :- ml_eval(ml,E,Ts,Vs).
handle_request(output(R,O))   :- with_output_to(string(O),handle_request(R)).

%% ml_async(+Req, +Timeout:float, -Output:string) is det.
%% ml_async(+Req, +Timeout:float) is det.
%
%  Use Matlab server thread to do computation described by Req. Req can be
%     *  exec(Cmd:ml_stmt)
%     *  eval(Expr:ml_expr(A),Returns:ml_vals(A))
%
%  If the computation takes longer than Timeout seconds, Matlab is interrupted
%  and abort(timeout) is thrown. The computation can also be interrupted
%  by signalling the client thread with =|throw(abort(Reason))|=, where Reason 
%  can be anything. If third argument Output is supplied, anything printed by
%  Matlab computation is captured and returned as a string.
ml_async(R,T) :- req(R,Req,Reply), ml_request(Req,Reply,T).  
ml_async(R,T,O) :- req(R,Req,Reply), ml_request(output(Req,O),Reply-O,T).  


req(exec(Cmd),exec(Cmd),_) :- !.
req(eval(Expr,Rets),eval(Expr,Types,Vals),Vals) :-
   maplist(leftval,Rets,Types,Vals), !.
req(R,_,_) :- throw(invalid_mlserver_request(R)).


%% ml_request(+Req, ?Reply, +Timeout) is det.
%
%  Send a request to Matlab server thread. When complete, the Reply term
%  is sent back to this thread.
%  Request can be:
%     *  exec(Cmd)
%     *  eval(Expr,Types,Vals)
%
%  Reply can be any term, possibly sharing variables with Req. This is used
%  to pick out which valuesfrom an eval(_,_,_) request are returned.
%  NB the mutex ensures that if we have to interrupt due to timeout, then we
%  only interrupt our own Matlab computation, not some other one.
ml_request(Req,Reply,Timeout) :- 
   gensym(ml,ID), 
   with_mutex(mlclient,ml_request(ID,Req,Reply,Timeout)).

ml_request(ID,Req,Reply,Timeout) :-
	thread_self(Self),
   (current_matlab_thread(Matlab,_) -> true; throw(matlab_not_running)),
   debug(mlserver(client),'Sending request ~w: ~W.',[ID,Req,[max_depth(8)]]),
   setup_call_catcher_cleanup(
      thread_send_message(Matlab,req(Self,ID,Req,Reply)),
      (  thread_get_message(Self,resp(ID,Status,Reply),[timeout(Timeout)])
      ;  throw(abort(timeout))
      ),
      exception(abort(Reason)),
      interrupt_cleanup(Reason,ID)
   ), !,
   debug(mlserver(client),'Got response ~w: ~w.',[ID,Status]),
   memo:reflect(Status).

interrupt_cleanup(Reason,ID) :-
   debug(mlserver(client),'request (~w) interrupted due to: ~w.',[ID,Reason]),
   ml_interrupt,
   thread_get_message(resp(ID,Status,_)),
   debug(mlserver(client),'request (~w) got post-~w response: ~w.',[ID,Reason,Status]).

%% ml_interrupt is det.
%  Sends a SIGINT (interrupt) signal to the Matlab process.
ml_interrupt :-
   debug(mlserver(client),'Interrupting MATLAB.',[]),
   (current_matlab_thread(_,PID) -> true; throw(matlab_not_running)),
   process_kill(PID,int).

