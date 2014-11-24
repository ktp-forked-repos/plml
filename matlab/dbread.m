% dbread - Load values from ASCII file under matbase tree.
%
% dbread :: string -> A.
%
% This looks for a file given a path relative to the current
% matbase root as returned by dbroot. It then attempts to
% read it as an ASCII file using READ.

function x=dbread(file), x=read(fullfile(dbroot,file));
	
