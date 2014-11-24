function vn=uniquevar(x)
% uniquevar - Allocate a unique unused variable name
%
% uniquevar :: unit -> action string.
% uniquevar :: A~'initialied value'  -> action string.
%
% If no initial value is given the variable is NOT allocated.
% There are up to 100000 variable names available.

	exists=1;
	while exists,
		vn=sprintf('t_%05d',floor(100000*rand));
		exists=evalin('base',['exist(''',vn,''',''var'')']);
	end
	if nargin>0,
		assignin('base',vn,x);
	end


