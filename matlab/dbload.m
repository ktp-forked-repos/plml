% dbload - Load value from given matbase locator
%
% dbload :: locator(A) -> A.

% SA: 2008-06 - much simplified by using functional form of load
function x=dbload(loc)
	n=strfind(loc,'|');

	if n>1, 
		x=getfield(load(fullfile(dbroot,loc(1:n-1))),loc(n+1:end));
	else
		error('dbload:badlocator',sprintf('%s is not a valid locator',loc));
	end
