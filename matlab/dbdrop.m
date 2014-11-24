function dbdrop(loc)
% dbdrop - delete matfile at given locator from matbase
%
% dbdrop :: locator(A) -> action unit.
%
% The files containing the specified locators are deleted from
% the file system. If the file pointed to by the locator does
% not exist, a warning is given but the function completes.

% SA 2008-06 - No longer maps over multiple arguments.

	n=strfind(loc,'|');
	if n>1, matname=loc(1:n-1); else matname=loc; end
	fn=fullfile(dbroot,[matname '.mat']);

	if exist(fn,'file')
		fprintf('*** DELETING FILE: %s\n', fn);
		delete(fn);
	else
		fprintf('*** Warning: %s does not exist\n', fn);
	end
	
