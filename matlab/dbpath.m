function p=dbpath(loc)
% dbpath - Return full path of matfile given locator.
%
% dbpath :: locator(A) -> path.

	n=strfind(loc,'|');
	p=[fullfile(dbroot,loc(1:n-1)) '.mat']; 
	
