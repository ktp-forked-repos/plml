function locator=dbsaveas(fn,x)
% dbsaveas - Save object to MatBase using given name
%
% dbsaveas :: string, A -> action locator(A).
%
% Note: the file must not already exist; an error is raised if it does.

fullfn=fullfile(dbroot,[fn,'.mat']);
if exist(fullfn,'file'), error('dbsaveas:file exists'); end
save(fullfn,'x');
locator=[fn '|x'];
