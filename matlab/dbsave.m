function locator=dbsave(x)
% dbsave - Save object to MatBase using given name
%
% dbsave :: A -> action locator(A).

% SA: 2008-05-20 - no longer saving file name in mat file

dt=clock;
[root,host]=dbroot;
dir=[host dirname(dt)];	% makes up a directory name based on the date.
if ~exist(fullfile(root,dir),'dir')
	[rc,msg]=mkdir(strrep(root,'~',getenv('HOME')),dir);
end

fn=uniquefile(dt,root,dir,'m%s.mat');	% make up a filename
save(fullfile(root,fn),'x');
locator=[removeext(fn),'|x'];

% make up directory name based on the year and month
function dir=dirname(dt)
	dir=sprintf('d%s%s',datestr(dt,'yy'),datestr(dt,'mm'));
	

