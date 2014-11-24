function locator=dbtmp(x)
% dbtmp - Save object to matbase under tmp subtree
%
% dbtmp :: A -> action locator(A).

dt=clock;
root=dbroot;
dir='tmp';
if ~exist(fullfile(root,dir),'dir')
	[rc,msg]=mkdir(root,dir);
% if rc==0, error(msg); end
end

fn=uniquefile(dt,root,dir,'m%s');	% make up a filename
save(fullfile(root,[fn '.mat']),'fn','x');
locator=[fn '|x'];

