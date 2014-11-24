function fn=uniquefile(dt,root,dir,pattern)
% uniquefile - Allocate a unique unused filename
%
% uniquefile :: 
%	  [[1,6]] ~'date as returned by clock (currently not used)', 
%	  path    ~'implicit root directory', 
%	  path    ~'explicit directory relative to implicit root', 
%	  string  ~'filepath pattern with exactly one %s somewhere to accept id'
% -> path    ~'unique path relative to implicit root'.

% SA 2005-04-25 Seems that 10000 files per directory is not enough..
% SA 2008-06-27 Pattern must now contain %s, not %d.

	exists=1;
	numpat=sprintf(pattern,'%05d'); 
	while exists,
		fn=fullfile(dir,sprintf(numpat,floor(100000*rand)));
		exists=exist(fullfile(root,fn),'file');
	end


