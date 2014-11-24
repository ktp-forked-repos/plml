function [p,h]=dbroot(newroot, newhost)
% dbroot - Set or retrieve Matbase root directory
%
% dbroot :: unit -> string ~'current root', string ~'current name'.
% dbroot :: string ~'new root' -> action unit.
% dbroot :: string ~'new root', string ~'new host' -> action unit.
%
% The matbase system uses the root and host name to decided where to
% put the MAT files saved by dbsave, dbsaveas and dbtmp. A directory named
% after the host is created under the given root. Directories based on
% the current date are created under the per-host directories. This means
% the root can be on a shared filesystem as long as each host has a unique
% name.
%
% Note that dbroot MUST be called at least once before the matbase is used
% otherwise the results are undefined.
%
% If no hostname is given, dbroot will attempt to read the HOSTNAME
% environment variable. If this is empty, a error will be raised.

global DBROOT
global HOSTNAME

if nargin>=1, 
	DBROOT=newroot; 
	if nargin<2, newhost=getenv('HOSTNAME'); end
	if isempty(newhost)
		[rc,hostname]=system('hostname');
		newhost=strtrim(hostname);
		if isempty(newhost)
			error('dbroot:nohostname','Could not get host name');
		end
	end
	HOSTNAME=[newhost filesep]; 
else
	% no input arguments so return current values
	p=DBROOT;
	h=HOSTNAME;
end


