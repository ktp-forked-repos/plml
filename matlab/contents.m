% *** db :: functions for dealing with MAT file database
%
% The Prolog-Matlab interface module plml requires these functions
% to manage a database of MAT files and a few other tasks. The MAT
% file database (matbase) is stored in the file system under a root
% directory specified by a call to dbroot, eg to specify the 
% directory lib/matbase off your home directory on a host named 
% 'godzilla' (the host name must be supplied to allow for multiple
% hosts to use the same root directory):
%
%    dbroot('~/lib/matbase','godzilla');
%
% From this point, any calls to dbsave will result in files being
% created under ~/lib/matbase/godzilla. MAT files in the matbase
% are referred to using a locator, which is a string of the form
%
%    <host>/<path>/<filename>|<varname>
%
% This refers to a Matlab variable named <varname> in a MAT file
% whos path is constructed using the matbase root, the host name,
% and the <path> component of the locator. Eg, after the above
% dbroot statement, the locator
% 
%    'godzilla/d0608/m34521|x'
%
% refers to the MAT file ~/lib/matbase/godzilla/d0608/m34521.mat
% NOTE: this is probably not going to work for Windows users due
% to silly slash vs backslash file separator issues. 
% The functions in this directory are:
%
% dbdrop     - delete matfile at given locator from matbase
% dbload     - Load value from given matbase locator
% dbpath     - Return full path of matfile given locator
% dbread     - Load values from ASCII file under matbase tree
% dbroot     - Set or retrieve Matbase root directory
% dbsave     - Save object to MatBase using given name
% dbsaveas   - Save object to MatBase using given name
% dbtmp      - Save object to matbase under tmp subtree
% typecode   - Return basic typing information about a value
% uniquefile - Allocate a unique unused filename
% uniquevar  - Allocate a unique unused variable name

