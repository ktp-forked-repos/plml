% typecode - Return basic typing information about a value
%
% typecode :: A -> natural ~'number of elements', bool ~'is numeric', bool ~'is char'.
function [n,isnum,isch]=typecodes(X)

n=numel(X);
isnum=isnumeric(X);
isch=ischar(X);
%tp=class(X);
