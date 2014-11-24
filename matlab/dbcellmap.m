function Y=cellmap(fn,X)
% cellmap - Map a function over a cell array
%
% cellmap :: (A->B, {[Size]->A}) -> {[Size]->B}

% preallocate to fix size
Y=cell(size(X));
for i=1:numel(X)
	Y{i}=feval(fn,X{i});
end
