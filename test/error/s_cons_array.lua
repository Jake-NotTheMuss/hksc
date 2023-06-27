-- when constructing a structure, if the prototype does not enable a backing
-- table, the compiler should not accept record fields that do not resolve to
-- slot names at compile-time

hstructure SMeta
	__index : SMeta;
	__newindex : SMeta;
	o : object;
end

hstructure S
	meta : SMeta;
	dummy : object;
end

local s = hmake S {
	dummy = 1, -- valid
	1 -- error, array index is invalid without a backing table enabled
};
