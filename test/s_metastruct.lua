-- test compiler struct slot lookups

hstructure M5
	__index : M5;
	c : number;
end

hstructure M4
	meta : M5;
	__index : M4;
	b : number;
	c : number;
end

hstructure M3
	meta : M4;
	__index : M3;
	b : number;
	c : number;
end

hstructure M2
	meta : M3;
	__index : M2;
	b : number;
	c : number;
end

hstructure M1
	meta : M2;
	__index : M1;
	__newindex : M1;
	b : number;
	c : number;
	s : string;
	o : boolean;
end

hstructure S
	meta : M1;
	b : number;
	c : number;
	s : number;
	t : table;
	o : object;
end

-- the local variable needs to be typed to emit struct codes
local s:S = hmake S {};

-- this should emit GETSLOTMT with a tag-chain of 4 (M1->M2->M3->M4)
local b = s.b;
-- accessing c has a tag-chain > 4 which will cause the compiler to use
-- GETTABLE
local c = s.c;

-- SETSLOT
s.t = t;

-- SETTABLE - slot-types do not match between structs S and M1
s.s = s1;

-- SETSLOTMT, TBOOLEAN
s.o = o;
