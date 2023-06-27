-- the compiler should not try to resolve slot names using meta prototypes
-- when setting a constructor field; test that, even when a slot is defined in
-- the meta structure, it cannot be resolved unless it is in the primary
-- structure

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
	o = 1 -- error, slot is invalid because meta prototypes are not referenced
};
