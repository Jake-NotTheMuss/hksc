-- test code generation for hmake expressions

--[[
Notes:
(*) Setting slots in constructors is always a primitive assignment, so
    SETSLOTMT should never be generated in them
(*) Hashtable space is only given for indices which will definitely not
    resolve to a slot at runtime. Any string index may resolve to a slot at
    runtime even if it doesn't resolve to one in the primitive lookup at
    compile-time, since it may be a slot name in a meta structure prototype.
    Indices of unknown type may be strings and therefore may also resolve to a
    slot at runtime.
]]

hstructure VecMeta
	-- pointers back to itself for metatable lookup
	__index : VecMeta;
	-- __newindex to make sure the compiler doesn't use it in constructors
	__newindex : VecMeta;
	x : number;
	y : number;
	z : number;
	w : number;
end

-- untyped and typed local variables for testing stuff
local defaultx = 0;
local defaulty:number = 0;

Vector = hmake VecMeta {
	-- I have different kinds fo expressions to show how type checks are
	-- generated for them
	x = defaultx, -- SETSLOT
	y = defaulty, -- SETSLOTI (defaulty is typed)
	z = 0, -- SETSLOTI
	w = nil, -- SETSLOTN
};

hstructure Vec2
	meta : VecMeta;
	x : number;
	y : number;
	o : object; -- an untyped slot to test stuff with
end

do
	-- this is to show why typing variables is needed, because the compiler
	-- never infers the type of a variable, only the type of an expression
	local v_untyped = hmake Vec2 {x = 0, y = 0};
	-- should emit CHECKTYPE because the variable has unknown type, and
	-- therefore the VLOCAL expression does too, even though it obviously
	-- isn't valid bacause the variable is not a number
	local a:number = v_untyped;
end

local v2:Vec2 = hmake Vec2 {
	x = GetX(), -- SETSLOT
	y = GetY(), -- SETSLOT
};

hstructure Vec3
	proxytable; -- enable this to test with the next constructor
	meta : VecMeta;
	x : number;
	y : number;
	z : number;
end

local v3:Vec3 = hmake Vec3 {
	x = v2.x, -- SETSLOTI -> GETSLOTMT
	y = v2.o, -- SETSLOT -> GETSLOT
	z = 0, -- SETSLOTI
	-- 'abc' will not be given space in the hash part, because it may resolve
	-- to a slot in a meta structure at runtime
	abc = 12, -- SETTABLE (Vec3 proxytable)
	1, 2, 3, -- some array indices for the proxytable
	-- '4' will be given space in the hash part because it is a non-string
	-- index, so it cannot resolve to a slot
	[4] = "",
	-- defaulty is a typed variable, and because it is not a string, it will
	-- be given space in the hashpart
	[defaulty] = F(),
	-- defaultx is not typed, therefore it will not be given space in the hash
	-- part because it may resolve to a slot at runtime
	[defaultx] = F()
};
