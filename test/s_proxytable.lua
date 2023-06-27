-- test compiler struct slot lookups with proxytables

hstructure S
	proxytable;
	a : object;
end

hstructure SMeta
	__index : S;
	__newindex : S;
end

local s:S = hmake S{};

do
	-- GETSLOT
	local a:object = s.a;
	-- GETTABLE, CHECKTYPE
	local b:number = s.b;
end

-- SETSLOTI
s.a = 12;

-- SETTABLE
s.b = 34;

hstructure S1
	proxytable;
	meta : SMeta;
	a : object;
	b : number;
end

local s:S1 = hmake S1{};

do
	-- GETSLOTMT
	local a:object = s.a;
	-- GETTABLE (because struct S proxytable may be indexed), CHECKTYPE
	local b:number = s.b;
	-- GETTABLE, CHECKTYPE
	local c:string = s.c;
end

-- SETSLOTMT
s.a = 12;

-- SETTABLE
s.b = 34;

-- SETTABLE
s.c = 12;
