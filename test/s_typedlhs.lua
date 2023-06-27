local a:userdata, b:luserdata, c:ui64;
local a:boolean, b:string, c:luserdata = true, "", 0x1234hi;

local a:cfunction, b:boolean, c:thread = f();

local a:number, b:string, c:table;

a, b, c = 1, f();

a, b, c, d, e = f, f();

hstructure T
	t : table;
	n : number;
	o : object;
end

local t:T = hmake T{};

t.t, b, t.n, t.o = f();

t.t, a, g = nil;

a, b, c = 1, "", {}, 2, 3;
a, b, c = 1, "", g, 2, 3;

a, b = t.n, t.o;
a, b = t.n, t.o, t.t;

local i:ifunction = nil
