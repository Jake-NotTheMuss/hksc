-- test typed parameters

local function f(x:string, y:number)
end

hstructure S
	o : object;
end

local function g(x:userdata, y, z:ui64, s:S)
end
