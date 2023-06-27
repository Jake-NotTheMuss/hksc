hstructure S
	n : number;
end

local a = "hello";
local b:string = "hello";

local s:S = hmake S{};

s.n = a; -- CHECKTYPE

s.n = b; -- error, type mismatch
