local a = "hello";
local b:string = "hello";

local c:string = a;
c = b;

local d:number = a;
d = b; -- error, type mismatch
