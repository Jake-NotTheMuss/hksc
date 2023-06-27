local function f(a, b:string)
	local c:string = a;
	c = b;

	local d:number = a;
	d = b; -- error, type mismatch
end
