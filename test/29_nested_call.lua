local function f(a, b)
	local c = a * b;
	local function f1()
		local d = a + b + c;
		print(tostring(d));
		return d;
	end
	return f1
end

f(1, 2)();
f(f(1, 2)(), f(3, 4)());
f(f(f(1, 2)(), f(3, 4)())(), f(f(5, 6)(), f(7, 8)())());

local function g(a, b)
	local c = a / b;
	local function g1(a1, b1)
		local d = (a + b + c) / (a1 * b1);
		local function g2()
			local e = a * b * c * d;
			print(tostring(e));
			return e;
		end
		return g2;
	end
	return g1;
end

g(1, 2)(3, 4)();
g(g(1, 2)(3, 4)(), g(5, 6)(7, 8)())(g(9, 10)(11, 12)(), g(13, 14)(15, 16)());
