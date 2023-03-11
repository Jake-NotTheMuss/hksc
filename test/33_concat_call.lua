local function g(a, b, c)
	print(a .. b .. c);
	return a .. b .. c;
end
local function f()
	return g
end

local function printstr(s)
	print(tostring(s));
	return tostring(s);
end

local a = "hello";
local b = " ";
local c = "there";

print(f()(a, b, printstr(a .. b) .. printstr()) .. ".");

local d = printstr((function() return "yes"; end)() .. a) .. printstr("hello") 
.. printstr(printstr(a .. b) .. c) .. a .. printstr(a .. b) .. c;

local e = a .. (a .. printstr(a .. printstr(b .. c)) .. c) .. c;
print(d);

do return; end

-- nonsense code just for compiling/decompiling

(a .. b)(a, b);

d = (a .. (a .. v(a .. v)))(a .. b .. c) .. a .. b .. c;
