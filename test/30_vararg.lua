local a = ...;
local b = 12;
local c = {...};
local d = 34;
local e = {..., 1};
local f = 56;

local function g(...)
	local first = ...;
	local all = {...};
	all.n = select("#", ...);
	print(all.n .. " arguments provided");
	print("argument 1 is '" .. tostring(first) .. "'");
	for i, val in ipairs(all) do
		if i > 1 then
			print("argument " .. i .. " is '" .. tostring(val) .. "'");
		else
			assert(val == first);
		end
	end
end

g();
g(1, 2, 3);
g(...);
