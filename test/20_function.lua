function f()
	return 1;
end

local function f1(x)
	return x * 2;
end

local f2 = function(x)
	return x * 2;
end

local function f3()
	local a = 1
	local function g()
		local b = 1;
		local function h()
			local c = 1
			return c
		end
		return b
	end
	return a
end
