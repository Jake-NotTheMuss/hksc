-- should be recursive
local function f(n)
	if n == 0 then
		return 1
	else
		return n + f(n-1);
	end
end

-- should not be recursive
local g = function(n)
	if n == 0 then
		return 1
	else
		return n + g(n-1);
	end
end
