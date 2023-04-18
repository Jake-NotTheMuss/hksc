while a do
	return a;
end

do
	local a = 12;
	while a do
		return a;
	end
	local function b()
		a = a + 1;
	end
end

if false then
	local a = 12;
	local function b()
		a = a + 1;
	end
	return b;
end
if false then
	local a = 12;
	local function b()
		a = a + 1;
	end
	return a;
end

local a = 12;

do
	local b = 34;
	local function c()
		b = b + 1;
	end
end
