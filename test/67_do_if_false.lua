local function func()
	local a;
	-- try to break the decompiler's ability to represent the sibling
	-- relationships correctly
	do
		if false then
			a = 0;
			return a;
		end
		local b = 12;
		local function c()
			b = b + 1;
		end
		local d = 34;
		local e = 56;
	end

	do return; end
end
------------------------------------------------------------------------------

local function g()
-- while not at all practical, this is a tricky one for the decompiler
-- the first 3 if-blocks should be seen as preceding siblings to the do-block
-- whereas the second 3 if-blocks are children of the do-block
do
	if false then
		local a = 12;
	end
	if false then
		local b = 34;
	end
	if false then
		local c= 56;
	end
	local d = 78;
	if false then
		local a = 12;
	end
	if false then
		local b = 34;
	end
	if false then
		local c= 56;
	end
	local a = 12;
	local function b()
		a = a + 1;
	end
	local c = 34;
	local d = 56;
end
end
