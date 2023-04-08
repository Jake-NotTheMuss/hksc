-- detected as WHILE (true) -> WHILE (true) -> IF (a)
while true do
	while a do
		local b = 12;
	end
end

-- This is what the decompiler sees - it is semantically equivalent and
-- generates the same code as above

--[[
while true do
	while true do
		if a then
			local b = 12;
		end
	end
end
]]

-- Because both while-loops have the same startpc, the decompiler sees the
-- exit-jump from the condition (a) as a jump from inside a while-loop back
-- to the start of the same while-loop, which indicates an optimized exit
-- from a tail-if-statement condition

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- detected as WHILE (true) -> WHILE (true) -> IF (a) -> IF (c)
while true do
	while a do
		local b = 12;
		if c then
			return a;
		end
	end
end

-- This is what the decompiler sees - it is semantically equivalent and
-- generates the same code

--[[
while true do
	while true do
		if a then
			local b = 12;
			if c then
				return a;
			end
		end
	end
end
]]


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- detected as WHILE (true) -> WHILE (true) -> IF (a) - IF (false)
while true do
	while a do
		local b = 12;
		repeat
			do break; end
			local c = 45;
		until true;
	end
end

-- This is what the decompiler sees - it is semantically equivalent and
-- generates the same code

--[[
while true do
	while true do
		if a then
			local b = 12;
		end
		if false then
			local c = 45;
		end
	end
end
]]

-- The 'break' looks like an exit out of an if-statement body, and the
-- decompiler detects an if-else inside a while-true-loop inside 2 while-loops
