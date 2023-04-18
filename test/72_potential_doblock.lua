do
	local a = 12;
	while a do
		return a;
	end
	local function b()
		a = a + 1;
	end
end 			-- OP_CLOSE
if false then	-- OP_JMP
	return b;
end
