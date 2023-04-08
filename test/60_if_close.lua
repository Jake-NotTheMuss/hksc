-- todo: need to handle this case, as the decompiler still creates a block
-- around the if-block, even though the exit-jump jumps past the OP_CLOSE
-- which tells you that the if-block closes the variable
if a then
	local a = 12;
	local function b()
		a = a + 1;
	end
	return a;
end
