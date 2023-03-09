-- this checks how dead code that contains closures is handled
-- Note: this test is interesting for the decompiler as well
do
	local a = 1
	if false then
		local function b()
			a = a + 1
		end
	end
end
