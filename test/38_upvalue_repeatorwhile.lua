-- test the following code pattern, and the decompiler's ability to
-- differentiate between different contexts (i.e. while-loop/repeat-loop)
-- OP_JMP   ; break OR jump-to-continue
-- OP_CLOSE
-- OP_JMP   ; exit (break OR fail-jump)
-- OP_CLOSE ;
-- OP_JMP   ; continue loop

while a do
	local b;
	local function c()
		b = b + 1
	end
	do break; end -- emit CLOSE-JMP
	do break; end -- emit CLOSE-JMP
end -- CLOSE-JMP

repeat
	local b;
	local function c()
		b = b + 1;
	end
	-- test "a" - JMP (2 instructions forward)
	-- CLOSE-JMP (jump out of loop)
	-- CLOSE-JMP (continue loop)
until a;
