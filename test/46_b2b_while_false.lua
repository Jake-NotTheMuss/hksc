	-- 1	[2]	JMP      	7	; to 9
	-- 2	[3]	LOADK    	0 -1	; 1234
	-- 3	[4]	LOADK    	1 -2	; 5678
	-- 4	[2]	JMP      	-4	; to 1
	-- 5	[6]	JMP      	3	; to 9
	-- 6	[7]	LOADK    	0 -1	; 1234
	-- 7	[8]	LOADK    	1 -2	; 5678
	-- 8	[6]	JMP      	-4	; to 5
	-- 9	[9]	RETURN   	0 1


-- normally, this will emit a jump past the end of the loop, but with multiple
-- while-false-loops back-to-back, the jump will be optimized to skip all
-- loops
while false do
	local a = 1234;
	local b = 5678;
	break;
end
while false do
	local a = 1234;
	local b = 5678;
end
while false do
	local a = 1234;
	local b = 5678;
end
