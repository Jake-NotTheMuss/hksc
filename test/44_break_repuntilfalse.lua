	-- 1	[2]	JMP      	3	; to 5
	-- 2	[3]	LOADK    	0 -1	; 1234
	-- 3	[4]	LOADK    	1 -2	; 5678
	-- 4	[5]	JMP      	-4	; to 1
	-- 5	[5]	RETURN   	0 1

-- if line info is used, this will be detected as a repeat-loop
repeat
	do break; end
	local a = 1234;
	local b = 5678;
until false;

	-- 1	[18]	JMP      	3	; to 5
	-- 2	[19]	LOADK    	0 -1	; 1234
	-- 3	[20]	LOADK    	1 -2	; 5678
	-- 4	[18]	JMP      	-4	; to 1
	-- 5	[21]	RETURN   	0 1

-- semantically equivalent to the repeat-loop above, although this is the
-- canonical representation in the decompiler
-- while false do
-- 	local a = 1234;
-- 	local b = 5678;
-- end
