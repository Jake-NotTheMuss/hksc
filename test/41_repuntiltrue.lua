	-- 1	[2]	LOADK    	0 -1	; 1234
	-- 2	[3]	LOADK    	1 -2	; 5678
	-- 3	[4]	RETURN   	0 1

-- this is not detected as a basic block because it emits no jumps
repeat
	local a = 1234;
	local b = 5678;
until true;
