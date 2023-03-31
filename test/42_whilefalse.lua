	-- 1	[11]	JMP      	3	; to 5
	-- 2	[12]	LOADK    	0 -1	; 1234
	-- 3	[13]	LOADK    	1 -2	; 5678
	-- 4	[11]	JMP      	-4	; to 1
	-- 5	[14]	RETURN   	0 1

-- this emits jumps and should be detected as a while-loop
while false do
	local a = 1234;
	local b = 5678;
end
