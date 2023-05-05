	-- 1	[2]	LOADK    	0 -1	; 1234
	-- 2	[3]	LOADK    	1 -2	; 5678
	-- 3	[4]	JMP      	-3	; to 1
	-- 4	[4]	RETURN   	0 1

-- this is semantically equivalent to a while-true-loop, however it generates
-- different line mappings for the final jump, so it will be detected as a
-- repeat-loop if line info is used
repeat
	local a = 1234;
	local b = 5678;
until false;

-- the following generates identical code to above
while true do
	local a = 1234;
	local b = 5678;
end
