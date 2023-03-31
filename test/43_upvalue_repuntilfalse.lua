	-- 1	[13]	LOADNIL  	0 0
	-- 2	[16]	CLOSURE  	1 0	; 0x6000029cc0a8
	-- 3	[16]	DATA     	1 0
	-- 4	[16]	CLOSE    	0
	-- 5	[12]	JMP      	-5	; to 1
	-- 6	[17]	RETURN   	0 1

while true do
	local b;
	local function c()
		b = b + 1;
	end
end

	-- 1	[36]	LOADNIL  	0 0
	-- 2	[39]	CLOSURE  	1 0	; 0x6000039040a8
	-- 3	[39]	DATA     	1 0
	-- 4	[42]	JMP      	2	; to 7
	-- 5	[42]	CLOSE    	0
	-- 6	[42]	JMP      	2	; to 9
	-- 7	[42]	CLOSE    	0
	-- 8	[42]	JMP      	-8	; to 1
	-- 9	[42]	RETURN   	0 1

repeat
	local b;
	local function c()
		b = b + 1;
	end
until false;
