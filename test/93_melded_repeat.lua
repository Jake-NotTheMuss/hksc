-- test the first pass analyzer's ability to differentiate repeat loop
-- conditions and meld nested loops together when appropriate

local a = nil;

-- this is standard example of a repeat-loop that looks like 2 nested loops
-- the 'and' operator causes the jumps to target the start of the loop
-- this is identifiable as a single loop because there are no statements
-- between 'a' and 'b'
repeat
	local b = 12;
until a and b;

a = nil;

-- this looks exactly like the loop above, but generates different debug info
-- 'b' has a different lifespan and the decompiler needs to account for that
repeat
	repeat
		local b = 12;
	until a
until b; -- b is out of scope here

a = nil;

-- this must be identified as 2 separate loops because there is a statement
-- between the end of the inner loop and the end of the outer loop
repeat
	repeat
		a = 34;
	until a;
	local b = b;
until b;

a = nil;

-- 2 separate loops
repeat
	repeat
		a = 34;
	until a;
	a = 43; -- LOADK; a statement
until b;

a = nil;
local a1 = 5;
-- 2 separate loops
repeat
	repeat
		a = 34;
	until a;
	a = a1; -- MOVE; a statement
until b;

a = nil;

-- 2 separate loops
repeat
	repeat
		a = 56;
	until a;
	b = 7;  -- SETGLOBAL; obviously a statement
until b;

a = nil;

-- 2 separate loops
repeat
	repeat
		a = 78;
	until a;
	a[b] = c; -- OP_SETTABLE; obviously a statement
until b;
