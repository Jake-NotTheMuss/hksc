-- test multiple local variables declared at once
local a, b = 1, 2;

local c, d; -- OP_LOADNIL 2 3
