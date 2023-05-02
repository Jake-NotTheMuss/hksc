-- test the decompiler's ability to handle an OP_LOADNIL that is shared
-- between multiple statements

local y = nil;
y = nil .. nil;
local a, c, b;

local a = nil;
a = (nil)();

a = nil;

a = (nil)(nil, nil, nil);

a = nil;

return nil, nil;
