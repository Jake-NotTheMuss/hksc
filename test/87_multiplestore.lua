a, b, c = 1, 2, 3;

local a = 12;
local b = 34;
local c = 56;

a, b, c = 1, 2, 3;

a, b, c = nil;

a, b, c = ...;

a, b, c = nil, nil, nil, nil;

a, b, c = 1, 2, 3, 4, 5, 6, 7;

c, b, a = a, b, c;

b, c, a = d(a, b, c), d(a, b, c);

a, g, f[1] = 5, nil, true;

a, f.f, g[a] = f, g, 3, nil;

a = 12;
b = 34;
c = 56;
