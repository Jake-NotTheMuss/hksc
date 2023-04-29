-- test an assigned expression list

local a = 12;
local b = 34;

a = 1, 2;

a = b, 1;

a = b, b;

a = 1, 2, 3;

a = a, b;
