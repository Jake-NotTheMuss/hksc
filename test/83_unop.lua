-- test unary operators

local a = -a;

local b = not b;

local c = #c;

local d = ##d;

local e = ###########e;

local f = not not not f;

-- the parens are important
local g = -(-g);

g = (-(-(-(-(-(-(g)))))));

-- test unary operators with binary operators

local h = -(a + b);

local i = (-i + j);

local j = -(j + (k - l));
