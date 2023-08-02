-- conditional expressions involving comparisons and open expressions

local a = a == b and 1;

a = a + b and c / d or e % f;

local a = {a and b};

local z = z or {z or b};

a = (a == b and b or c) + 1, a and b;

a = a +1 or 5;

local a = a and {a and b, c};

local a = (x ~= y and z) + 1, a and b;

local a = a and {a and b, c};
