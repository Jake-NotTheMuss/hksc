-- test constructors with array and hash items

local a = { 1, a = 2, 3, b = 4 };

local b = {
	nil,
	[nil] = nil,
	[a[1]] = b
};

local c = {a = nil, nil, nil, b = 12, ...};

c.a = {
	{a * b + c * d, a = b, c},
	a = ...,
	[c] = f
};

f({[1] = 2}, 3, {a = 1, ...});
