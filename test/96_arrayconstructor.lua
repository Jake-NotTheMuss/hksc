-- test second pass handling of array table constructors

local a = {1, 2, 3};

local b = {
	1,
	2,
	3
};

local a = {...};

local a = {f()};

local a = {1, 2, 3, ...};

local a = {a};

local c = {c};

local a = {{}};

a = {{}};

a = {a, {b, {c}}, d};

a = {
	a,
	{
		b, {
			c,
			{d}
		}
	}
};


({
	1,
	2,
	...,
	3
})(a, {...}, nil, {nil,

nil, nil
});

g[{}] = {{{{{{{{{{{{}}}}}}}}}}}};
