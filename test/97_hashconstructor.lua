local a = {a = 1};

local a = {
	a = 1,
	b = 2,
	c = 3
};

local a = {[b+c] = 1, [e+f] = 2};

local a = {
	a = c * d + e * f,
	b = {
		a = {a = nil},
		b = ...,
		c = nil,
		d = nil,
	}
};

a = {
	a = 1
};

T = {
	[g * h + i * j] = (g + h) * (i + j),

	[ ({[g] = ... .. g})(1, nil, nil, {
		[{}] = 12,
	}) ] = nil
};

T[1] = {
	[nil] = nil,
	[(nil)()] = 3
};


T[1][{}].f = {
	[...] = ...
}
