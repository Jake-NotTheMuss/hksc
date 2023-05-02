-- test multiple adjacent nil expressions in an expression list followed by a
-- non-nil expression

a = nil;


-- function calls
a(nil, nil, 1);

(nil)(nil, nil, nil, (b - a) / (1 - b));

-- concatenations
a = nil .. nil .. a .. b;

local b = nil;

a = nil .. nil .. (a + b) * (a + c);

-- return statements

do return nil, nil, 1; end

b = nil;

do return nil, nil; end

b = nil;

local c = nil;

do return nil, nil, 5 * a; end

local d = nil;

return nil;
