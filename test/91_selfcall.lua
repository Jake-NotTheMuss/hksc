-- test OP_SELF handler

local a = a:b();

local b = b:c(12);

local c = c:d(d, e, f);

local d = c:a(nil, nil, 34);

local a = a:getParent():b();

local b = b:getParent(a:getParent(nil), (5 + g[1]) * (g:a(1, nil) - nil)):getParent(a, b:c("")):getParent(b:a(a:b()));
