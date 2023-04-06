-- check the compiler's ability to build function names correctly

local function f()
end

function g()
end

local t = {};

function t.x()
end

function t:y()
end

t.t1 = {};

function t.t1.x1()
end

function t.t1:y1()
end
