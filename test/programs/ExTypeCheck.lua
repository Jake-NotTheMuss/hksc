-- ExTypeCheck.lua ("ExTypeCheck")
-- Type checking for Lua.
--
-- In this type model, types are associated with values at run-time.
-- A type consists of the set of values the value could have
-- at run-time.  This set can be large or infinite, so we
-- store only a small representative subset of those values.
-- Typically one would want to include at least the boundary
-- values (e.g. max and min) in this set.
-- Type checking is performed by verifying that all values
-- in that set are accepted by a predicate function for that type.
-- This predicate function takes a values and returns true or false
-- whether the value is a member of that type.
--
-- As an alternative to representing types as a set of representative
-- values, we could represent types more formally, such as with
-- first-order logic, but then we get into theorem proving,
-- which is more involved.
--
-- DavidManura, 2007, licensed under the same terms as Lua itself.

local M = {}

-- Stored Expression design pattern
-- ( http://lua-users.org/wiki/StatementsInExpressions )
local StoredExpression
do
  local function call(self, ...)
    self.__index = {n = select('#', ...), ...}
    return ...
  end
  function StoredExpression()
    local self = {__call = call}
    return setmetatable(self, self)
  end
end
 
-- Whether to enable type checking (true/false).  Default true.
local is_typecheck = true

-- TypeValue is an abstract type for values that are typed
-- This holds the both the actual value and a subset of possible
-- values the value could assume at runtime.  That set should at least
-- include the min and max values (for bounds checking).
local TypedValue = {}

-- Check that value x satisfies type predicate function f.
function M.check_type(x, f)
  for _,v in ipairs(x) do
    assert(f(v))
  end
  return x.v
end


-- Type check function that decorates functions.
-- Example:
--   abs = typecheck(ranged_real'(-inf,inf)', '->', ranged_real'[0,inf)')(
--     function(x) return x >= 0 and x or -x end
--   )
function M.typecheck(...)
  local types = {...}
  return function(f)
    local function check(i, ...)
      -- Check types of return values.
      if types[i] == "->" then i = i + 1 end
      local j = i
      while types[i] ~= nil do
        M.check_type(select(i - j + 1, ...), types[i])
        i = i + 1
      end
      return ...
    end
    return function(...)
      -- Check types of input parameters.
      local i = 1
      while types[i] ~= nil and types[i] ~= "->" do
        M.check_type(select(i, ...), types[i])
        i = i + 1
      end
      return check(i, f(...))  -- call function
    end
  end
end


function M.notypecheck() return function(f) return f end end
function M.nounbox(x) return x end

M.typecheck = is_typecheck and M.typecheck or M.notypecheck
M.unbox = is_typecheck and M.unbox or M.nounbox

-- Return a boxed version of a binary operation function.
-- For the returned function,
--   Zero, one, or two of the arguments may be boxed.
--   The result value is boxed.
-- Example:
--   __add = boxed_op(function(a,b) return a+b end)
function M.boxed_op(op)
  return function(a, b)
    if getmetatable(a) ~= TypedValue then a = M.box(a) end
    if getmetatable(b) ~= TypedValue then b = M.box(b) end
    local t = M.box(op(M.unbox(a), M.unbox(b)))
    local seen = {[t[1]] = true}
    for _,a2 in ipairs(a) do
      for _,b2 in ipairs(b) do
        local c2 = op(a2, b2)
        if not seen[c2] then
          t[#t + 1] = op(a2, b2)
          seen[c2] = true
        end
      end
    end
    return t
  end
end

-- Return a boxed version of a unary operation function.
-- For the returned function,
--   The argument may optionally be boxed.
--   The result value is boxed.
-- Example:
--   __unm = boxed_uop(function(a) return -a end)
function M.boxed_uop(op)
  return function(a)
    if getmetatable(a) ~= TypedValue then a = M.box(a) end
    local t = M.box(op(M.unbox(a)))
    local seen = {[t[1]] = true}
    for _,a2 in ipairs(a) do
      local c2 = op(a2)
      if not seen[c2] then
        t[#t + 1] = op(a2)
        seen[c2] = true
      end
    end
    return t
  end
end

TypedValue.__index = TypedValue
TypedValue.__add = M.boxed_op(function(a, b) return a + b end)
TypedValue.__sub = M.boxed_op(function(a, b) return a - b end)
TypedValue.__mul = M.boxed_op(function(a, b) return a * b end)
TypedValue.__div = M.boxed_op(function(a, b) return a / b end)
TypedValue.__pow = M.boxed_op(function(a, b) return a ^ b end)
TypedValue.__mod = M.boxed_op(function(a, b) return a % b end)
TypedValue.__concat = M.boxed_op(function(a, b) return a .. b end)
-- TypedValue.__le -- not going to work? (metafunction returns Boolean)
-- TypedValue.__lt -- not going to work? (metafunction returns Boolean)
-- TypedValue.__eq -- not going to work? (metafunction returns Boolean)
TypedValue.__tostring = function(self)
  local str = "[" .. tostring(self.v) .. " in "
  for i,v in ipairs(self) do
    if i ~= 1 then str = str .. ", " end
    str = str .. v
  end
  str = str .. "]"
  return str 
end
-- Convert a regular value into a TypedValue.  We call this "boxing".
function M.box(v, ...)
  local t = setmetatable({v = v, ...}, TypedValue)
  if #t == 0 then t[1] = v end
  return t
end
-- Convert a TypedValue into a regular value.  We call this "unboxing".
function M.unbox(x)
  assert(getmetatable(x) == TypedValue)
  return x.v
end


-- Returns a type predicate function for a given interval over the reals.
-- Example: ranged_real'[0,inf)'
-- Note: this function could be memoized.
function M.ranged_real(name, a, b)
  local ex = StoredExpression()

  if name == "(a,b)" then
    return function(x) return type(x) == "number" and x > a and x < b end
  elseif name == "(a,b]" then
    return function(x) return type(x) == "number" and x > a and x <= b end
  elseif name == "[a,b)" then
    return function(x) return type(x) == "number" and x >= a and x < b end
  elseif name == "[a,b]" then
    return function(x) return type(x) == "number" and x >= a and x <= b end
  elseif name == "(inf,inf)" then
    return function(x) return type(x) == "number" end
  elseif name == "[a,inf)" then
    return function(x) return type(x) == "number" and x >= a end
  elseif name == "(a,inf)" then
    return function(x) return type(x) == "number" and x > a end
  elseif name == "(-inf,a]" then
    return function(x) return type(x) == "number" and x <= a end
  elseif name == "(-inf,a)" then
    return function(x) return type(x) == "number" and x < a end
  elseif name == "[0,inf)" then
    return function(x) return type(x) == "number" and x >= 0 end
  elseif name == "(0,inf)" then
    return function(x) return type(x) == "number" and x > 0 end
  elseif name == "(-inf,0]" then
    return function(x) return type(x) == "number" and x <= 0 end
  elseif name == "(-inf,0)" then
    return function(x) return type(x) == "number" and x < 0 end
  elseif ex(name:match("^([%[%(])(%d+%.?%d*),(%d+%.?%d*)([%]%)])$")) then
    local left, a, b, right = ex[1], tonumber(ex[2]), tonumber(ex[3]), ex[4]
    if left == "(" and right == ")" then
      return function(x) return type(x) == "number" and x > a and x < b end
    elseif left == "(" and right == "]" then
      return function(x) return type(x) == "number" and x > a and x <= b end
    elseif left == "[" and right == ")" then
      return function(x) return type(x) == "number" and x >= a and x < b end
    elseif left == "[" and right == "]" then
      return function(x) return type(x) == "number" and x >= a and x <= b end
    else assert(false)
    end
  else
    error("invalid arg " .. name, 2)
  end
end


return M
