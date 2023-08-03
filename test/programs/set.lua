function Set(t)
  local s = {}
  for _,v in pairs(t) do s[v] = true end
  return s
end

function contains(t, e)
  return t[e]
end

t = Set{"abc", "def", "ghi"}

print (contains(t,"def"))  --> true
print (t["def"])           --> true (same as above)
print (contains(t,"ddd"))  --> nil
print (t["ddd"])           --> nil (same as above)

function OrderedSet(t)
  local s = {}
  for i,v in ipairs(t) do s[v] = i end -- key value is index
  return s
end

function contains(t,e) return t[e] end
function indexof(t,e) return t[e] end 

t = OrderedSet{"abc", "def", "ghi"}

if contains(t,"def") then
  print (indexof(t,"def"))
end
--> 2
