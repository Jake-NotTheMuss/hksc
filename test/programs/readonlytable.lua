function readonlytable(table)
   return setmetatable({}, {
     __index = table,
     __newindex = function(table, key, value)
                    error("Attempt to modify read-only table")
                  end,
     __metatable = false
   });
end

Directions = readonlytable {
  LEFT   = 1,
  RIGHT  = 2,
  UP     = 3,
  DOWN   = 4,
  otherstuff = {}
}
