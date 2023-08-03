-- code by AlexanderMarinov
-- Compatible with Lua 5.1 (not 5.0).
require "lfs"

function dirtree(dir)
  assert(dir and dir ~= "", "directory parameter is missing or empty")
  if string.sub(dir, -1) == "/" then
    dir=string.sub(dir, 1, -2)
  end

  local diriters = {lfs.dir(dir)}
  local dirs = {dir}

  return function()
    repeat 
      local entry = diriters[#diriters]()
      if entry then 
        if entry ~= "." and entry ~= ".." then 
          local filename = table.concat(dirs, "/").."/"..entry
          local attr = lfs.attributes(filename)
          if attr.mode == "directory" then 
            table.insert(dirs, entry)
            table.insert(diriters, lfs.dir(filename))
          end
          return filename, attr
        end
      else
        table.remove(dirs)
        table.remove(diriters)
      end
    until #diriters==0
  end
end
