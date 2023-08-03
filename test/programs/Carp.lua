-- Carp.lua
-- This package is based on Perl Carp
-- (http://search.cpan.org/~nwclark/perl-5.8.8/lib/Carp.pm)
-- David Manura, 2006-09, 2007-07

local M = {}

function M.croak(message)
  local current_env = getfenv(2)
  local level = 2
  while true do
    local is_success, result = pcall(function()
      return getfenv(level + 2)
    end)
    if is_success then
      local env = result
      --print("DEBUG:level", level, env._NAME)
      if env ~= current_env then
        --print("DEBUG:found", level, env._NAME)
        error(message, level)
      end
    elseif string.find(result, "(invalid level)") then
      break
    end
    level = level + 1
  end
end

return M
