--**** should be compatible with 5.xx
function intlimit()
  local floor = math.floor

  -- get highest power of 2 which Lua can still handle as integer
  local step = 2
  while true do
    local nextstep = step*2
    if nextstep-(nextstep-1) == 1 and nextstep > 0 then
      step = nextstep
    else
      break
    end
  end

  -- now get the highest number which Lua can still handle as integer
  local limit,step = step,floor(step/2)
  while step > 0 do
    local nextlimit = limit+step
    if nextlimit-(nextlimit-1) == 1 and nextlimit > 0 then
      limit = nextlimit
    end
    step = floor(step/2)
  end
  return limit
end
