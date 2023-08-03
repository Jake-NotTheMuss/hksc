intervals={
  {"seconds",1}, --the "1" should never really get used but
  {"minutes",60},
  {"hours",60},
  {"days",24},
}

positions={}
for i=1,4 do
  positions[intervals[i][1]]=i
end

function convert_to(value, sourceunits, targetunits)

  local sourcei, targeti = positions[sourceunits], positions[targetunits]
  assert(sourcei and targeti)

  if sourcei<targeti then

    local base=1
    for i=sourcei+1,targeti do
      base=base*intervals[i][2]
    end

    return value/base

  elseif sourcei>targeti then

    local base=1
    for i=targeti+1,sourcei do
      base=base*intervals[i][2]
    end

    return value*base

  else return value end
end

print(convert_to(86400,"seconds","days"))
print(convert_to(1,"days","seconds"))
