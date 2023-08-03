--[[
> fsum = require 'fsum'
> = fsum(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1) - 1
0
> fadd, ftotal = fsum()
> for i = 1, 10 do fadd(0.1) end
> = ftotal() - 1
0
> ftotal(0)    -- clear calculator
> for i = 1, 10 do fadd(0.1) end
> = ftotal() - 1
0
--]]

local function fsum(...)
    local p, abs = {1}, math.abs        -- p[1] == #p
    local function fadd(x)
        local p, i = p, 2
        for j = 2, p[1] do
            local y = p[j]
            if abs(x) < abs(y) then x, y = y, x end
            local hi = x + y
            local lo = y - (hi - x)
            x = hi
            if lo ~= 0 then p[i] = lo; i = i + 1 end
        end
        if x - x ~= 0 then i = 2 end    -- Inf or NaN
        p[1] = i
        p[i] = x
    end
    local function ftotal(clear)
        local p, x = p, 0
        if clear then p[1] = 1 end      -- ftotal(0) "clear" calculator
        for i = p[1], 2, -1 do          -- sum in reverse
            local y = p[i]
            local hi = x + y
            local lo = y - (hi - x)
            x = hi
            if lo ~= 0 and i ~= 2 then  -- check half way case
                if (lo < 0) == (p[i-1] < 0) then
                    lo = lo * 2         -- |lo| = 1/2 ULP
                    hi = x + lo         -- -> x off 1 ULP
                    if lo == hi - x then x = hi end
                end
                return x
            end
        end
        return x
    end

    if select('#', ...) == 0 then return fadd, ftotal end
    for i = 1, select('#', ...) do fadd(select(i, ...)) end
    return ftotal()
end

if select(1, ...) ~= 'fsum' then            -- test code
    local read, fadd, ftotal = io.read, fsum()
    io.input(select(1, ...))                -- read from file
    pcall(function() while true do fadd(read('*n')) end end)
    print(ftotal())
end
return fsum
