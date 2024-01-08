-- comparison order sometimes matters in LT/LE

local a;

-- these cases have to do with the order of generation vs the order of
-- referencing of constants, in the first case, the order is the same for
-- generation and referencing
-- in the second case, the constants are referenced in a different order than
-- they are generated, causing different bytecode to be generated

-- must be GE to ensure constants are generated in order
if a + 1 >= 0 then return; end

-- must be LE to ensure constants are generated in order but referenced out of
-- order
if 2 <= a + 3 then return; end


if a > 4 then return; end

if 5 < a then return; end


if b + 1 > 0 then
	return;
end
