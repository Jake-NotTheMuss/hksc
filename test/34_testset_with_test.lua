-- test the first pass analyzer's ability to correct an erroneous branch
-- detection due to a regular test and jump at the end of a testset expression
local a;
a = a .. b or a or b;
