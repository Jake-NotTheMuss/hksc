-- test the decompiler's ability to match line mappings with unneeded parens
-- causing opcodes to map to different lines than the operands

local a = (-a + 12
           )
*

(

b + 12
 )

local e;
e = 1, (a * 3 / (b + 4)
        );

-- test it again but at the very end of the program
local c = (-c + 12
           )
*

(

b + 12
 )
