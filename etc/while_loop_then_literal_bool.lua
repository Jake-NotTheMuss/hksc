--[[
while_loop_then_literal_bool.lua

This file contains an example of a bug in Lua 5.1 and describes it.
The bug has to do with Lua's optimization of literal boolean values used with
operators 'and'/'or' after a while-loop.
]]

while a do
	local a = 1 -- some code to separate the jump instructions for clarity
	do break end
	a = 4
end

local a = false and 4;

--[[
Template code for this bug:
COND = any expression except 'true'
EXP = any expression
NAME = any valid variable name
===
while COND do
end
local NAME = false and EXP [OR] local NAME = true or EXP
===
]]

--[[
CODE LISTING
=============================================================================
        1       [9]     GETGLOBAL                       0 -1    ; a
        2       [9]     TEST_R1                         0 0
        3       [9]     JMP                             9       ; to 13
        4       [10]    LOADK                           0 -2    ; 1
        5       [11]    JMP                             5       ; to 11
        6       [12]    LOADK                           0 -3    ; 4
        7       [9]     JMP                             -7      ; to 1
        8       [15]    JMP                             2       ; to 11
        9       [15]    LOADK                           0 -3    ; 4
        10      [15]    JMP                             2       ; to 13
        11      [15]    LOADBOOL                        0 0 1
        12      [15]    LOADBOOL                        0 1 0
        13      [15]    RETURN                          0 1

]]

--[[
EXPLANATION
=============================================================================
The explanation will contain pieces of the code followed by comments that
describe what the compiler is doing as a result
The format of these comments is outlined as such:
  - '==>' is to be read as 'as a result' or 'in other words'
  - ':' separates actions taken by the compiler
  - Jump lists are lists of jump instructions that have not been patched
    yet, i.e. their destination is not yet known; in Lua, they are encoded
    by an integer respresenting the PC of the first jump instruction in the
    list; the destination of each jump in the list gives the next jump; the
    last jump in the list will have its own PC as the destination, i.e. the
    jump offset will be -1 (the macro NO_JUMP). Jump lists in comments are'
    space-separated identifiers or numbers, such as 'pc breaklist', which
    desribes a jump list where 'pc' is the first jump, and 'breaklist' is the
    second and final jump
  - JPC is fs->jpc, which is a jump list saved to the current function
    state; various operations will access this jump list:
      * jump() [alias for luaK_jump]: moves JPC to a local variable, clears
        the global JPC, creates a new jump instruction, concatenates the
        local copy of JPC to the newly created jump, and returns that new list
      * dischargejpc(): patches all jumps in JPC to the current pc, and
        clears JPC to -1 (NO_JUMP), indicating an empty list (this is
        called every time a new instruction is coded in order to finalize any
        pending jump list not handled explicitly by earlier parser code)
      * patchtohere(LIST) [alias for luaK_patchtohere]: concatenates a list of
        jumps LIST to JPC, i.e., JPC = JPC LIST
   - concatenating NO_JUMP to a jump list has no effect, i.e.
     concat(JPC, NO_JUMP) ==> JPC = JPC NO_JUMP ==> JPC = JPC
   - expressions have 2 jump lists, a true-exit and false-exit; the
     true-exit is used for jumps that are taken when evaluation of the
     expression yields a true value, for example, the left operand to the 'or'
     operator, which creates a jump to a destination that skips the
     evaluation of the second operand; the false-exit is similarly used for
     jumps that are taken when evaluation of the expression yields a false
     value; access to these lists is indicated in the comments by [exp]->f'
     and [exp]->t, for the false-exit and true-exit lists, respectively
   - 'breaklist' is a per-block jump list that contains all of the break
     instructions encountered at that block's lexical level
]]

--[[
while a do -- condexit = 'a'->f ==> condexit = 3 (pc of first JMP)
  local a = 1;
  do break end -- concat(breaklist, jump()) ==> breaklist = 5 (pc of break JMP)
  a = 4
end -- patchtohere(breaklist) ==> JPC = breaklist
    -- patchtohere(condexit) ==> JPC = breaklist condexit

local a = false and -- RHS->f = jump() ==> RHS->f = 8 (pc of 'false' jump) JPC, and then JPC = NO_JUMP
                    -- ==> RHS->f = 8 breaklist condexit, and then JPC = NO_JUMP
4; -- fj = jump() ==> fj = 10 JPC ==> fj = 10: p_f = code_label(0): p_t = code_label(1)
-- patchtohere(fj) ==> JPC = JPC fj ==> JPC = fj ==> JPC = 10
-- patchlistaux(RHS->f, fs->pc, reg, p_f)
--  'patchlistaux' will traverse a jump list, and for each jump that is not
--  controlled by a tested value, it will patch that jump to a 'default'
--  target (argument 4) where a value will be loaded into REG, to ensure every
--  control path in the expression yields a value; for each jump that is
--  controlled by a tested value, it will patch that jump to after the end of
--  the expression (argument 2: fs->pc)
--  RHS->f = 8 breaklist condexit
--    (PC 8) has no tested value; it is an unconditional jump
--      Therefore, it will be patched to p_f
--    breaklist (PC 5) has no tested value; it is a n unconditional jump
--      Therefore, it will be patched to p_f
--    condexit (PC 3) has a tested value, register 0 (global 'a')
--      Therefore it will be patched to fs->pc (the current pc)
--      This results in a bug where 'local a' does not have the correct
--      value if the loop is broken by 'a' being false, since the jump will
--      go to PC 13, skipping any code that loads a value into 'local a'
]]
