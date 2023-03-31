-- trick the decompiler into detecting an if-block that isn't there, and test
-- its ability to recover
local a,b;
a = a() or b or a or d

if a then
	return a;
end
