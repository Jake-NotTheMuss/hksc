-- test the decompiler's ability to detect sibling relationships to groups of
-- nested if-false blocks

while a do
	return a;
end -- optimized jump!
if false then
	local a = 12;
end
if false then
	local b = 34;
end
if false then
	local c= 56;
end
local d = 78;
if false then
	local a = 12;
end
if false then
	local b = 34;
end
if false then
	local c= 56;
end

while a do
	return a;
end

do return; end
------------------------------------------------------------------------------

while a do
	return a;
end -- not optimized
local a11 = 12; -- separator to avoid jump optimization
if false then
	local a = 12;
end
if false then
	local b = 34;
end
if false then
	local c= 56;
end
local d = 78;
if false then
	local a = 12;
end
if false then
	local b = 34;
end
if false then
	local c= 56;
end

while a do
	return a;
end
