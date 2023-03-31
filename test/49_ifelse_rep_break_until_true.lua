-- test the decompiler's handling of repeat-break-until-true inside an
-- if-else block

if a then
	return a;
else
	local b = 12;
	repeat
		do break; end
		local c, d, e = 12, 34, 56;
	until true;
end
