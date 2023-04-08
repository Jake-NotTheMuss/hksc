while a do
	if false then
		local a  =12;
	end
end

-- optimized jump past the end of the "2" outer loops
while a do
	if false then
		local a = 12;
	end
end
while false do
	local b = 12;
	print(b);
end
local c = 56;

-- multiple breaks in a repeat-until-true
while a do
	local a1 = 12;
	repeat
		do break; end
		local d = 1313;
		do break; end
		local e = 4949;
		do break; end
		local a = 12;
		local b = 34;
	until true
end
