local a = 0;
while a < 5 do
	print("a = " .. a);
	local b = 0;
	while b < 2 do
		print("b = " .. b);
		break;
	end
end
local c = 0;
while c < 5 do
	print("c = " .. c);
	do break; end
	local d = 0;
	while d < 2 do
		print("d = " .. d);
	end
end
