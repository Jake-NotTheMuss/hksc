local a = true;
while a do
	local b = 1234;
	print(b);
	do break; end
	local c = 5678;
	print(c);
end
