while a do
	local a = 12;
	while b do
		local b = 34;
		do break; end -- if optimized, will jump to start of outer loop
	end
end

while true do
	local a = 12;
	for i=0,5 do
		local b = 34;
		do break; end -- if optimized, will jump to start of outer loop
	end
end
