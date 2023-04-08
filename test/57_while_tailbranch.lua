-- a tail-branch at the end of a while-loop

while a do
	local a = 12;
	if a then
		local b = 34;
		if b then
			a();
		end
	else
		local c = 56;
	end
end
