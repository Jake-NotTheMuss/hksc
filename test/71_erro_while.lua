while a do
	local a = 12;
	do
		local b = 34;
		local function c()
			b =b +1;
		end
	end
	if false then
	end
end

do return; end

------------------------------------------------------------------------------

-- PASSES
while a do
	local a = 12;
	do
		local b = 34;
		local function c()
			b =b +1;
		end
	end
	if a then
		return a;
	else
	end
end

do return; end

------------------------------------------------------------------------------

while a do
	local a = 12;
	if false then
		return a;
	end
	local b = 34;
	if a then
		return a;
	else
	end
end

do return; end


