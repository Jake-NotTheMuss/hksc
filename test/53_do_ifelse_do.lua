-- a block inside an else-block inside a block
do
	local a = 12;
	local function b()
		a = a + 1;
	end
	if a then
		return a;
	else
		local c = 34;
		local function d()
			c = c + 1;
		end
		do
			local e = 56;
			local function f()
				e = e + 1;
			end
		end
	end
end
