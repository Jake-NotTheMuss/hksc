-- test the decompiler's handling of repeat-break-until-true inside a
-- while-loop

-- repeat-break-until-true inside a while-loop
while a do
	repeat
		do break; end
	until true;
	local a = 12;
end

-- repeat-break-until-true inside a while-loop
while true do
	local b;
	local function c()
		b = b + 1;
	end
	repeat
		break;
	until true;
	break;
end

-- repeat-break-until-true inside a block inside a while-loop
while a do
	local b = 12;
	local function c()
		b = b + 1;
	end
	do
		local d = 34;
		local function e()
			d = d + 1;
		end
		repeat
			break;
		until true;
	end
	break;
end

-- repeat-break-until-true inside an else-block inside a while-loop, the
-- else-block closes variables
while a do
	local b = 12;
	local function c()
		b = b + 1;
	end
	if a then
		return a
	end
	if a then
		return a;
	else
		local d = 34;
		local function e()
			d = d + 1;
		end
		break;
	end
	break;
end

-- repeat-break-until-true inside a block inside an else-block inside a
-- while-loop
while a do
	local b = 12;
	local function c()
		b = b + 1;
	end
	if a then
		return a;
	else
		do
			local d = 34;
			local function e()
				d = d + 1;
			end
			repeat
				break;
			until true;
		end
		break;
	end
	break;
end

-- same as above but with a single if-block preceding the if-else blocks
while a do
	local b = 12;
	local function c()
		b = b + 1;
	end
	if a then
		return a
	end
	if a then
		return a;
	else
		do
			local d = 34;
			local function e()
				d = d + 1;
			end
			repeat
				break;
			until true;
		end
		break;
	end
	break;
end
