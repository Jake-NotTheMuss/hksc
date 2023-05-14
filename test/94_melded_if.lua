-- test the first pass analyzer's ability to differentiate if-statement
-- conditions and meld nested blocks together when appropriate

if a and b then
	return a;
end

-- can be conbined into 1 block
if a then
	if b then
		return a;
	end
end

-- must be kept as 2 separate blocks
if a then
	local b = b;
	if b then
		return a;
	end
end

if a and b then
	return a;
else
	return b;
end

if a then
	local b = b;
	if b then
	end
end
