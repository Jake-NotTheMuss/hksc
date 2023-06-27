
for i:number=0, 5 do
	i = g;
	return i;
end

-- this shouldn't emit any type checks
for i:object=0, 5 do
	i = g;
	return i;
end
