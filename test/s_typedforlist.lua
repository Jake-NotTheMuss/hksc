
for x:string, y:number in g do
	x = g;
	return x;
end

for x:string, y:object, z:userdata in pairs(T) do
	x, y, z = f();
end

for x:string, y:object, z:userdata in a, b, c, d do
	x, y, z = f();
end
