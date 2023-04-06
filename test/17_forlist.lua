local t = {1, 2, 3};
for x, y in pairs(t) do
	print(x);
end

for a, b, c, d, e, f, g, h, i, j, k in pairs(t) do
	print(x);
end

for x in nil do
	print(x);
end

for x, y in a, b, c do
	print(x);
end
