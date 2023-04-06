local t = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
print(t[1][2]);
for x = 0, #t do
	for y = 0, #t[x] do
		print(t[x][y]);
	end
end
t = {a = {a = 1, b = 2}, b = {a = 1, b = 2}};
print(t.a.b);
for x in pairs(t) do
	for y in pairs(t[x]) do
		print(t[x][y]);
	end
end
