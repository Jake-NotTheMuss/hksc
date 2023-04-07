while true do
	while a do
		if false then
			local a  =12;
		end
	end
end

-- the following is semantically equivalent and generates the same code
-- while true do
-- 	while true do
-- 		if a then
-- 		end
-- 		if false then
-- 			local a = 12;
-- 		end
-- 	end
-- end
