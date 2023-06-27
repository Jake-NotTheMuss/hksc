-- test an invalid slot error

hstructure S
	n : number;
end

local s:S = hmake S{};

local n = s.n;

local x = s.x; -- invalid slot
