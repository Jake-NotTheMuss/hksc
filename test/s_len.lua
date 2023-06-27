hstructure S
	o : object;
end

local s:S = hmake S{};

local len : number = #s; -- should be statically type-checked

local s = hmake S{};

local len : number = #s; -- CHECKTYPE
