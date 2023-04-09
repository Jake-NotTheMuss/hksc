--[[
decompiler will call see this as:

if false then
end

which is sementically equivalent and generates the same code
]]

repeat
	break;
until true;
