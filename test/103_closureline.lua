-- see how the second pass emits function headers when OP_CLOSURE is the first
-- code 

function a()
	function b() -- see note [1]
		return;
	end
	return;
end

function a() function b()
		return;
	end
	return;
end

function a()

	function b()
		return;
	end
end


a = function ()
	b = function () -- see note [2]
		return;
	end
	return;
end

a = function()
	b = function()
		c = function()
			d = function() e = function() end
			end
		end
	end
end

--[[
NOTES:
[1] In this case, OP_CLOSURE is mapped to line 7, where function 'b' ends; in
this case the analyzer can look at the next code and see that is is a store,
OP_SETGLOBAL, and it maps to line 5, because of the function declaration being
named, if the function were an anonymous one being assigned to 'b',
OP_SETGLOBAL would map to line 7 like OP_CLOSURE

[2] In this case, the line for OP_SETGLOBAL is not useful as it maps to the
same line as OP_CLOSURE, line 28. The analyzer can use the line mapping for
code in functino 'b' as an appropriate reference for how much line-room is
available, as long as that code is not OP_CLOSURE. If it is, the analyzer uses
the first code of the nested function. It will keep traversing the child
function list until it finds a function whose first code is not OP_CLOSURE
]]
