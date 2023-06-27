-- test OP_SELFSLOT and OP_SELFSLOTMT generation


hstructure ItemMeta
	__index : ItemMeta; -- pointer back to itself
	new : ifunction;
	delete : ifunction;
	getName : ifunction;
	setName : ifunction;
	isNamed : ifunction;
	updateHasName : ifunction;
end

hstructure Item
	meta : ItemMeta;
	m_name : string;
	m_hasName : boolean;
end

Item = hmake ItemMeta {};
Item.__index = Item;

function Item.new(self:ItemMeta, name:string)
	local o:Item = hmake Item {};
	setmetatable(o, self);
	o:setName(name);
end

function Item.getName(self:Item)
	return self.m_name;
end

function Item.setName(self:Item, name:string)
	local oldName = self:getName();
	self.m_name = name;
	self:updateHasName();
	return oldName;
end

function Item.updateHasName(self:Item)
	self.m_hasName = self:getName() ~= nil;
end

function Item.hasName(self:Item)
	return self.m_hasName;
end
