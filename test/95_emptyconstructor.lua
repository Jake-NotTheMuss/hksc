-- test second pass handling of empty table constructors

local a = {};

a = {};

call(1, {}, 2, {});

({})();

a = {} + 1;

local b = x .. {} .. y;
