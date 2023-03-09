local point_array = {
	{x=0,y=0},
	{x=0,y=0},
	{x=0,y=0},
	{x=0,y=0},
	{x=0,y=0},
	{x=0,y=0},
	{x=0,y=0},
	{x=0,y=0},
	{x=0,y=0},
	{x=0,y=0}
};

local my_point = 10;

point_array[my_point].x = 12.34;
point_array[my_point].y = 56.78;

print(point_array[my_point].x .. ", " .. point_array[my_point].y);
