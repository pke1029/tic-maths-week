-- title:  MW20-Quaternions
-- author: pke1029
-- desc:   Interactive slides for Maths Week Ireland 2020: Quaternions.
-- script: lua


WIDTH = 240
HEIGHT = 136
HEIGHT2 = 68
PI = 3.1415
sin = math.sin
cos = math.cos
max = math.max
min = math.min
abs = math.abs
sqrt = math.sqrt
ceil = math.ceil
floor = math.floor
random = math.random


vec3d = {

	new = function(x, y, z)
		local v = {x, y, z}
		setmetatable(v, vec3d.mt)
		return v
	end,

	mt = {

		__add = function (u, v)
			return vec3d.new(u[1]+v[1], u[2]+v[2], u[3]+v[3])
		end,

		__sub = function(u, v)
			return vec3d.new(u[1]-v[1], u[2]-v[2], u[3]-v[3])
		end,

		__mul = function(k, v)
			if type(k) == "table" then
				return vec3d.new(k[1]*v[1], k[2]*v[2], k[3]*v[3])
			else
				return vec3d.new(k*v[1], k*v[2], k*v[3])
			end
		end,

		__div = function(v, k)
			if type(k) == "table" then
				return vec3d.new(v[1]/k[1], v[2]/k[2], v[3]/k[3])
			else
				return vec3d.new(v[1]/k, v[2]/k, v[3]/k)
			end
		end,

		__pow = function(v, k)
			return vec3d.new(v[1]^k, v[2]^k, v[3]^k)
		end,

		__eq = function(u, v)
			return u[1] == v[1] and u[2] == v[2] and u[3] == v[3]
		end,

		__tostring = function(v)
			return "(" .. v[1] .. "," .. v[2] .. "," .. v[3] .. ")"
		end,

		__concat = function(s, v)
			return s .. "(" .. v[1] .. "," .. v[2] .. "," .. v[3] .. ")"
		end,

		__index = {

			rotate_quaternion = function(self, theta, u)
				u = u / vec3d.norm(u)
				local c = cos(theta/2)
				local s = sin(theta/2)
				local k = vec3d.dot(s*u, self)
				local w = vec3d.cross(s*u, self)

				local v = 2*k*s*u + (c*c-s*s)*self + 2*c*w
				self[1] = v[1]
				self[2] = v[2]
				self[3] = v[3]
				return self
			end,

			rotate = function(self, ax, ay, az)
				local x1 = self[1]
				local y1 = self[2] * cos(ax) - self[3] * sin(ax)
				local z1 = self[2] * sin(ax) + self[3] * cos(ax)
				local x2 = x1 * cos(ay) + z1 * sin(ay)
				local y2 = y1
				local z2 = -x1 * sin(ay) + z1 * cos(ay)
				self[1] = x2 * cos(az) - y2 * sin(az)
				self[2] = x2 * sin(az) + y2 * cos(az)
				self[3] = z2
				return self
			end,

			normalise = function(self)
				local r = vec3d.norm(self)
				self[1] = self[1] / r
				self[2] = self[2] / r
				self[3] = self[3] / r
				return self
			end,

			ortho_proj = function(self)
				return self[1], self[2]
			end,

			pers_proj = function(self, camera, depth)
				local v = self - camera
				local x = depth * v[1] / v[3]
				local y = depth * v[2] / v[3]
				return x, y
			end,
		}
	},

	norm = function(v)
		return sqrt(v[1]*v[1] + v[2]*v[2] + v[3]*v[3])
	end,

	dot = function(u, v)
		return u[1]*v[1] + u[2]*v[2] + u[3]*v[3]
	end,

	cross = function(u, v)
		return vec3d.new(u[2]*v[3]-v[2]*u[3], v[1]*u[3]-u[1]*v[3], u[1]*v[2]-v[1]*u[2])
	end,

	avg = function(u)
		return (u[1] + u[2] + u[3]) / 3
	end,

	rotate = function(v, ax, ay, az)
		local c = cos(ax)
		local s = sin(ax)
		local x1 = v[1]
		local y1 = v[2] * c - v[3] * s
		local z1 = v[2] * s + v[3] * c
		c = cos(ay)
		s = sin(ay)
		local x2 = x1 * c + z1 * s
		local y2 = y1
		local z2 = -x1 * s + z1 * c
		c = cos(az)
		s = sin(az)
		local x3 = x2 * c - y2 * s
		local y3 = x2 * s + y2 * c
		local z3 = z2
		return vec3d.new(x3, y3, z3)
	end,

	rotate_quaternion = function(v, theta, u)
		u = u / vec3d.norm(u)
		local c = cos(theta/2)
		local s = sin(theta/2)
		local k = vec3d.dot(s*u, v)
		local w = vec3d.cross(s*u, v)
		local v = 2*k*s*u + (c*c-s*s)*v + 2*c*w
		return v
	end
}


mathFun = {
	between = function(x, a, b)
		return x >= a and x < b
	end,

	clamp = function(x, a, b)
		if mathFun.between(x, a, b) then return x end
		if x < a then return a end
		if x >= b then return b end
	end,

	bool2int = function(a)
		return a and 1 or 0
	end,

	mod = function(x, a)
		return x - floor(x/a)*a
	end,

	sign = function(x)
	    return x > 0 and 1 or -1
	end,

	isClockwise = function(x1, y1, x2, y2, x3, y3)
		local a1 = x2 - x1
		local a2 = y2 - y1
		local b1 = x3 - x1
		local b2 = y3 - y1
		return (a1*b2 - b1*a2) < 0
	end,

	round = function(num, numDecimalPlaces)
	  local mult = 10^(numDecimalPlaces or 0)
	  return math.floor(num * mult + 0.5) / mult
	end,

	quaternion_mult = function(a, b)
		local w = a[1]*b[1] - a[2]*b[2] - a[3]*b[3] - a[4]*b[4]
		local x = a[1]*b[2] + a[2]*b[1] + a[3]*b[4] - a[4]*b[3]
		local y = a[1]*b[3] + a[3]*b[1] + a[4]*b[2] - a[2]*b[4]
		local z = a[1]*b[4] + a[4]*b[1] + a[2]*b[3] - a[3]*b[2]
		return {w, x, y, z}
	end,

	stereo_proj = function(q, d)
		local x = q[2]*d/(d-q[1])
		local y = q[3]*d/(d-q[1])
		local z = q[4]*d/(d-q[1])
		return vec3d.new(x, y, z)
	end
}


function mousep(md)
	if md_old == nil then md_old = false end
	local mp = not md_old and md
	md_old = md
	return mp
end

function trib(x1, y1, x2, y2, x3, y3, col)
	line(x1, y1, x2, y2, col)
	line(x2, y2, x3, y3, col)
	line(x3, y3, x1, y1, col)
end

function polyb(p, x, y, col)
	for i = 2,#p do
		line(p[i-1][1]+x, p[i-1][2]+y, p[i][1]+x, p[i][2]+y, col)
	end
	line(p[#p][1]+x, p[#p][2]+y, p[1][1]+x, p[1][2]+y, col)
end

-- taken from https://gist.github.com/sixFingers/ee5c1dce72206edc5a42b3246a52ce2e
function march(points)
    local p = #points

    local cross = function(p, q, r)
        return (q[2] - p[2]) * (r[1] - q[1]) - (q[1] - p[1]) * (r[2] - q[2])
    end

    table.sort(points, function(a, b)
        return a[1] == b[1] and a[2] > b[2] or a[1] > b[1]
    end)

    local lower = {}
    for i = 1, p do
        while (#lower >= 2 and cross(lower[#lower - 1], lower[#lower], points[i]) <= 0) do
            table.remove(lower, #lower)
        end

        table.insert(lower, points[i])
    end

    local upper = {}
    for i = p, 1, -1 do
        while (#upper >= 2 and cross(upper[#upper - 1], upper[#upper], points[i]) <= 0) do
            table.remove(upper, #upper)
        end

        table.insert(upper, points[i])
    end

    table.remove(upper, #upper)
    table.remove(lower, #lower)
    for _, point in ipairs(lower) do
        table.insert(upper, point)
    end

    return upper
end


fog = {

	-- 8 bit address 0xbfc0
	-- 4 bit address 0x17f80

	show = function()
		memcpy(0, 0xbfc0, 16320)
	end
}


mesh = {

	poly = {
		-- format: {a = {u1, u2, u3},   	(buffer coords)
	    --          b = {v1, v2, v3}, 		(temp coords)
	    --          c = {w1, w2, w3}, 		(original coords)
	    --          uv = {x1, y1, x2, y2, x3, y3}, 
	    --          col = *int, 
	    --          id = *int}
	},

	center = function(p)
		return (p[1] + p[2] + p[3]) / 3
	end,

	sort = function(p1, p2)
		return mesh.center(p1.a)[3] > mesh.center(p2.a)[3]
	end, 

	isClockwise = function(p)
		local x1, y1 = p[1]:proj()
		local x2, y2 = p[2]:proj()
		local x3, y3 = p[3]:proj()
		local a1 = x2 - x1
		local a2 = y2 - y1
		local b1 = x3 - x1
		local b2 = y3 - y1
		return (a1*b2 - b1*a2) < 0
	end,

	-- convexHull = function(points)
	-- 	local hull = {}
	-- 	table.sort(points, function(p1, p2) return p1[1] < p2[1] end)
	-- 	table.insert(hull, points[1])
	-- 	for i = 2,#points do
	-- 		local x1, y1 = table.unpack(hull[#hull])
	-- 		local x2, y2 = table.unpack(points[i])
	-- 		local flag = true
	-- 		for j = 1,#points do
	-- 			local x3, y3 = table.unpack(points[j])
	-- 			flag = not mathFun.isClockwise(x1, y1, x2, y2, x3, y3)
	-- 		end
	-- 		if flag == true then
	-- 			table.insert(hull, points[i])
	-- 		end
	-- 	end
	-- 	print(#hull, 10, 10)
	-- 	return hull
	-- end,

	rotate_euler = function(ax, ay, az)
		for i,p in pairs(mesh.poly) do
			if p.id == 1 or p.id == 5 then
				p.a[1] = vec3d.rotate(p.c[1], ax, ay, az)
				p.a[2] = vec3d.rotate(p.c[2], ax, ay, az)
				p.a[3] = vec3d.rotate(p.c[3], ax, ay, az)
			elseif p.id == 2 then
				p.a[1] = vec3d.rotate(p.c[1], ax, ay, az)
				p.a[2] = vec3d.rotate(p.c[2], ax, ay, az)
				p.a[3] = vec3d.rotate(p.c[3], ax, ay, az)
			elseif p.id == 3 then
				p.a[1] = vec3d.rotate(p.c[1], 0, ay, az)
				p.a[2] = vec3d.rotate(p.c[2], 0, ay, az)
				p.a[3] = vec3d.rotate(p.c[3], 0, ay, az)
			elseif p.id == 4 then
				p.a[1] = vec3d.rotate(p.c[1], 0, 0, az)
				p.a[2] = vec3d.rotate(p.c[2], 0, 0, az)
				p.a[3] = vec3d.rotate(p.c[3], 0, 0, az)
			end
		end
	end,

	rotate_quaternion = function(theta, u)
		for i,p in pairs(mesh.poly) do
			if p.id == 1 then
				p.a[1] = vec3d.rotate_quaternion(p.b[1], theta, u)
				p.a[2] = vec3d.rotate_quaternion(p.b[2], theta, u)
				p.a[3] = vec3d.rotate_quaternion(p.b[3], theta, u)
			end
		end
	end,

	rotate_partial = function(a, b, u)
		for i,p in pairs(mesh.poly) do
			u = u / vec3d.norm(u)
			local q1 = {cos(a/2), u[1]*sin(a/2), u[2]*sin(a/2), u[3]*sin(a/2)}
			local q2 = {cos(b/2), -u[1]*sin(b/2), -u[2]*sin(b/2), -u[3]*sin(b/2)}
			if p.id == 1 then
				local v1 = mathFun.quaternion_mult(mathFun.quaternion_mult(q1, {0, p.c[1][1], p.c[1][2], p.c[1][3]}), q2)
				local v2 = mathFun.quaternion_mult(mathFun.quaternion_mult(q1, {0, p.c[2][1], p.c[2][2], p.c[2][3]}), q2)
				local v3 = mathFun.quaternion_mult(mathFun.quaternion_mult(q1, {0, p.c[3][1], p.c[3][2], p.c[3][3]}), q2)
				-- p.a[1] = vec3d.new(v1[2], v1[3], v1[4])
				-- p.a[2] = vec3d.new(v2[2], v2[3], v2[4])
				-- p.a[3] = vec3d.new(v3[2], v3[3], v3[4])
				p.a[1] = mathFun.stereo_proj(v1, 50)
				p.a[2] = mathFun.stereo_proj(v2, 50)
				p.a[3] = mathFun.stereo_proj(v3, 50)
			end
		end
	end,

	apply_rot = function()
		for i,p in pairs(mesh.poly) do
			p.b[1] = p.a[1]
			p.b[2] = p.a[2]
			p.b[3] = p.a[3]
		end
	end,

	reset_rot = function()
		for i,p in pairs(mesh.poly) do
			p.b[1] = p.c[1]
			p.b[2] = p.c[2]
			p.b[3] = p.c[3]
		end
	end,

	draw1 = function(x0, y0, mode)
		if x0 == nil then x0 = 68 end
		if y0 == nil then y0 = 68 end
		table.sort(mesh.poly, mesh.sort)
		-- mesh
		for i,p in pairs(mesh.poly) do
			if p.id == 1 and not mesh.isClockwise(p.a) then
				local uv = p.uv
				local x1, y1 = p.a[1]:proj()
				local x2, y2 = p.a[2]:proj()
				local x3, y3 = p.a[3]:proj()
				if mode == 0 then
					textri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, uv[1], uv[2], uv[3], uv[4], uv[5], uv[6])
				elseif mode == 1 then
					tri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, 1)
					trib(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, 12)
				end
			end
		end
	end,

	draw2 = function(x0, y0, mode)
		if x0 == nil then x0 = 68 end
		if y0 == nil then y0 = 68 end
		table.sort(mesh.poly, mesh.sort)
		for i,p in pairs(mesh.poly) do
		-- mesh
			if p.id == 1 and not mesh.isClockwise(p.a) then
				local uv = p.uv
				local x1, y1 = p.a[1]:proj()
				local x2, y2 = p.a[2]:proj()
				local x3, y3 = p.a[3]:proj()
				if mode == 0 then
					textri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, uv[1], uv[2], uv[3], uv[4], uv[5], uv[6])
				elseif mode == 1 then
					tri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, 1)
					trib(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, 12)
				end
		-- x, y, z axes
			elseif mathFun.between(p.id, 2, 5) then
				local x1, y1 = p.a[1]:proj()
				local x2, y2 = p.a[2]:proj()
				local x3, y3 = p.a[3]:proj()
				tri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, p.col)
			end
		end
	end,

	draw3 = function(x0, y0, mode)
		if x0 == nil then x0 = 68 end
		if y0 == nil then y0 = 68 end
		table.sort(mesh.poly, mesh.sort)
		-- mesh
		for i,p in pairs(mesh.poly) do
			if p.id == 1 and not mesh.isClockwise(p.a) then
				local uv = p.uv
				local x1, y1 = p.a[1]:proj()
				local x2, y2 = p.a[2]:proj()
				local x3, y3 = p.a[3]:proj()
				if mode == 0 then
					textri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, uv[1], uv[2], uv[3], uv[4], uv[5], uv[6])
				elseif mode == 1 then
					tri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, 1)
					trib(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, 12)
				end
			elseif p.id == 5 then
				local uv = p.uv
				local x1, y1 = p.a[1]:proj()
				local x2, y2 = p.a[2]:proj()
				local x3, y3 = p.a[3]:proj()
				if mode == 0 then
					textri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, uv[1], uv[2], uv[3], uv[4], uv[5], uv[6])
				elseif mode == 1 then
					tri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, 3)
					trib(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, 12)
				end
			end
		end
	end,

	anim_axes = function()
		for i,p in pairs(mesh.poly) do
			if mathFun.between(p.id, 2, 5) then
				p.a[2]:rotate_quaternion(-t, p.a[1])
				p.a[3]:rotate_quaternion(-t, p.a[1])
			end
		end
	end,

	init = function()

		-- empty polygons
		mesh.poly = {}

		ico = {

			verts = {
				{ 1, 0, 2},
				{ 0, 2, 1},
				{ 2, 1, 0},
				{ 2,-1, 0},
				{ 0,-2, 1},
				{-1, 0, 2},
				{ 0, 2,-1},
				{ 1, 0,-2},
				{ 0,-2,-1},
				{-2,-1, 0},
				{-2, 1, 0},
				{-1, 0,-2}
			},

			faces = {
				{ 1, 2, 3},
				{ 1, 3, 4},
				{ 1, 4, 5},
				{ 1, 5, 6},
				{ 1, 6, 2},
				{ 2, 7, 3},
				{ 3, 7, 8},
				{ 3, 8, 4},
				{ 4, 8, 9},
				{ 4, 9, 5},
				{ 5, 9,10},
				{ 5,10, 6},
				{ 6,10,11}, 
				{ 6,11, 2},
				{ 2,11, 7},
				{ 8, 7,12},
				{ 9, 8,12},
				{10, 9,12},
				{11,10,12},
				{ 7,11,12},
			}, 

			uvs = {
				{96, 32, 96, 64, 64, 32},
				{96, 32, 64, 32, 64, 0},
				{96, 32, 64, 0, 96, 0},
				{96, 0, 128, 0, 128, 32},
				{96, 0, 128, 32, 96, 32},
				{96, 64, 64, 64, 64, 32},
				{64, 64, 32, 64, 32, 32},
				{64, 64, 32, 32, 64, 32},
				{64, 32, 32, 32, 32, 0},
				{64, 32, 32, 0, 64, 0},
				{0, 0, 32, 0, 32, 32},
				{32, 64, 32, 96, 0, 64},
				{0, 64, 32, 96, 0, 96},
				{128, 32, 128, 64, 96, 32},
				{96, 32, 128, 64, 96, 64},
				{32, 64, 0, 64, 0, 32},
				{32, 32, 32, 64, 0, 32},
				{0, 0, 32, 32, 0, 32},
				{0, 96, 32, 96, 32, 128},
				{0, 128, 0, 96, 32, 128}
			}
		}

		-- apply scale, rotation
		for i,v in pairs(ico.verts) do
			setmetatable(v, vec3d.mt)
			v = 20 * v
			v:rotate(0.46, 0, 0)
			ico.verts[i] = v
		end

		for i,f in pairs(ico.faces) do
			local p = {}
			p.a = {ico.verts[f[1]], ico.verts[f[2]], ico.verts[f[3]]}
			p.b = {ico.verts[f[1]], ico.verts[f[2]], ico.verts[f[3]]}
			p.c = {ico.verts[f[1]], ico.verts[f[2]], ico.verts[f[3]]}
			p.uv = ico.uvs[i]
			p.id = 1
			table.insert(mesh.poly, p)
		end

		axes = {

			verts = {
				{47, 0, 0},
				{55, 4, 0},
				{55, 4*cos(2*PI/3), 4*sin(2*PI/3)},
				{55, 4*cos(4*PI/3), 4*sin(4*PI/3)},
				{0, -47, 0},
				{4, -55, 0},
				{4*cos(2*PI/3), -55, 4*sin(2*PI/3)},
				{4*cos(2*PI/3), -55, 4*sin(4*PI/3)},
				{0, 0, -47},
				{4, 0, -55},
				{4*cos(2*PI/3), 4*sin(2*PI/3), -55},
				{4*cos(4*PI/3), 4*sin(4*PI/3), -55},
				{0, -47, 0}
			},

			faces = {
				{1, 2, 3}, 
				{1, 3, 4}, 
				{1, 4, 2}, 
				{5, 6, 7},
				{5, 7, 8},
				{5, 8, 6},
				{9, 10, 11},
				{9, 11, 12},
				{9, 12, 10},
			},

			cols = {2, 2, 2, 6, 6, 6, 10, 10, 10},

			ids = {2, 2, 2, 3, 3, 3, 4, 4, 4}
		}

		for i,v in pairs(axes.verts) do
			setmetatable(v, vec3d.mt)
			axes.verts[i] = v
		end

		for i,f in pairs(axes.faces) do
			local p = {}
			p.a = {axes.verts[f[1]], axes.verts[f[2]], axes.verts[f[3]]}
			p.b = {axes.verts[f[1]], axes.verts[f[2]], axes.verts[f[3]]}
			p.c = {axes.verts[f[1]], axes.verts[f[2]], axes.verts[f[3]]}
			p.col = axes.cols[i]
			p.id = axes.ids[i]
			table.insert(mesh.poly, p)
		end

		axis4 = {
			
			x0 = HEIGHT2,
			y0 = HEIGHT2,
			radius = 47,
			x = 0,
			y = 0,
			z = 0,
			mo = false,
			verts = {
				vec3d.new(47, 0, 0),
				vec3d.new(55, 4, 0),
				vec3d.new(55, 4*cos(2*PI/3), 4*sin(2*PI/3)),
				vec3d.new(55, 4*cos(4*PI/3), 4*sin(4*PI/3))
			},
			faces = {
				{1, 2, 3},
				{1, 3, 4},
				{1, 4, 2}
			},

			__index = {

				draw = function(self, x0, y0)
					for i,f in pairs(self.faces) do
						local x1, y1 = self.verts[f[1]]:proj()
						local x2, y2 = self.verts[f[2]]:proj()
						local x3, y3 = self.verts[f[3]]:proj()
						tri(x1+x0, y1+y0, x2+x0, y2+y0, x3+x0, y3+y0, 4)
					end
					if self.mo == true then
						local points = {}
						for i,p in pairs(self.verts) do
							local x, y = p:proj()
							table.insert(points, {x, y})
						end
						polyb(march(points), x0, y0, 3)
					end
				end,

				update = function(self)
					-- check if mouse over
					local x = mx - self.x0
					local y = my - self.y0
					self.mo = sqrt(x*x + y*y) < self.radius+10
					-- update position when clicked
					if self.mo and md then
						self:getVerts(mx, my)
						mesh.apply_rot()
						quaternion.slider["val"] = 0
					end
					-- rotate cursor
					self.verts[2] = vec3d.rotate_quaternion(self.verts[2], -1/60, self.verts[1])
					self.verts[3] = vec3d.rotate_quaternion(self.verts[3], -1/60, self.verts[1])
					self.verts[4] = vec3d.rotate_quaternion(self.verts[4], -1/60, self.verts[1])
				end,

				getVerts = function(self, x, y)
					self.x = x - self.x0
					self.y = y - self.y0
					local l = sqrt(self.x*self.x + self.y*self.y)
					if l < self.radius then
						self.z = -sqrt(self.radius*self.radius-self.x*self.x-self.y*self.y)
						self.verts[1] = vec3d.new(self.x, self.y, self.z)
					else
						self.x = self.radius/l * self.x
						self.y = self.radius/l * self.y
						self.z = 0
						self.verts[1] = vec3d.new(self.x, self.y, self.z)
					end
					l = sqrt(self.z*self.z + self.y*self.y)
					if l == 0 then
						self.verts[2] = 55/47*self.verts[1] - vec3d.new(0, 4, 0)
					else
						self.verts[2] = 55/47*self.verts[1] - vec3d.new(0, -4*self.z/l, 4*self.y/l)
					end
					self.verts[2] = vec3d.rotate_quaternion(self.verts[2], -t, self.verts[1])
					self.verts[3] = vec3d.rotate_quaternion(self.verts[2], 2*PI/3, self.verts[1])
					self.verts[4] = vec3d.rotate_quaternion(self.verts[2], 4*PI/3, self.verts[1])
				end
			}
		}

		setmetatable(axis4, axis4)
		axis4:getVerts(axis4.x0, axis4.y0)

		octa = {

			verts = {
				{ 1,  0,  0},
				{-1,  0,  0},
				{ 0,  1,  0},
				{ 0, -1,  0},
				{ 0,  0,  1},
				{ 0,  0, -1}
			},

			faces = {
				{5, 1, 3},
				{5, 3, 2},
				{5, 2, 4},
				{5, 4, 1},
				{6, 3, 1},
				{6, 2, 3},
				{6, 4, 2},
				{6, 1, 4}
			},

			uvs = {
				{96, 96, 64, 64, 64, 96},
				{96, 96, 64, 96, 96, 128},
				{32, 128, 64, 128, 32, 96},
				{96, 96, 96, 64, 64, 64},
				{64, 96, 64, 64, 32, 64},
				{64, 128, 96, 128, 64, 96},
				{64, 96, 32, 96, 64, 128},
				{64, 96, 32, 64, 32, 96}
			},
		}

		-- apply scale, rotation
		for i,v in pairs(octa.verts) do
			setmetatable(v, vec3d.mt)
			v = 10 * v + vec3d.new(80, 0, 0)
			octa.verts[i] = v
		end

		for i,f in pairs(octa.faces) do
			local p = {}
			p.a = {octa.verts[f[1]], octa.verts[f[2]], octa.verts[f[3]]}
			p.b = {octa.verts[f[1]], octa.verts[f[2]], octa.verts[f[3]]}
			p.c = {octa.verts[f[1]], octa.verts[f[2]], octa.verts[f[3]]}
			p.uv = octa.uvs[i]
			p.id = 5
			table.insert(mesh.poly, p)
		end
	end
}


ui = {

	header = function(text)
		rect(0, 0, WIDTH, 8, 15)
		print(text, 1, 1, 13)
	end,

	footer = function(text)
		rect(0, HEIGHT-8, WIDTH, 8, 15)
		print(text, 1, HEIGHT-7, 13)
	end,

	print_shadow = function(text, x, y, col1, col2)
		if col1 == nil then col1 = 15 end
		if col2 == nil then col2 = 0 end
		print(text, x+1, y+1, col2)
		print(text, x, y, col1)
	end,

	print_bob = function(text, x, y, col)
		print(text, x, y+mathFun.mod(t, 2), col)
	end,

	print_centered = function(text, y, col)
		local width = print(text, 0, -6)
		print(text, (240-width)//2, y, col)
	end,

	print_alt = function(dict, x, y)
	 	-- format: font = {"text"}, print = {"text", col}
		local w = 0
		for i,p in pairs(dict) do
			if p[2] then
				print(p[1], x+w, y, p[2], true)
				w = w + print(p[1], 0, -6, p[2], true) + 1
			else
				font(p[1], x+w, y)
				w = w + font(p[1], 0, -6, -1, 6, 6, true) 
			end
		end
	end,

	slider = {

		new = function(text, x, y, sym, col)
			local s = {text=text, x=x, y=y, val=0, mo=false, sym=sym, col=col}
			setmetatable(s, ui.slider.mt)
			return s
		end,

		mt = {
			__index = {

				update = function(self)
					self.mo = mathFun.between(mx, self.x-5, self.x+85) and mathFun.between(my, self.y-4, self.y+21)
					if md and self.mo then 
						self.val = mathFun.clamp((mx-self.x) * 4.5, 0, 360)
					end
				end,

				draw = function(self)
					if self.mo == true then
						rectb(self.x-5, self.y-4, 90, 24, 13)
					end
					font(self.sym, self.x, self.y)
					print(self.text, self.x+11, self.y, self.col)
					print(string.format("%3.0f", self.val), self.x+63, self.y, 12, true)
					rect(self.x, self.y+11, 80, 2, 13)
					rect(self.x-1+self.val/4.5, self.y+8, 3, 8, 12)
				end,
			}
		}
	},

	button = {

		new = function(id, x, y)
			local b = {id=id, x=x, y=y, mo=false, toggle=false, pressed=false}
			setmetatable(b, ui.button.mt)
			return b
		end, 

		mt = {
			__index = {

				update = function(self)
					self.mo = mathFun.between(mx, self.x, self.x+8) and mathFun.between(my, self.y, self.y+8)
					if self.mo and mp == true then
						if type(self.click) == 'function' then
							self:click()
						end
					end
				end,

				draw = function(self)
					local k = mathFun.bool2int(self.toggle)
				 	if self.mo == true then 
				 		k = k + 16 
				 	end
				 	spr(self.id+k, self.x, self.y, 0)
				end
			}
		}
	}
}


stars = {

	pos = {},

	load = function(n)
		for i = 1,n do
			local x = random()*WIDTH
			local y = random()*HEIGHT
			table.insert(stars.pos, {x, y})
		end
	end,

	update = function()
		for i = 1,#stars.pos do
			stars.pos[i][1] = mathFun.mod(stars.pos[i][1] + 1, WIDTH)
		end
	end,

	draw = function()
		for i,v in pairs(stars.pos) do
			pix(v[1], v[2], 12)
		end
	end,
}


b1 = ui.button.new(385, 232, 128)
b1.click = function(self) current = current+1 end
b2 = ui.button.new(384, 224, 128)
b2.click = function(self) current = current-1 end
b3 = ui.button.new(395, 216, 128)
function b3.click(self) 
	euler.slider1["val"] = 0
	euler.slider2["val"] = 0
	euler.slider3["val"] = 0
	quaternion.slider["val"] = 0
	mesh.reset_rot()
	axis4:getVerts(axis4.x0, axis4.y0)
	quaternion3.slider1["val"] = 0
	quaternion3.slider2["val"] = 0
end
b4 = ui.button.new(386, 208, 128)
function b4.click(self) 
	self.toggle = not self.toggle
	if self.toggle then
		music(0, 2)
	else
		music(1)
	end
end
b5 = ui.button.new(393, 200, 128)
b5.click = function(self) self.toggle = not self.toggle end


title = {

	ax = 0,
	ay = 0,
	az = 23.5 * PI/180,

	text = "\n  3D rotation in 4D space \n\nPresenter\n  Kenny Pang\n\nInteractive slides available at\n  tic80.com/play?cart=1452",

	tic = function()
		cls()
		-- update
		title.ay = title.ay + 0.01
		mesh.rotate_euler(title.ax, title.ay, title.az)
		stars.update()
		-- draw
		stars.draw()
		mesh.draw3(60, HEIGHT2, mathFun.bool2int(b5.toggle))
		-- mesh.draw3(WIDTH/2, HEIGHT2, mathFun.bool2int(b5.toggle))
		print("Quaternions", 120, 39+mathFun.mod(t, 2), 12)
		print(title.text, 120, 40+mathFun.mod(t, 2), 12, false, 1, true)
		ui.footer("Maths Week Ireland 2020")
	end
}

euler = {

	ax = 0,
	ay = 0,
	az = 0,

	slider1 = ui.slider.new("(x-axis)", 140, 35, "A", 2),
	slider2 = ui.slider.new("(y-axis)", 140, 60, "B", 6),
	slider3 = ui.slider.new("(z-axis)", 140, 85, "C", 10),

	tic = function()
		cls()
		--update
		euler.slider1:update()
		euler.slider2:update()
		euler.slider3:update()
		euler.ax = euler.slider1["val"] * 2*PI/360
		euler.ay = euler.slider2["val"] * 2*PI/360
		euler.az = euler.slider3["val"] * 2*PI/360
		mesh.rotate_euler(euler.ax, euler.ay, euler.az)
		mesh.anim_axes()
		-- draw
		mesh.draw2(HEIGHT2, HEIGHT2, mathFun.bool2int(b5.toggle))
		euler.slider1:draw()
		euler.slider2:draw()
		euler.slider3:draw()
		ui.header("Euler angles")
		ui.footer("")
	end
}

matrix = {

	x = 50,
	y = 18,

	tic = function()
		cls(0)
		print("Rx = ", matrix.x-20, matrix.y+8, 12)
		ui.print_alt({{"   1  ", 12}, {"   1  ", 12}, {"   1  ", 12}}, matrix.x, matrix.y)
		ui.print_alt({{"   0  ", 12}, {"  cos", 12}, {"A"}, {" -sin", 12}, {"A"}}, matrix.x, matrix.y+8)
		ui.print_alt({{"   0  ", 12}, {"  sin", 12}, {"A"}, {"  cos", 12}, {"A"}}, matrix.x, matrix.y+16)
		print("Ry = ", matrix.x-20, matrix.y+40, 12)
		ui.print_alt({{"  cos", 12}, {"B"}, {"   0  ", 12}, {"  sin", 12}, {"B"}}, matrix.x, matrix.y+32)
		ui.print_alt({{"   0  ", 12}, {"   1  ", 12}, {"   0  ", 12}}, matrix.x, matrix.y+40)
		ui.print_alt({{" -sin", 12}, {"B"}, {"   0  ", 12}, {"  cos", 12}, {"B"}}, matrix.x, matrix.y+48)
		print("Rz = ", matrix.x-20, matrix.y+72, 12)
		ui.print_alt({{"  cos", 12}, {"C"}, {" -sin", 12}, {"C"}, {"   0  ", 12}}, matrix.x, matrix.y+64)
		ui.print_alt({{"  sin", 12}, {"C"}, {"  cos", 12}, {"C"}, {"   0  ", 12}}, matrix.x, matrix.y+72)
		ui.print_alt({{"   0  ", 12}, {"   0  ", 12}, {"   1  ", 12}}, matrix.x, matrix.y+80)
		spr(256, matrix.x, matrix.y, 0, 1, 0, 0, 1, 3)
		spr(257, matrix.x+110, matrix.y, 0, 1, 0, 0, 1, 3)
		spr(256, matrix.x, matrix.y+32, 0, 1, 0, 0, 1, 3)
		spr(257, matrix.x+110, matrix.y+32, 0, 1, 0, 0, 1, 3)
		spr(256, matrix.x, matrix.y+64, 0, 1, 0, 0, 1, 3)
		spr(257, matrix.x+110, matrix.y+64, 0, 1, 0, 0, 1, 3)
		print("v' = Rz * Ry * Rx * v ", matrix.x-20, matrix.y+96, 12)
		spr(258, 190, 100, 0, 2, 0, 0, 2, 2)
		print("Wait, it's all\nmatrix?", 185, 84+mathFun.mod(t, 2), 12, false, 1, true)
		ui.header("Rotation matrix")
		ui.footer("")
	end
}

hamilton = {

	tic = function()
		fog.show()
		ui.header("W.R. Hamilton")
		print("Irish mathematician\nSir William Rowan Hamilton", 140, 60, 12, false, 1, true)
		ui.footer("")
	end
}

complex = {

	y = 40,

	tic = function()
		cls()
		print("Complex numbers", 20, complex.y, 12)
		spr(432, 100, complex.y+10, 0, 1, 0, 0, 4, 1)
		print("Quaternions", 20, complex.y+30, 12)
		spr(448, 60, complex.y+40, 0, 1, 0, 0, 14, 1)
		-- ui.print_centered("i*i = j*j = k*k = i*j*k = -1", complex.y+40, 12)
		ui.header("Quaternions")
		ui.footer("")
	end
}

quaternion = {

	slider = ui.slider.new("", 140, 50, "D", 11),

	tic = function()
		cls()
		-- update
		quaternion.slider:update()
		mesh.rotate_quaternion(quaternion.slider["val"] * 2*PI/360, axis4.verts[1])
		axis4:update()
		-- draw
		mesh.draw1(HEIGHT2, HEIGHT2, mathFun.bool2int(b5.toggle))
		axis4:draw(HEIGHT2, HEIGHT2)
		quaternion.slider:draw()
		print("(angle of rot.)", 149, 50, 11, false, 1, true)
		print("Select the axis of \nrotation by clicking \non the mesh.", 130, 90)
		ui.header("Quaternion rotation I")
		ui.footer("")
	end
}

quaternion2 = {

	x = 45,
	y = 40,

	tic = function()
		cls()
		ui.print_centered("v' = p * v * q", quaternion2.y-2, 12)
		print("Rotation quaternions", 20, quaternion2.y+12, 12)
		print("p = cos   + sin   * (", quaternion2.x, quaternion2.y+30, 12)
		print("q = cos   - sin   * (", quaternion2.x, quaternion2.y+50, 12)
		spr(464, quaternion2.x+90, quaternion2.y+28, 0, 1, 0, 0, 8, 1)
		spr(480, quaternion2.x+35, quaternion2.y+25, 0, 1, 0, 0, 1, 2)
		spr(480, quaternion2.x+70, quaternion2.y+25, 0, 1, 0, 0, 1, 2)
		spr(464, quaternion2.x+90, quaternion2.y+48, 0, 1, 0, 0, 8, 1)
		spr(480, quaternion2.x+35, quaternion2.y+45, 0, 1, 0, 0, 1, 2)
		spr(480, quaternion2.x+70, quaternion2.y+45, 0, 1, 0, 0, 1, 2)
		ui.header("Quaternion rotation II")
		ui.footer("")
	end
}

quaternion3 = {

	slider1 = ui.slider.new("(quat. p)", 140, 60, "DJ", 11),
	slider2 = ui.slider.new("(quat. q)", 140, 85, "DK", 11),

	tic = function()
		cls()
		-- update
		quaternion3.slider1:update()
		quaternion3.slider2:update()
		axis4:update()
		mesh.rotate_partial(quaternion3.slider1["val"] * 2*PI/360, quaternion3.slider2["val"] * 2*PI/360, axis4.verts[1])
		-- draw
		mesh.draw1(HEIGHT2, HEIGHT2, mathFun.bool2int(b5.toggle))
		quaternion3.slider1:draw()
		quaternion3.slider2:draw()
		axis4:draw(HEIGHT2, HEIGHT2)
		print("What if 0 is different \nfor p and q ? ", 120, 30, 12)
		spr(324, 161, 29)
		ui.header("Quaternion rotation III")
		ui.footer("")
	end
}

summary = {

	x = 30,
	y = 45,

	tic = function()
		cls()
		print("1. Avoids gimbal lock issue with \n   matrix rotation.", summary.x, summary.y-8, 13)
		print("2. Fewer trigonometry functions \n   to evaluate.", summary.x, summary.y+10, 13)
		print("3. Algebraic discription of 3D \n   Rotation.", summary.x, summary.y+28, 13)
		print("4. 4D objects can be applicable \n   to a 3D world!", summary.x, summary.y+46, 12)
		ui.header("Summary")
		ui.footer("")
	end
}

extra = {

	x = 30,
	y = 45,

	tic = function()
		cls()
		spr(290, extra.x, extra.y)
		print("   3Blue1Brown", extra.x, extra.y, 12)
		spr(290, extra.x, extra.y+10)
		print("   Numberphile", extra.x, extra.y+10, 12)
		spr(291, extra.x, extra.y+20)
		print("   Things to make and do in the \n   Fourth Dimension", extra.x, extra.y+20, 12)
			spr(291, extra.x, extra.y+36)
		print("   Flatland", extra.x, extra.y+36, 12)
		ui.header("Further readings")
		ui.footer("")
	end
}

endcard = {

	tic = function()
		cls()
		local col = 4*t
		ui.print_centered("GAME OVER", 60, col)
		ui.print_centered("(Thank you for listening)", 68, col)
		print("Concept & Programming by Kenny Pang \nMusic by Tiziana Comito", 0, 114, col)
		ui.header("tic80.com/play?cart=1452")
		ui.footer("")
	end
}


t = 0
camera = vec3d.new(0, 0, -120)
depth = 100


-- initialize mesh
mesh.init()
stars.load(20)
-- create slide sequence
current = 0
slides = {title, euler, matrix, hamilton, complex, quaternion, quaternion2, quaternion3, summary, extra, endcard}
-- define porjection type
-- vec3d.mt.__index.proj = vec3d.mt.__index.ortho_proj
vec3d.mt.__index.proj = function(self) return vec3d.mt.__index.pers_proj(self, camera, depth) end
-- start music 
music(1)

function TIC()

	dt = time()/1000 - t
	t = time()/1000

	mx, my, md = mouse()
	mp = mousep(md)

	if btnp(2,60,6) then 
		current = current-1
		mesh.reset_rot()
	end
    if btnp(3,60,6) then 
    	current = current+1
    	mesh.reset_rot()
    end

    b1:update()
    b2:update()
    b3:update()
    b4:update()
    b5:update()

    current = mathFun.mod(current, #slides) 
    slides[current+1].tic()
    
    b1:draw()
    b2:draw()
    b3:draw()
    b4:draw()
    b5:draw()

    -- debug
    print(floor(mathFun.round(1/dt, -1)).."FPS", 160, 129, 14)

end

-- <TILES>
-- 000:9999999999999999999999999999999999999999999999999999999999999999
-- 001:9999999999999996999999969999996699999966999966669996666699966666
-- 002:6666669966666669666666666666666666666666666666666666666666666666
-- 003:9999999999999999666999996669666666666666666666666666666966666669
-- 004:9999999999999999999999999999999999999999999999999999999999999999
-- 005:9966666699969666999996669999966699999969999999999999999999999999
-- 006:6996699969999996999996669999666699996666999966669999999999999999
-- 007:6666999966666699666666996666666966666999666669999666999999999999
-- 008:6666669966666669666666696666669966666699666666696666666966666666
-- 009:9966666666666666996666669966666699666666999666669996666669966666
-- 010:6669999966666699666666696666666666666666666666666666666666666666
-- 011:9dd9999999dd99999999999999999999699999d9699999996699666966666999
-- 012:6666666666666666666666666666666666666666666666669666666699966666
-- 013:6666666666666666666666666666666666666666666666666666666666666666
-- 014:6666666666666666666666666666666666666666666666666666666666666666
-- 015:9999999969969999666699996666999966669999666699996666999966666996
-- 016:9999999999999999999999999999999999999999999999999999999999999999
-- 017:9996666699966666996666669966666699666666999666669996666699996666
-- 018:6666666666666666666666666666666666666666666666666666666666666666
-- 019:6666666966666666669969666699966666999696666996666666666666666666
-- 020:9999999999999999999999999999999999999999999999999999999999999999
-- 021:9999999999999999999999999999999999999999999999999999999999999999
-- 022:9999999999999999999999999999999999999999999999999999999999999999
-- 023:9969999999669999999999999999999999999999999999999996669999966699
-- 024:6666666666666666666666666666666666666666666666666666666666666666
-- 025:6699966666999669666999666666996666666996666666666666666666666666
-- 026:6666666696666666996666666966666666966666666666666666666666666666
-- 027:6666699666666666666666666666666666666666666666666666666666666666
-- 028:9999666699996666999966669999666699999666999996669999666696966966
-- 029:6666666666666666666666666666666666666666666666666666666666666666
-- 030:6666666666666666666666666666666666666666666666666666666666666666
-- 031:6666666666666666666666666666666666666666666666666666666666666999
-- 032:9999999999999999999999999999999999999999999999999999999999999999
-- 033:9999666699996666999966669999966699999666999999669999999699999999
-- 034:6666666666666666666666666966666699996666999996669999996666999999
-- 035:6666666666666666666666666666666666666666666666666666696966669999
-- 036:9999999969999999699999996699999966699999666999996666999966669999
-- 037:9999999999999999999999999999999999999999999999999999999999999999
-- 038:9999999999999999999999999999999999999999999999999999999999999999
-- 039:9999669999996669999996669969966699666666996666669996666696666696
-- 040:6666666666666666666666666666666666666666666666666666666666666666
-- 041:6666966666669666666966666669666666666666666666666666966666666996
-- 042:6666666666666966666666666666666666666666666966666669666666666666
-- 043:6666666666666666666666666666666666666666666666666666666666666666
-- 044:9669696696666966966669969966999999669999996699999969996699969966
-- 045:6666666666966666669996669699999699999999999999999999999666999966
-- 046:6666666666669966666699666669996669999996999999969999999699999999
-- 047:6666669966666669666666699999999999999699699966696666699999666999
-- 048:9999999999999999999999999999999999999999999999999999999999999999
-- 049:9999999999999999999999999999999999999999999999999999999999999999
-- 050:6666999996699999966999699966999699669999999999999996999999996999
-- 051:6999999969999999999999999999999969999999969999999999999999669999
-- 052:9999999999999999999999999999999999999999999999999999999999999999
-- 053:9999999999999999999999999999999999999999999999999999999999999999
-- 054:9966666699666666966666669666666696666666966666669966666699666666
-- 055:6666699666666666666666666666666666666666666666666666666666666666
-- 056:6666669966666999666669999966999999999999999999999999666999666666
-- 057:6666666999996669999999999999999999999999999999999999999999999999
-- 058:9666696699966696999999999999999999999999999999999999999999999999
-- 059:6666666666666666666666669666666699666666996666669966666699966666
-- 060:9999696699999696999999999999996999999996999999999999999966669999
-- 061:6669996666699966999999669666996696666999699999999699996999699996
-- 062:9999999999999999999999999999999999999999999999999999999999999999
-- 063:9999999999999999999999999999999999999999999999999999999999999999
-- 064:9999999999999999999999999999999999999999999999999999999999999999
-- 065:9999999999999999999999999999999999999999999999999999999999999999
-- 066:9999969999999666966666666666666696666666966666669666666696666666
-- 067:9996699999996999669999996699999966699999666999996666699966666699
-- 068:9999999999999999999999999999999999999999999999999999999999999999
-- 069:9999999999999999999999999999999999999999999999999999999999999999
-- 070:9996666699996666999996669999996699999996999999999999999999999999
-- 071:6666666666666666666666666666666666666666966666669666666699666666
-- 072:9966666699966699999999999999999999999999999999999999999999999999
-- 073:9999999999999999999999999999999999999999999999999999999999999999
-- 074:9999999999999999999999999999999999999999999999999999999999999999
-- 075:9996666699966666999666669966666699666666996666669996999999969999
-- 076:6666699966666666666666669666666696666666966666669966666699996666
-- 077:9996996666999999666999996666669966666699666666696666666966666669
-- 078:6999999969999999669999999669999999699999966699999966699999966999
-- 079:9999999999999999999999999999999999999999999999999999999999999999
-- 080:9999999999999999999999999999999999999999999999999999999999999999
-- 081:9999999999999999999999999999999999999999999999969999996699996666
-- 082:9666666696666666996666669966666666666666666666666666666666666666
-- 083:6666669966666699666666996666666966666666666666666666666666666666
-- 084:9999999999999999999999999999999999999999999999999999999999999999
-- 085:9999999999999999999999999999999999999999999999999999999999999999
-- 086:9999999999999999999999999999999999999999999999999999999999999966
-- 087:9966666699666666966666669666666699666666966666666666666666666666
-- 088:9999999999999999999999999999999999999999999999999999999999999999
-- 089:9999999999999999999999999999999999999999999999999999999999999999
-- 090:9999999999999999999999999999999999999999999999999999999999999999
-- 091:9999999999999999999999999999999999999999999999999999999999999999
-- 092:9999999699999999999999999999999999999999999999999999999999999999
-- 093:6666666666666666966666669966666699666666996666669996666699966666
-- 094:9669669966999699666996966669999966699999666999996669999966699999
-- 095:9999999999999999699999999999999999999999999999999999999999699999
-- 096:99999999999999999999999999999999999999999999999999999999c9999999
-- 097:9996666696666666966666669666666666669999969999999999999999999999
-- 098:6666666666666666666666666666666666666666999666669999996699999999
-- 099:6666666666666666666666666666666666666666666666666666666699966666
-- 100:9999999999999999999999999999999999999999999999999999999999999999
-- 101:9999999999999999999999999999999999999999999999999999999999999999
-- 102:9999996699999966999996669999966699999666999966669999666699999666
-- 103:6666666666666666666666666666666666666666666666666666666666666666
-- 104:9999999999999999999999999999999999999999ccc99999cccc9999ccccc999
-- 105:9999999999999999999999999999999999999999999999999999999999999999
-- 106:9999999999999999999999999999999999999999999999999999999999999999
-- 107:9999999999999999999999999999999999999999999999999999999999999999
-- 108:9999999999999999999999999999999999999999cc999999cccc9999ccccc999
-- 109:9999666699996666999969999999999999999999999999999999999999999999
-- 110:6669999966999999999999999999999999999999999999999999999999999999
-- 111:9999999999999999999999999999999996999999999999999999999999999999
-- 112:ccc99999ccccccccccccccc9ccc9cc99ccc99999ccc99999cccccc99ccccc999
-- 113:99999999c9999999999999999999999999999999999999999999999999999999
-- 114:9999999999999999999999999999999999999999999999999999999999999999
-- 115:9999666699999969999999999999999999999999999999999999999999999999
-- 116:99cc9999ccccc999cccccc99ccccccc9cccccccccccccccccccccccccccccccc
-- 117:9999999999999999999999999999999999999999c9999999cc999999ccc99999
-- 118:9999996699999996999999999999999999999999999999999999999999999999
-- 119:6666666666666666966666669996966999999999999999999999999999999999
-- 120:ccccc999cccccc99cccccccccccccccccccccccccccccccccccccccccccccccc
-- 121:999999999999999999999999c9999999cc999999ccc99999ccc99999cccc9999
-- 122:9999999999999999999999999999999999999999999999999999999999999999
-- 123:9999999999999999999999999999999999999999999999999999999999999999
-- 124:cccccc99ccccccc9ccccccc9ccccccccccccccccccccccccccc99999ccc99999
-- 125:9999999999999999999999999999999999999999999999999999999999999999
-- 126:9999999996999999966699999996666699999669999999999999999999999999
-- 127:9999999999999999999999999999999999999999999999999999999999999999
-- 128:9999999999999999999999999999999999999999999999999999999999999999
-- 129:9999999999999999999999969999999999999999999999999999999999999999
-- 130:9666666999666669666666699666666699996666999996669999996699999999
-- 131:9999999999999999999999999999999969999999699999996999999966999999
-- 132:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 133:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddeeddddd
-- 134:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 135:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 136:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 137:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 138:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 139:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 144:9999999999999999999999999999999999999999999999999999999999999999
-- 145:9999999999999999999999999999999999999999999999999999999999999999
-- 146:9999999999999999999999999999999999999999999999999999999999999999
-- 147:9966669999666669966666669966666696666666999666669999999699999999
-- 148:dddddddddddddddddddeeeeeeeeeeeeedeeeeeeedeeeeeeeeeeeeeeeeeeeeeee
-- 149:dddddddddeeeeeddeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
-- 150:ddddddddddddddddeedddeddeedeeeedeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
-- 151:ddddddddddddddddddddddddddddddddddddddddedddddddedddddddeedddddd
-- 152:eedddddddeeddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 153:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 154:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 155:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 160:9999999999999999999999999999999999999999999999999999999999999999
-- 161:9999999999999999999999999999999999999999999999999999999999999999
-- 162:9999999999999999999999999999999999999999999999999999999999999999
-- 163:9999999999999999999999999999999999999999999999999999999999999999
-- 164:deeeeeeeddeeeeeeddeeeeeedddeeeeedddeeeeeddddddeeddddddeeddddddde
-- 165:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddeeeeee
-- 166:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedeeeeeeeeddeee
-- 167:eeeeddddeeeeedddeeeeedddeeeeeeddeeeeedddeeeeeeddeeeeeeddeeeeeedd
-- 168:ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddde
-- 169:ddddddddddddddddddddddddddddddddddddddddddddddddddddddddeeeeeeed
-- 170:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 171:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 176:9999999999999999999999999999999999999999999999999999999999999999
-- 177:9999999999999999999999999999999999999999999999999999999999999999
-- 178:9999999999999999999999999999999999999999999999999999999999999999
-- 179:9999999999999999999999999999999999999999999999999999999999999999
-- 180:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 181:edeeeeeeeddeeeeedddeeeeedeeeeeeedeedeeeeddedeeeeddeedeeedddeddde
-- 182:eeeddeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedd
-- 183:eeeeddddeeedddddeeeeddddeeeeedddeeeeeeddeeeeeeedeeeeeeeeeeeeeeee
-- 184:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 185:eeeeeeeeeeeeeeeeeeeeeeeddeeeeeeeddeeeeeeddeeeeedddddeddddddddddd
-- 186:ddddddddddddddddddddddddeeedddddeeedddddeeeddddddeeeddddeeeeeddd
-- 187:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 192:9999999999999999999999999999999999999999999999999999999999999999
-- 193:9999999999999999999999999999999999999999999999999999999999999999
-- 194:9999999999999999999999999999999999999999999999999999999999999999
-- 195:9999999999999999999999999999999999999999999999999999999999999999
-- 196:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 197:ddddeddddddddeddddddddeeddddddeeddddddeedddddddedddddddedddddddd
-- 198:deeeeeddddeeeeeeddddeeeedddeeeeedddeeeeeeddeeeeeeeeeeeeedddeeeee
-- 199:eeeeeeeeeeeeedddeeeeedddeeeeedddeeeeedddeeeeedddeeeeeeedeeeeeeed
-- 200:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 201:dddddeeedddddeeedddddeeddddddeeddddddddddddddddddddddddddddddddd
-- 202:eeeeddddeddeeddddeeedddddeeeeeddddededdddddddddddddddddddddddddd
-- 203:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 208:9999999999999999999999999999999999999999999999999999999999999999
-- 209:9999999999999999999999999999999999999999999999999999999999999999
-- 210:9999999999999999999999999999999999999999999999999999999999999999
-- 211:9999999999999999999999999999999999999999999999999999999999999999
-- 212:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 213:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 214:ddddeeeedddddedddddddddddddddddddddddddddddddddddddddddddddddddd
-- 215:eeeeeeeeeeeeeeeedeeeeeeedeeeeeeeddeeeeeedddeeeeeddddeeeeeeeddeee
-- 216:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 217:dddddddddddddddddddddddddddddddddddddddddddddddddddddddedddeeeee
-- 218:dddddeddddddddddddddddddddddddddddddddddddddddddeeedddddeeeddddd
-- 219:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 224:9999999999999999999999999999999999999999999999999999999999999999
-- 225:9999999999999999999999999999999999999999999999999999999999999999
-- 226:9999999999999999999999999999999999999999999999999999999999999999
-- 227:9999999999999999999999999999999999999999999999999999999999999999
-- 228:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 229:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 230:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 231:eeeedeeeeeeeedeeeeeeedeedeeedddedddddddeddddeeedddeddeeedeeddddd
-- 232:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 233:ddddeeeedddddeeeddddddeedddddddddddddddddddddddddddddddddddddddd
-- 234:eeeeddddeeeeedddeeeeddeeeeeedddddddddddddddddddddddddddddddddddd
-- 235:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 240:999999999999999999cc999999ccc9999cccccc99cccccc9cccccccccccccccc
-- 241:9999999999999999999999999999999999999999999999999999999999999999
-- 242:9999999999999999999999999999999999999999999999999999999999999999
-- 243:9999999999999999999999999999999999999999999999999999999999999999
-- 244:dddddddedddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 245:edddddddeddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 246:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 247:deeddddddeeedddedeeeddddeeeeeddddeeeeddddedddeeedddddeeeddeeeeee
-- 248:ddddddddddddddddddddddddddddddddddddddddddddddddddddddddeddddddd
-- 249:ddddddddddddddddddeedeedddeeddeeddeeddeeddeeddeedeedddeedeeeeeee
-- 250:dddddddddddddddddeedddddeeddddddeeddddddedddddddeeddddddeeeeeeed
-- 251:ddddddddddddddddddddddddddddddddddddddddddeeddddddeeeededeeeeeee
-- </TILES>

-- <SPRITES>
-- 000:0000c000000c0000000c0000000c0000000c0000000c0000000c0000000c0000
-- 001:000c00000000c0000000c0000000c0000000c0000000c0000000c0000000c000
-- 002:000000000000000000000fff0000f555000ffff500fccaaf00faaaaf000ffff5
-- 003:0000000000000000f00000005f00000056f0000056ff000056f5f00056f6f000
-- 004:000000000000000000000fff0000f444000ffff400fccaaf00faaaaf000ffff4
-- 005:0000000000000000f00000004f00000043f0000043ff000043f4f00043f3f000
-- 016:000c0000000c0000000c0000000c0000000c0000000c0000000c0000000c0000
-- 017:0000c0000000c0000000c0000000c0000000c0000000c0000000c0000000c000
-- 018:000f5555000f5555000f5556000f6666000f6fff000f6f0f000fff0f00000000
-- 019:56f6f00066fff00066f0000066f0000066f0000066f00000fff0000000000000
-- 020:000f4444000f4444000f4443000f3333000f3fff000f3f0f000fff0f00000000
-- 021:43f3f00033fff00033f0000033f0000033f0000033f00000fff0000000000000
-- 032:000c0000000c0000000c0000000c0000000c0000000c0000000c00000000c000
-- 033:0000c0000000c0000000c0000000c0000000c0000000c0000000c000000c0000
-- 034:02222220222c2222222cc222222c222202222220000000000000000000000000
-- 035:9cccdccc9cccdccc9cccdccc9cccdccc9cccdccc999999990000000000000000
-- 065:0222200022000200222222002200020022000200022220000000000000000000
-- 066:0006000006666000660606006606060006666000000600000000000000000000
-- 067:000a0000aa0a0a00aa0a0a00aa0a0a000aaaa000000a00000000000000000000
-- 068:0bbbb000bb000b00bbbbbb00bb000b00bb000b000bbbb0000000000000000000
-- 069:0cccc000cc000c00cccccc00cc000c00cc000c000cccc0000000000000000000
-- 070:ccc00000000c00000cc00000c0000000cccc0000000000000000000000000000
-- 071:00000c000000cc00ccc00c0000000c000000ccc0000000000000000000000000
-- 072:0000000000222200020022000200220000222200000000000000000000000000
-- 073:0022200002200200022220000220020002200200022220000220000000000000
-- 074:000000000000000000000000000000000b000000bb0000000b000000bbb00000
-- 075:00000000000000000000000000000000bb00000000b00000bb000000bbb00000
-- 128:000000000000dd00000ddd0000dddd0000eddd00000edd000000ee0000000000
-- 129:0000000000dd000000ddd00000dddd0000ddde0000dde00000ee000000000000
-- 130:00000000000dd02000dde2000ddd2d000ed20d00002dde00020ee00000000000
-- 131:00000000000dd00000dded000ddd0d000edd0d0000edde00000ee00000000000
-- 132:00ddd0000deeed00de0d0d0dd00eddded000ede0ed000e000edddd0000eeee00
-- 133:00000000000dddd0000deed00dddd0d00deeddd00d00dee00dddd0000eeee000
-- 134:000000000dddddd00deeeed00d0dd0d00d0dd0d00d0ee0d00dddddd00eeeeee0
-- 135:00000000000d0000000d0000000d0000000dddd000deeee00de000000e000000
-- 136:00000000000d0020000d0200000d20000002ddd0002eeee002e000000e000000
-- 137:000000000dddd0000edde00000dd000000dd000000dd000000ee000000000000
-- 138:000000000d000d000d0d0d000ddddd000ddddd000ddedd000ee0ee0000000000
-- 139:000000000ddddd000eeeed000d000d000d000e000ddddd000eeeee0000000000
-- 144:00000000000000000000dd00000ddd0000dddd00000ddd000000dd0000000000
-- 145:000000000000000000dd000000ddd00000dddd0000ddd00000dd000000000000
-- 146:0000000000000000000dd02000dd02000ddd2d0000d20d00002dd00002000000
-- 147:0000000000000000000dd00000dd0d000ddd0d0000dd0d00000dd00000000000
-- 148:0000000000ddd0000d000d00d00d0d0dd000ddd0d0000d000d00000000dddd00
-- 149:0000000000000000000dddd0000d00d00dddd0d00d00ddd00d00d0000dddd000
-- 150:00000000000000000dddddd00d0000d00d0dd0d00d0dd0d00d0000d00dddddd0
-- 151:0000000000000000000d0000000d0000000d0000000dddd000d000000d000000
-- 152:0000000000000000000d0020000d0200000d20000002ddd00020000002000000
-- 153:00000000000000000dddd00000dd000000dd000000dd000000dd000000000000
-- 154:00000000000000000d000d000d0d0d000ddddd000ddddd000dd0dd0000000000
-- 155:00000000000000000ddddd0000000d000d000d000d0000000ddddd0000000000
-- 176:000000cc000000000002200c000000c0000220cc000220000002200000000000
-- 177:c00000000c000000c00000000000000ccc0000000000000c0000000000000000
-- 178:000000000000000000000000ccc0000000000000ccc000000000000000000000
-- 179:000000000000000000000cc00000ccc0ccc00cc000000cc00000cccc00000000
-- 192:000000cc000000000002200c000000c0000220cc000220000002200000000000
-- 193:c00000000c000000c00000000000000ccc0000000000000c0000000000000000
-- 194:000000000000000000000000ccc0000000000000ccc000000000000000000000
-- 195:0000000c00000000000660000000000c0006600c000660006006600006660000
-- 196:cc00000000c00000cc00000000000000ccc00000000000000000000000000000
-- 197:000000000000000000000000cccc000000000000cccc00000000000000000000
-- 198:0000000c000000000aa000000aa00a0c0aaaa00c0aa00a000aa00a0000000000
-- 199:cc00000000c00000cc00000000000000ccc00000000000000000000000000000
-- 200:000000000000000000000000cccc000000000000cccc00000000000000000000
-- 201:0000000000000000022000060000000002200006022000060220600600000666
-- 202:000000000000000060aa000000aa00a060aaaa0060aa00a060aa00a000000000
-- 203:0000000000000000000000000000cccc000000000000cccc0000000000000000
-- 204:0000000000000000000000000000000000000ccc000000000000000000000000
-- 205:000000000000000000cc00000ccc000000cc000000cc00000cccc00000000000
-- 208:00000000000000000000c000000c00cc000c000c000c000c0000c0cc00000000
-- 209:0000000000000000000022000cc00000cc002200cc0022000cc0220000000000
-- 210:000000000000000000000000000c000000ccc000000c00000000000000000000
-- 211:0000000000000000000000000c00cc000c00cc0000cccc000000cc0600ccc000
-- 212:0000000000000000006600000000000000660000006600000066000066600000
-- 213:0000000000000000000000000c00000cccc000000c0000000000000c00000000
-- 214:000000000000000000000aa0cccc0aa00cc00aaacc000aa0cccc0aa000000000
-- 215:0000000000000000000c00000a00c000a000c0000a00c0000a0c000000000000
-- 224:00bbbb000bb000b00bbbbbb00bb000b00bb000b000bbbb00000000000cccccc0
-- 227:00000000000000000000000000cccc000c00cc000c00cc0000cccc0000000000
-- 228:000000000000000000000000000c000000ccc000000c00000000000000000000
-- 229:00000000000000000cc000020cccc0000cc00c020cc00c020cccc00200000000
-- 230:0000000000000000200000000000000020000000200000002000000000000000
-- 240:0000000000cccc0000000cc0000ccc0000cc000000ccccc00000000000000000
-- 243:00000000000000000000000000cccc000c00cc000c00cc0000cccc0000000000
-- 244:000000000000000000000000000c000000ccc000000c00000000000000000000
-- 245:00000000000000000cc000020cccc0000cc00c020cc00c020cccc00200000000
-- 246:000000000000000020000000000000c020000ccc200000c02000000000000000
-- 247:00000000000000000000000000000ccc0000ccc00000ccc000000ccc00000000
-- 248:000000000000000000000660c00000000000066000000660c060066000066600
-- 249:0000000000000000000000000000c000000ccc000000c0000000000000000000
-- 250:000000000000000000000cc0000cccc000c00cc000c00cc0000cccc000000000
-- 251:0000000000000000aa000000aa00a000aaaa0000aa00a000aa00a00000000000
-- </SPRITES>

-- <MAP>
-- 068:112111211121212121121112112112112112121111111111112111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111111112111211111111111111111211211111211112111112112111111111111111111121111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 069:111111111121112111121121212111111111111111111121111211121111111111111111111111111111121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211212111111111111211112121111111111111111111121121121121111111111111111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 070:111111121211111111122121121211111111111111211211111112112111111111111111111111111111111111111111111111111111111111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111111111111111111111111112121212111212111111112111111211111111121211111121211211211211121112112112112112111121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 071:111111211211111111211111211212111121111112111121111111211111111111111111111111111111111111111111112111111111111111121112111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112121212121212121111111111121111111111111121112121211112111111211111111111211111112111111111111111111111111111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 072:111111121111111121112121112111212111111212121211212112111111111211121211111111111111111211111111211111111111111111111121111211111121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111212112111211211211221111121211111111111111111121212121121111111111112111211111111111111111111111111111111111111111111112111121111111111111111111111111111111111111111111111121111111111111111111111111111111111111111111111111111111111
-- 073:112111111111121121121121121111212111112121111112111212111111111111121111211211111111111111111111111111111111111111111211111112111111111111111111111111111111111111111111111111111111111121111111111111111111111111111111111111111111111111111111111111112112111211212111212121111121211111212121121111121211212112111211111111111111121111111211111111111111211111111111121111121112111121112111111111111111111111111111111111111111111111121111111111111111111111111111111111111111111111111111
-- 074:111111211111112111111121111111212111111112111111111111111121111111111112111111111111111111111111111211112111111111111111111111111111112111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111111121211121212112212121121112121211111211111212121111121211211111112112111111111111111111111111111111111111112111111111111121111111121111111111111111111111111111111111111111111112111111111111111111111111111111111111111111111111111112
-- 075:111112111111111211111211121112111211111112121112121111111211111112112112111111111111111111111111111111111112111111111111111111121111121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111111121211121111211112112111121112112121111111111111211112121111111111111121111111111111111111111121111111111121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 076:112112111112111121121211211212111211211111212111121211111211111212121111111111111111111111111111111111112111111111111111111111111111211121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121211111111112111112111111121112111121121111211111121111111111111211111111121111211111111111111111111111111111111111111111111111111111111111111111111121111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111112
-- 077:112111211111111112121111211121211211121121211111211111211121112112111111111111111111111111111111111111111111111111112111111111112111112111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111221121111111111111212111111111121111211111112112111121211111121111111111111111111111111111111111111111111111111111111111111111111211111111111111111121111111111111111111111111111111111111111111111111211111111111111111111111111111111111112122112111211
-- 078:111121121111112111212111121112121211211121111112111111111111211121112111121111111111111111111111111111111111111121111111111111111111211121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111212121121111112111111111211121111112111211111111112112121211112111112111111111111111111211111111111111111111111111111111111111121111111111112111111111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111222121212111111
-- 079:111112111211111111121111111111211121211121111111111112111111112111111111111111111111111111111111121112112111111111111111111111111111112111111111111111111111111111111111111111111111111111111111111111111111111111111111111121222122212111111121111111121111121111111211121112112111112111111212111111111111111111111111111111111111111111111122233333322221211111111111111111111111111111121111111111111111111111111111111111111111111111111111111111111111111111122122222212122221221121121111
-- 080:112111111111111121111112111211111121211212121111121121111111111111111111111111111111111111222333334343443332212112111111111111111111111121111111121111111111111111111111111111111111111111111111111111111111111111111211212222212121111211111111111112111211111111121111111112121111111111111121111111111111111111111111111111111111111112233344443444344434332211121111111111111111111111111121111111111111111111111111111111111111111111111111111111111111111111112112121211211111211111211121
-- 081:121111111111111211111121121111111111212121212111111111111111111111111111111111111111111223343434444444444444443322111111111111111111121121121111112111111111111111111111111111111111111111111111111111111111111111111111211121112111111111111111111111121111111111111111111211211211111111111112121111111111111111111111111111111111122333434444444444444444344433222111111111111111111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 082:111121111121111111211112121111111111111212121211111111111111111111111111111111111112223334344444444444444444444444332121111111111111111211211112121121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111211111112111111211111111111211111111111111111111111111111111112233344444444444444444444444444443221211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 083:121111121111111111111111111111112111112112121211111111111111111111111111111111111122334434444444444444444444444444444322111111111111111111112121111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111211111111111121211111111111111111111112111111111111111111111111111111111222333444444444444444444444444444444433221111111111111112111111211111121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 084:111111111111111111212111111211211111211211211211111111111111111111111111111111111223344344444444444444444444444444444443312111111111111111121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121121121111111111111111111111111121111111121111111111111111111111111111111111112233434444444444444444444444444444444444322111111111111111111112111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 085:111111111112111112111211111111111111111121111111111111111111111111111111111111122333344444444444444444444444444444444444432211111111111111211111112111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111121111111111111111211121112111121111112111111111111111111111111111111112222334443444444444444444444444444444444444432211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 086:111121111121112111121121112111111111111112111111111111111111111111111111111112223234344444444444444444444444444444444444443221111111111111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111211111111112111111111111211121111211111111111111111111111111111111111222222333443444444444444444444444444444444444443222111111111111111111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 087:111111112111111111111211211111111111111111111111111111111111111111111111111122322344444444444444444444444444444444444444443322111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111111121211121111112112111211111111111211111111111111111111111111111222222334344444444444444444444444444444444444443332111111111111111111111112111111111111111111111111111111111111111111111111111111111111111111111111111111111111112212
-- 088:111112111121111112111111111111111111121121111111111111111111111111111111111122223344444444444444444444444444444444444444443322111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211121111111111111211121111111111111111111111111111111111111111111111112222323343443444444444444444444444444444444444444322211111111111111111111111111121112111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 089:111111111111212111111112111111121112111211111111111111111111111111111111111212232344444444444444444444444444444444444444444322112111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111211211211111121212111111121111111111111111111111111111111111111111111111222223344344444444444444444444444444444444444444322211111111111111111111112111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 090:111111111111121111111111112111112121121111211112111111111111111111111111111222233333444344444444444444444444444444444444443322121111111111111111111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121212111121112121211111111111111111111111111211111111111111111111111212233344434443444444444444444444444444444444444322211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 091:111212111111112111211111111112111111211112111111111111111111111111111111111212233343344344434444444444444444444444444444444321121111111111111111111111211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111211111121112112111112111111111111111111111111111111111111111111122323334434443444444444444444444444444444444443322111111111111111111111111111121112112111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 092:112111211121112121111111111111111112111111111112111211111111111111111111112222333333343344344344444444444444444444444444444322121211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121112111211112121212121111211111112112111111111111111111111111111111221222223334344434434444444444444444444444444444443322221111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 093:111111211121121211111111111121111111111111111111111111111111111111111111122223333333333343443444344344444444444444444433333332222221111111111111111111111111112112111111211111111111111111111111111111111111111111111111111111111111111111111111112111112112111111212121211111112112111111111111111111111111111111111112122322232333434434344333333333343444444444333333333322222211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 094:111121211211212121111111111211111111112111111111112111111111111111111111222332333333333343433333323232223333333322223233333322222222111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111111111111112121211121112111111111111111111111111111111111111111112223223333333433433433332222222222222333222223333333222222111111111111111111111111111111111111121111111111111111111111111111111111111111111111111111111111111111111111111
-- 095:111112121121212111111112111111111211211112111111111111111111111111111112222333333343343343333323333222222233332222222333333222222221111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111211111211211211111211121111111111111111111111111111111111111122222233333344433433333233332222222333333222222222222222222112111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 096:111111211112121121121112121111111111111121111111111111111111111111111122223333334333344333223333222221222333333222222222223222222211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121111211112112112111111212112111211111111111111111111111111111111122223233333434433433232222222122222334443223322222333322222121111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 097:111111121112211111121212121111111111111111111111111111111111111111111122222323334343334333322223323333322333433333333333333323222111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211112212111111111121121111111111111111111111111111111111111112222233343433433433322233333333333334444333333333433322222111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 098:111111111111211121212121212111111111111111111111111111111111111111111112222323343433343333333333333333333344343333333333343333221111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111212121112111111111111112111111111121111111111111111111111111111111222323344343333434333333333333444433444444334344433322221111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 099:111111111111211121212121121111112111111111111111111111111111111111111111122323333334333343343434343434443444434333444434333332211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111111111111211111111111111111111111111111111111111111111222322344333333334434343434344344343444444434343333322211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 100:111112112121111121221212111112111111111111111111111111111111111111111111112232333333323333343444444444444444344333344434333322111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111212111111111121111121111111111111111111111111111111111111111122233333332233333334334343434333334443434343443333322111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 101:111111111111111212121112111211111111111111111111111111111111111111111111112223333322323233333344444443344443444433344434333222111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211121121211111111211211111111111111111111111111111111111111111111111112223233332222333333434344343344344434433333433333221111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 102:111111111111121212121111112111211111111111111111111111111111111111111111111223223323223233333344343433343433444433333443333211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111111111121111111111111111111111111111111111111111111111111111122222332322333333434434433333333333433333334332211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 103:112111112111121212121111211121111121111111111111111111111111111111111111111111222233222233333343443333333222333333333333332111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121111111211111111111111111111111111111111111111111111112122223322233333343333344332222222233443333332111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 104:121111211121112121212111121111111211111111111111111111111111111111111111111111122223222233333333333434433332223233433433321111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111111112111211111111111111111111111111111111111111111111111111232222322233333333333433344333323334343333322111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 105:111111112111111211121211111112111111111111111111111111111111111111111111111111234222212223333333333343434343333333433333321111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121211111112111111111111121111111111111111111111111111111111111111111111111111234432222223333333333333333433333333333333321111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 106:111112111111112112111112111121111111111111111111111111111111111111111111111111243333322232333333333333333333333232222333322111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121111111111111111111111111111111111111111111111111111111111244443332223233333333222232222322222323332222111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 107:121211111211111121111211211211111211111111111111111111111111111111111111111111133334434332323333332223333332333233232333322111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111111111111111211111121111111111111111111111111111111111111111134443343433332333333333333323222323333322321111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 108:212112111111111112112112111111111111111111111111111111111111111111111111111111123334434344233333333333333323332233332333321111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111111111111111112111111111211111111111111111111111111111111111111111113343444444323333333333333332333333333323211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 109:121211111111111121111112111121112111111111111111111111111111111111111111111111112343434433333233333333333333333333333232211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111111111111112111111111211111111111111111111111111111111111111111111111111234343444223333333333433343333343332322111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 110:121211111111121111111111121111111112111111111111111111111111111111111111111111111223344343323233333334344434443333332221111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112112121211111111111121111211112111111111111111111111111111111111111111111111111223433443332333333333333343334333322211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 111:211211111111111121112111111111111111111111111111111111111111111111111111111111111112334333232332333333334334333332223111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121121111111111111111121121112111121111111111111111111111111111111111111111111111122234443222233233233333333333322232211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 112:112112121211112111111111111111111111111211111111111111111111111111111111111111111112233334322222323333333333333223334311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121211111111111111121112111121112111111111111111111111111111111111111111111111111112334444443222223332333333323233344431111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 113:212121212111111111111111111111111111111111111111111111111111111111111111111111111112344444344332222233233232223334444443111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111211111111112111111111111111111111111111111111111111111111111111111112344444444443433322322222233344444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 114:121121211211111111111112111111111111111111111111111111111111111211111111111111111112344444444444443343233333334444444444311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111212112121121211211112111111111111111111111111111111111111111112111111111111111111112444444444444444444433434444444444444421111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 115:111211112111111111111111111111111111111111111111111111111111111111111111111111111111444444444444444444444444444444444444421111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121111211211212111111111111111111111111111111111111111111111112111111111111111111112344444444444444444444444444444444444431111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 116:112122121121111111211111111111111111111111111111111111111111111112111111111111111111244444444444444444444444444444444444431111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111112112121111111111111111111111111111111111111111111111211211111111111111111111223444444444444444444444444444444444431111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 117:121212211211211211111111111111111111111111111111111111111111121111111111111111111111134444444444444444444444444444444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121121111112111111111111111111111111111111111111111111111212121111111111111111134444444444444444444444444444444444444422111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 118:121212112112121111111111111111111111111111111111111111111111212111111111111111111111134444444444444444444444444444444444444444432111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111212121212121212211111111111111111111111111111111111111111111111121121111111111111111144444444444444444444444444444444444444444444321111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 119:111111111111111121211111111111111111111111111111111111111111121112111111111111111111144444444444444444444444443343444444444444444443111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121211212121212111111111111111111111111111111111111111111111111111121121111111111111133444444444444444444444444434444444444444444444211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 120:111121121112121111111111111111111111111111111111111111111111211111111111111111111111112244444444444444444444444443434444444444444444311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121211111211111211211111111111111111111111111111111111111111111212121211111111111111111144444444444444444444444444444344444444444432211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 121:211112212112121111111111111111111111111111111111111111111111111111211111111111111111111134444444444344444444444444444434343322223322111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121211111111111212111111111111111111111111111111111111111112121121121211111111111111111124444444444444444444444444444444444421111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 122:112112121212121111111111111111111111111111111111111111111111112211222111111111111111111113444444444443444444444444444444443331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211211211121112121211111111111111111111111111111111111111121111122111111111111111111111113444444444444444444444444444444444431111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 123:121121112111211111111211111111111111111111111111111111111111212111221211111111111111111111444444444444444444444444444444444331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111211121111112111111111111111111111111111111111111111211111222212111111111111111111111344444444444444444444444444444444432111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 124:121221212112121211112111111111111111111111111111111111111112121111221212111111111111111111244444444444444444444444444444444432111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111111211111111211111111111111111111111111111111111111111112121212121111111111111111111234444444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 125:122121212112121121111121111111111111111111111111121111111112221122121212111111111111111111134444444444444444444444444444444432111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111212111121111111121112111111111111111111111111211111111121112111222112111111111111111111123444444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 126:121111121112121111211111111111111111111111111112111111111112121212111211111111111111111111123444444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111221212111111121111121112121111111111111111121111111111111212121212121211111111111111111123444444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 127:121112111212121111112111211111111111111111111111111111111121121111211211111111111111111111112444444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112121121111111111111121111211111111111111111211111111111112112121122121211111111111111111112344444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 128:121112111212112112111111211112111111111111111111111111111111211212111211121111111111111111112244444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211121121111111111111121211111111111111111112111111111111121111121121111111111111111111111244444444444444444444444444444442211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 129:121112112111121111121212111121121111111111111111111111111111111212112112111111111111111111111234444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211211112111111111111121212112111111111111121111111111112121111112221111111111111111111111134444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 130:111111121211121111111112112121211111211111111111111111111111212121211112111111111111111111111134444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111121212112112111111111111121122122121111111111111111111111111111111121222211111111111111111111123444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 131:111111211211111111111111212211212112111111111111111111111111121212112111111111111111111111111113444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112121121121112111111111121222121221111111111111111111111111111111221122211111111111111111111113444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 132:111111212111211111111112112112222112111111111111111111111111212121112211121111111111111111111112444444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211111211111111111111222221211221211111111111111111111111111112121122211111111111111111111112344444444444444444444444444442111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 133:112112121111112111111111112122212212111111111111111111111111112121112211121111111111111111111111244444444444444444444444444432111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112112111111111111211221122121111111111111111111111111111111211212211111111111111111111111234444444444444444444444444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111211
-- 134:111211211111111111111111122122212212211111111111111111111111111121112122111111111111111111111111134444444444444444444444444432111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112111121111111111111111212122121111111111111111111111111112111212212121111111111111111111111114444444444444444444444444422111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- 135:111211111112112111111111112121122222211111111111111111111111111211111121211111111111111111111111112444444444444444444444444421111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111112121111111111111111111112121111111111111111111111111111111111212112111111111111111111111111112443444444444444444444444321111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
-- </MAP>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <SFX>
-- 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
-- </SFX>

-- <PATTERNS>
-- 000:b22106b00006b00006b00006800006800006800006800006800006800006800006800006600006600006600006600006600006600006600006600006b00006b00006b00006b00006b00006b00006b00006b00006900006900006800006800006800006800006800006800006800006800006b00016b00016b00016b00016f00016f00016f00016f00016f00016f00016f00016f00016d00016d00016d00016d00016b00016b00016b00016b00016900016900016900016900016100010100010
-- 001:122100100000100000100000b00004b00004b00004b00004900004900004900004900004400004400004400004400004b00004b00004b00004b00004b00004b00004b00004b00004b00012b00012b00012b00012b00012b00012b00012b00012100000100000100000100000100000100000b00004b00004b00004b00004b00014b00014b00014b00014b00014b00014b00014b00014900014900014900014900014400014400014600014600014600014600014600014600014600014600014
-- 002:822116800016800016800016800006800006800006800006600006600006600006600006b00006b00006b00006b00006400006400006400006400006400006400006400006400006f00004f00004f00004f00004f00004f00004f00004f00004b00004b00004d00004d00004d00004d00004f00004f00004f00004f00004400006400006400006400006400006400006400006400006600006600006600006600006800006800006d00006d00006d00006d00006b00006b00006b00006b00006
-- 003:622114600014600014600014100000100000100000100000b00014b00014b00014b00014900014900014900014900014900014900014900014900014400014400014400014400014400014400014400014400014d00014d00014b00014b00014b00014b00014b00014b00014b00014b00014400014400014400014400014b00014b00014b00014b00014b00014b00014b00014b00014800014800014800014800014400014400014400014400014d00014d00014b00014b00014b00014b00014
-- 004:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000040002a50002a60002a400028f00018400018500018400018100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 005:822116800016800016800016100000100000800016800016800016800016100000100000b00016b00016b00016b00016100000100000b00016b00016b00016b00016100000100000800016800016800016800016100000100000800016800016800016800016100000100000b00016b00016b00016b00016100000100000b00016b00016b00016b00016100000100000800016b00016900016800016600016800016600016400016600016800016100000100000800016800016800016800016
-- 006:b22114b00014b00014b00014b00014b00014b00014b00014b00014b00014b00014b00014400014400014400014400014400014400014400014400014400014400014400014400014b00014b00014b00014b00014b00014b00014b00014b00014b00014b00014b00014b00014400014400014400014400014400014400014400014400014400014400014400014400014400014400014400014400014400014400014400014400014400014400014b00014b00014b00014b00014b00014b00014
-- 007:122100100000800006800006800006800006100000100000800016800016800016800016800016800016800016800016b00016b00016b00016b00016d00016d00016d00016d00016800016800016800016800016600016600016600016600016100000100000f00016f00016f00016f00016100000100000f00016f00016f00016f00016100000100000600018600018600018600018100000100000600018600018600018600018100000100000f00016f00016f00016f00016100000100000
-- 008:b22124b00024b00014b00014b00014b00014b00014b00014d00014d00014d00014d00014d00014d00014d00014d00014400014400014400014400014600014600014600014600014d00014d00014d00014d00014b00014b00014b00014b00014b00014b00014600024600024600024600024600024600024600024600024600024600024000000000000b00024b00024b00024b00024b00024b00024b00024b00024b00024b00024b00014b00014600024600024600024600024600024600024
-- 009:122100100000100000100000100000100000100000100000100010100010100010100010100010100010100010100010800014800014800014800014a00014a00014a00014a00014400014400014400014400014f00014f00014f00014f00014100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000
-- 010:f22116f00016f00016f00016100000100000600018600018600018600018100000100000600018600018600018600018100000100000f00016600018400018f00016d00016b00016d00016f00016f00016f00016f00016f00016400018400018400018400018400018400018f00016f00016d00016d00016f00016f00016f00016f00016f00016f00016f00016f00016f00016f00016100010100010800016800016800016800016800016800016800016800016800016800016800016800016
-- 011:622124600024600024600024600024600024b00026b00026b00026b00026b00026b00026b00026b00026b00026b00026b00026b00026b00024b00024b00024b00024b00024b00024b00024b00024b00024b00024d00014d00014d00014d00014d00014d00014d00014d00014b00014b00014b00014b00014100010100010100010100010100010100010100010100010100020100020100020100020b00024b00024b00024b00024b00024b00024b00024b00024b00024b00024b00024b00024
-- 012:122100100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000100000600014600014600014600014600014600014600014600014600014600014600014600014400024400024400024400024400024400024400024400024000000000000000000000000
-- 013:122100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </PATTERNS>

-- <TRACKS>
-- 000:380e001010006c1000842a00b03d00000000000000000000000000000000000000000000000000000000000000000000ab0000
-- 001:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d0ff
-- </TRACKS>

-- <PALETTE>
-- 000:1a1c2c5d275dd63c44ef7d57ffcd75a7f07038b76425717929366f3b5dc979d2f6c285fff4f4f494b0c2566c86333c57
-- </PALETTE>

-- <COVER>
-- 000:569000007494648393160f00880077000012ffb0e45445353414055423e2033010000000129f40402000ff00c2000000000f00880078a1c1c24f4f4fb3d59c490b2c837b4633c37565c66800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000080ff001080c1840b0a1c388031a2c58c0b1a3c780132a4c9841b2a5cb88133a6cd8c1b3a7cf80234a8c1942b4a9c398235aac59c2b5abc790336acc9943b6adcb98337aecd9c3b7afcf90438a0d1a44b8a1d3a8439a2d5ac4146086a053aa5c7a25baa5dba853ba6ddac5bba7dfa063ca8d1b46bca9d3b869825d4bc6bdabd7b077bead8b47beaddbb873fae54a377fafdfb0830bf4104850fe52c88b0b16bb30a1f084cf8713c52098b2316acd1bfa4e1a69b2310ac2080c663b0af335e3d98f3f561d3a3ca9e7d0b363ce6dd556bdebd6a96765cdcbf6be69adb38bd66910a8f343ea9b1a1ffb2cb9f0749bcf9647185c19757aa5d9a767007dd0e6feead7979ffd5decbc7e6393d3db6f5fbeda787cecd5a3cf4f2e9c67e0d7b343e73f7e742d7aecd1b72ed671f114ffdf710687461c6d95c6bfd96cfd7652118416588d54737128fddc740285f0a967f126012e6f12f68edf6515f6f063811e37826d44e5b823af612168acd140f9249c947506159cd3413ec824ec6221f8916e89d168912b760e2964639d3e0871a1552e09e02f754208b6d6990e6984608f4a69906c716649d52f82661555a5902698b6eb6ef18930179f56d9272d936a8974ed927239c9db9ec10aba956326c6b0df8acd4937a18c8258d56a9c763ab7649636f8c995914238aa186e99c65612aa410a18a8afa53852eb82319adc9aac9ed7442aae21bffac66baea2c90b6cac666aa968822ada7b2b42baa8b6ae7cb6a6ebefa9250b1cd0b5ca1b0222bb292b5e53b5baa6204660aed82994b2c5a64da3b0a56676a0835a294eee53beea6de7bbf1268d208a7ec7d7acb9deb51427b2c2bbbee5bc1179c4ab73fa08e755b3b2bbdeabbdfe2a37ac7b07f93e6dbe61fa9ba1c70b260de9421b094c25c4b6d897e17017c55ca6c413fae179ba82a977a2a69ab54b21a2369c62b354f2f9a2b4963b8c917482371b72b9916e08b8e39d12b5f23cc3de1dd332cf11a98c54a36a795668c0692d84f2bb4bfc1315d0fadb4de8a95fcc8117d0be7b3d68d12e5c5295d753f686b8da6f7ca19ebbaebd7f5cd3291c57bdd691effd229edb73fdc4bfdd110edda0e5916ba919d0f25d3d57d4113c50979ad6846836d88f0ea15ac5920c50a3ac2b990525ad0fac39f9bd53ec46efd79bbd22e6c86b742fe3aa2f7ec86ec4973e8b7f5f479b9a70d09a8e0259c1879af6bce966ac347e9c1a4e5a7dcaafac44bee0c7bce43bedabc6b1b387c7fbdbb2cf4b2869e5fffe58e43acd4229dbb5729f68b0bfbf19cc4228bbf68d48b742fbd6c26df6ff9eb934ebab69d44ae3ed1206246741f27e1c4887afbe41ffaf7d4bafc7fa67f830d9fb4d3d837e1bf657ae1ed1d4836d0ce5d303b6952785292a6bcbfe42fa164ebd3d29cb3a021a9d48e6ad311e8079652cb914d0f435b74dcbc38a7abd9a0ff516e0b91e7da68ecacc5c9056de3185119252f9b02b03c74441f189cc793932a1fa35993e7d05ae5c83ba89dad4d5cebddf03656480e91d42964c1ea1395d64a8db1552174a6dc136557ceccd1362974b8de19f817cf3ed4ab8284294817024892e8a13e6784e3e5741998cae856200e7c24af4e1989cc4632d29031d4262939c71f4e62149539f42921a260c35aa2759cac65ab2f590bc85ac27694bca5ad2f698bcc5ae29633ac594f27790cc06a0378977cf5213f898cc46aff499ccc66af5a000dc86a437a94dca6a53fa98dcc6a637b9cdce6a73fb90ec07a837c98dc6c410cc95d447e93700cd467243dd99d457e43d130fc276d3bd97f4a7643d13fffe4e76f3fe94ec96a1060a1dc18a100a960528624b0a605d6ae37e95fc782445a92fc57af35f94f467a4452afec37a74d2abf447a849a9225f72847d9415e7e64d4a31537ac3d5aa1508604b00405682d4f0a53548e1496a73dd9a63d00d81f96f318a12d186e3f5a845596f379ac4d89ac433a8258964595a12d6ae4515ad1dbaa74dd951d39a25dbae2d9925371ad35e9295b6ab3d68e63b2af55ca64495ab1ddae74b4a55df8e9434aa2557a9474a315caef3bea37d896749ba74d49e06fda9551bad4dca96db9a439caa351ae53b2a045d82e570024daceb513bbfceca541e997d3c6755aa77db827512a4a5cbaa6f3be6df7e15d4b4858da267ca9355bf26e4d1bc0dbdad37ca14d2a215f1b8459cec678b4cd2ea17f8b105ddae4f1bd851866658b8cd8ea477ab4ddaea8530200b3
-- </COVER>

