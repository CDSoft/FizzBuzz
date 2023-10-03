local w, h = 2048, 2048

img {
    width = w,
    height = h,
    font_size=h/8, text_anchor="middle",
}

local border_size = 64

img:Rect {
    x = border_size/2,
    y = border_size/2,
    width = w-2*border_size/2,
    height = h-2*border_size/2,
    rx = 2*border_size,
    stroke = "#435488", stroke_width = border_size,
    fill = "grey",
}

img:Text "Fizz Buzz" {
    x = w/2,
    y = h/4,
}
img:Rect {
    x = w/4-64, width = w/2+2*64,
    y = h/4-100, height = 64,
    rx = 32,
    fill = "red",
    opacity = 0.7,
}
img:Rect {
    x = 128, width = w-2*128,
    y = h/4+64, height = 64,
    rx = 32,
    fill = "#435488",
    opacity = 0.7,
}

local function ix(i) return w/4 + (256+64)*(i-1) end
local function iy(i) return 896 + 256*(i-1) end

local names = string.words "LuaX ypp Panda Pandoc ..."

names:mapi(function(i, name)
    img:Text(name) { x = ix(i), y = iy(i) } { fill = "white" }
end)

img:Text "bang" {transform=("translate(%d, %d) rotate(45)"):format(ix(1), iy(3.5))} { fill = "white" }
