library(grid)
my_circle <- circleGrob(x = 0.5, y = 0.5, r = 0.5,
                        gp = gpar(col = "gray", lty = 3))
grid.draw(my_circle)

my_circle <- circleGrob(name = "my_circle",
                        x = 0.5, y = 0.5, r = 0.5,
                        gp = gpar(col = "gray", lty = 3))
grid.draw(my_circle)

my_rect <- rectGrob(x = 0.5, y = 0.5, width = 0.8, height = 0.3)
grid.draw(my_rect)

grid.edit("my_circle", gp = gpar(col = "red", lty = 1))

candy <- circleGrob(r = 0.1, x = 0.5, y = 0.6)
stick <- segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 0.5)
lollipop <- gTree(children = gList(candy, stick))
grid.draw(lollipop)

grid.ls(lollipop)
