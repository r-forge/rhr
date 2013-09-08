library(grid)

pdf("ttsi.pdf")
# ---------------------------------------------------------------------------- #
# set up
y <- c(3, 4, 1, 5, 6, 7, 6, 5, 2, 5)
x <- c(1, 2, 3, 4, 5, 6, 7, 7.5, 9, 10)

pushViewport(viewport(width=0.95, height=0.95))
grid.rect()

# ---------------------------------------------------------------------------- # 
# msr
pushViewport(viewport(width=1, height=0.5, x=0, y=0.5, just=c("left", "bottom")))

# plotting region
pushViewport(viewport(width=1, height=1, x=0.0, y=0.0, just=c("left", "bottom"), xscale=c(0,11), yscale=c(-3, 10)))

grid.text("(a)", 0.05, 0.9)
grid.lines(x,y, default.units="native", gp=gpar(col="gray80"))
for (i in seq(x)) grid.lines(c(mean(x), x[i]), c(mean(y), y[i]), default.units="native", gp=gpar(col="red"))
grid.points(x,y, gp=gpar(col="grey50"), pch=19)
grid.points(mean(x), mean(y), gp=gpar(col="black"), pch=19, default.units="native")
grid.text(1:10, x, unit(y, "native") + unit(7, "points"), default.units="native", gp=gpar(fontsize=10, col="gray80"), just="bottom")
grid.text("centroid", mean(x), unit(mean(y), "native") - unit(1, "lines"), gp=gpar(col="red", fontsize=10, alpha=.80), default.units="native")

popViewport(2)

# ---------------------------------------------------------------------------- #
# LI
pushViewport(viewport(width=1, height=0.5, x=0, y=0.0, just=c("left", "bottom")))

# plotting region
pushViewport(viewport(width=1, height=0.5, x=0, y=0.5, just=c("left", "bottom"), xscale=c(0,11), yscale=c(-3, 10)))

grid.text("(b)", 0.05, 0.9)
grid.lines(x,y, default.units="native", gp=gpar(col="gray80"))
grid.lines(x[c(1,10)], y[c(1, 10)], default.units="native", gp=gpar(col="red"))
grid.points(x,y, gp=gpar(col="grey50"), pch=19)
grid.text(1:10, x, unit(y, "native") + unit(7, "points"), default.units="native", gp=gpar(fontsize=10, col="gray80"), just="bottom")

popViewport(1)

# plotting region
pushViewport(viewport(width=1, height=0.5, x=0, y=0.0, just=c("left", "bottom"), xscale=c(0,11), yscale=c(-3, 10)))

grid.text("(c)", 0.05, 0.9)
grid.lines(x,y, default.units="native", gp=gpar(col="red"))
grid.points(x,y, gp=gpar(col="grey50"), pch=19)
grid.text(1:10, x, unit(y, "native") + unit(7, "points"), default.units="native", gp=gpar(fontsize=10, col="gray80"), just="bottom")

popViewport(1)
dev.off()



