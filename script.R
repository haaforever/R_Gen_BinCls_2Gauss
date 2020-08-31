

dt <- fn.gen.bincls.2gauss(n.neg = 1000, n.pos = 100, mu = 0.5, d = 5)
table(dt$"y")
library("rgl")
plot(dt[,1:5], col = dt$"y" + 1)
plot3d(dt[,1:3], col = dt$"y" + 1)


dt <- fn.gen.bincls.2gauss(n.neg = 1000, n.pos = 500, mu = 0.75, d = 2)
table(dt$"y")
plot(dt[,1:2], col = dt$"y" + 1)

