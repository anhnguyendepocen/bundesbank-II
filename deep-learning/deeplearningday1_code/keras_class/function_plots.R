# sigmoid
# Predefined Cardiff colours
# primary
cardiffred <- rgb(211,55,74, maxColorValue = 255)
cardiffblack <- rgb(35,21,32, maxColorValue = 255)
cardiffgrey <- rgb(47,68,78, maxColorValue = 255)
cardiffgold <- rgb(189,158,94, maxColorValue = 255)
# secondary 
cardiffblue <- rgb(21,44,81, maxColorValue = 255)
cardiffpurple1 <- rgb(29,15,51, maxColorValue = 255)
cardiffpurple2 <- rgb(60,44,89, maxColorValue = 255)


pdf("../../slides/graphs/sigmoid.pdf")
sigmoid = function(x){1/(1+exp(-1*x))}
curve(sigmoid, -10,10, las=1, lwd = 3, col = cardiffblue, bty="n")
abline(v=0)
dev.off()



pdf("../../slides/graphs/tanh.pdf")
tanh = function(x){(exp(x)-exp(-1*x))/(exp(x)+exp(-1*x))}
curve(tanh, -10,10, las=1, lwd = 3, col = cardiffred, bty="n")
abline(v=0, h=0)
dev.off()





pdf("../../slides/graphs/relu.pdf")
relu = function(x){max(0,x)}
plot(Vectorize(relu), -10,10, las=1, lwd = 3, col = cardiffgrey, bty="n")
abline(v=0)
dev.off()



pdf("../../slides/graphs/leakyrelu.pdf")
leakyrelu = function(x){max(.1*x,x)}
plot(Vectorize(leakyrelu), -10,10, las=1, lwd = 3, col = cardiffgold, bty="n")
abline(v=0, h=0)
dev.off()



# Overfitting Plots -------------------------------------------------------

x1<-runif(20, 0, 10)
y1<-runif(20, 0, 7)
x2<-runif(20, 0, 7)
y2<-runif(20, 7, 10)
x3<-runif(20, 5, 10)
y3<-runif(20, 5, 10)


# make data set 


pdf("../../slides/graphs/dataunderfit.pdf")
plot(x1, y1, xlim = c(0,10), ylim = c(0,10), pch = 16, 
     col = cardiffblue, bty = 'n', cex = 2)
abline(a=2, b=.7, col = cardiffgold, lwd = 3)
points(x1,y1, col = cardiffblue, pch = 16, cex = 2)
points(x2,y2, col = cardiffblue, pch = 16, cex = 2)
points(x3,y3, col = cardiffred, pch = 16, cex = 2)
dev.off()

pdf("../../slides/graphs/datawofit.pdf")
plot(x1, y1, xlim = c(0,10), ylim = c(0,10), pch = 16, 
     col = cardiffblue, bty = 'n', cex = 2)
points(x1,y1, col = cardiffblue, pch = 16, cex = 2)
points(x2,y2, col = cardiffblue, pch = 16, cex = 2)
points(x3,y3, col = cardiffred, pch = 16, cex = 2)
dev.off()


