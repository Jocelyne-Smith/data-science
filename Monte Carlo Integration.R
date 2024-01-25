x = runif(100, 0, 1)

y = sum(cos(2*pi*x))/100
y

x = runif(1000, 0, 1)

y = sum(cos(2*pi*x))/1000
y


x = runif(1000000, 0, 1)

y = sum(cos(2*pi*x*x))/1000000
y



x = runif(10000, 0, 1)
x


curve(expr = cos(2*pi*x*x), from = 0, to = 1)
abline(h = 0, col="black")


segments(x0 = x,
         x1 = x,
         y0 = cos(2*pi*x*x),
         y1 = 0,
         col = "blue") 


f = c(cos(2*pi*x*x))
fresnelC(f)
