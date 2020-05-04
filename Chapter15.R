#5.3.1
mx  = c(0, 0, 0, 1, 1, 1)
my  = c(1, 0, 1, 1, 0, 1)
mz  = c(1, 1, 1, 0, 1, 1)


v1 = seq(0, 1, length.out = 100)
plot(log(v1), asinh(v1), type = 'l')
plot(v1, asinh(v1), type = 'l')
v3 = seq(30, 3000, length = 100)
plot(log(v3), asinh(v3), type= 'l')
