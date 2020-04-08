library(forecast)
data("AirPassengers")
AirPassengers


temp <- fourier(x = AirPassengers, K = 4)
temp

u = 2*pi/12
u
sin(u)
cos(u)
sin(vec*u)

v = 4*pi/12
v
sin(vec*v)

# S1-12      C1-12      S2-12 C2-12 S3-12 C3-12      S4-12 C4-12
# [1,] 0.5000000  0.8660254  0.8660254   0.5     1     0  0.8660254  -0.5
# [2,] 0.8660254  0.5000000  0.8660254  -0.5     0    -1 -0.8660254  -0.5
# [3,] 1.0000000  0.0000000  0.0000000  -1.0    -1     0  0.0000000   1.0

# S1-12: sin 1 (first order) 12 (season)
# C1-12: cos 1 (first order) 12 (season)

# S2-12: sin 2 (second order) 12 (season)
# C2-12: cos 2 (second order) 13 (season)


head(temp)

plot(temp[,1], type = "l")
plot(temp[,2], type = "l")


plot(AirPassengers, type = "l")
plot(AirPassengers*temp[8,], type = "l")
