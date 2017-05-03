x1 <- c(4,6,2,2,9,4,9,8,3,10)
x2 <- c(2,8,4,3,10,4,5,7,6,8)
xk <- c(1,2,1,1,2,1,2,2,1,2)

data1 <- data.frame(x1,x2,xk)
colnames(data1) <- c("x1","x2","kelas")

# mencari u1, u2 : rataan sebaran data dari tiap kelas
datakelas1 <- data1[data1$kelas==1,1:2]
datakelas2 <- data1[data1$kelas==2,1:2]

u1 <- c(mean(datakelas1[,1]), mean(datakelas1[,2]))
u2 <- c(mean(datakelas2[,1]), mean(datakelas2[,2]))

#mencari s <- jumlah dari (x-u1)(x-u1)
S1 <- cov(datakelas1)
S2 <- cov(datakelas2)

# mencari Sw
Sw <- S1+S2

# mencari Sb 
Sb <- (u1-u2)%*%t(u1-u2)

#
