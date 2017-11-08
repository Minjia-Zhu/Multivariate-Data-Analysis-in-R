
setwd("~/Desktop/hw2")
options(digits=3)
library(RColorBrewer)
#all palette available from RColorBrewer
#display.brewer.all()
#Utility function for ploting pac Chart
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", 
       ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", 
       ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

track = read.table("p1.txt")
colnames(track) = c("Country","100m","200m","400m","800m","1500m","3000m","Marathon")
CountryClasses <- factor(track$Country)
X <- data.matrix(track[-1])
n <- nrow(X)
p <- ncol(X)

#X_mean <- (1/n)* t(X) %*% matrix(1, n, 1)
X_mean <- colMeans(X)
t(X_mean)

#X_center <- X - matrix(1, n, 1) %*% t(X_mean)
#S <- (1/(n-1)) * t(X_center) %*% X_center
S <- cov(X)
S

#D <- diag(1/diag(S))
#R1 <- D^(0.5) %*% S %*% D^(0.5)
R <- cor(X)
R

R_log <- cor(log(X))
R_log

sigma = matrix(c(2,-1,1,-1,5,0,1,0,3),3,3)
e3 <- eigen(sigma)
e3$values

e3$vectors

e4 <- eigen(R)
e4_value <- e4$values
Q4 <- e4$vectors
print('eigenvalues are')
e4_value

print('matrix of eigenvector is')
Q4

round(sum(e4_value),2)
dim(X)

X.pc <- princomp(X,cor=T)
summary(X.pc,loading=T)

q1 <- Q4[,1,drop=FALSE]
q2 <- Q4[,2,drop=FALSE]
q1

q2

library(ggplot2)
plot(X.pc$scores[,1:2])
title("2D scatterplot of 54 obs in (PC1,PC2) plane")

country_score <- matrix(X.pc$scores[,1], nrow = n, dimnames = list(CountryClasses, c("Score")))
country_rank <- as.data.frame(country_score)
head(country_rank[order(-country_rank$Score), , drop = FALSE],n=6)

tail(country_rank[order(-country_rank$Score), , drop = FALSE],n=3)

air = read.table("p5.txt")
colnames(air) = c("Wind", "Radiation", "CO", "NO", "NO2", "O3", "HC")
cov(air)
airPCA_o <- princomp(air)
airPCA_s <- princomp(air,cor=T)
print("---------Summary for original data-------------")
summary(airPCA_o,loadings = T)
print("-------Summary for standardlized data-----------")
summary(airPCA_s,loadings = T)

#par(mfrow=c(1,2))
screeplot(airPCA_o,type="l",main ="Scree plot - Original Data")
screeplot(airPCA_s,type="l",main ="Scree plot - Standardized Data")

cov(air)

#Utility function to standardized data
stdData <- function(X) {
    n <- nrow(X)
    p <- ncol(X)
    E <- colMeans(X)
    X_center <- X - matrix(1, n, 1) %*% t(E)
    S <- cov(X)
    D <- diag(1/diag(S))
    A_hat <- data.matrix(X_center) %*% (D^0.5)
    colnames(A_hat) = colnames(X)
    A_hat
}

X_p6 = read.csv("p6-data.txt")
p <- ncol(X)
A_hat = stdData(X_p6)
A_hat

A_svd <- svd(A_hat)
A_d = A_svd$d
A_u = A_svd$u
A_v = A_svd$v
v1 = A_v[,1,drop=FALSE]
v2 = A_v[,2,drop=FALSE]
print("----v1 is----")
v1
print("----v2 is----")
v2

c
print("----u1 is----")
t(u1)
print("----u2 is----")
t(u2)

plot(u1,u2,xlab='v1',ylab = 'v2',data = A_hat)
 text(u1,u2,labels = rownames(A_hat))
 title('Projection onto v1 and v2')

plot(v1,v2,xlab='v1',ylab = 'v2',data=A_hat,type="n")
 arrows(0, 0, v1, v2)
 text(v1,v2,labels = colnames(A_hat))
 title('Projection onto u1 and u2')

plot(u1,u2,xlab='v1',ylab = 'v2',data = A_hat)
 text(u1,u2,labels = rownames(A_hat))
par(new=TRUE)
plot(v1,v2,xlab='u1',ylab = 'u2',data=A_hat,type="n")
 arrows(0, 0, v1, v2,col="red")
 text(v1,v2,labels = colnames(A_hat))
title ("biplot")

X_p6_r = X_p6[!rownames(X_p6) %in% c("Hong Kong","Singapore"), ]
A_hat_r = stdData(X_p6_r)
A_hat_r

A_svd_r <- svd(A_hat_r)
A_d_r = A_svd_r$d
A_u_r = A_svd_r$u
A_v_r = A_svd_r$v
v1_r = A_v_r[,1,drop=FALSE]
v2_r = A_v_r[,2,drop=FALSE]
u1_r = A_u_r[,1,drop=FALSE]
u2_r = A_u_r[,2,drop=FALSE]

plot(u1_r,u2_r,xlab='v1_r',ylab = 'v2_r',data = A_hat_r)
 text(u1_r,u2_r,labels = rownames(A_hat_r))
 title('Projection onto v1_r and v2_r (outlier removed)')

n <- nrow(X_p6)
p <- ncol(X_p6)
E <- colMeans(X_p6)
A_hat_c <- X - matrix(1, n, 1) %*% t(E)

A_svd_c <- svd(A_hat_c)
A_d_c = A_svd_c$d
A_u_c = A_svd_c$u
A_v_c = A_svd_c$v
v1_c = A_v_c[,1,drop=FALSE]
v2_c = A_v_c[,2,drop=FALSE]
u1_c = A_u_c[,1,drop=FALSE]
u2_c = A_u_c[,2,drop=FALSE]

plot(u1_c,u2_c,xlab='v1_c',ylab = 'v2_c',data = A_hat_c)
 text(u1_c,u2_c,labels = rownames(A_hat_c))
par(new=TRUE)
plot(v1_c,v2_c,xlab='u1_c',ylab = 'u2_c',data=A_hat_c,type="n")
 arrows(0, 0, v1_c, v2_c,col="red")
 text(v1_c,v2_c,labels = colnames(A_hat_c))
title ("biplot - with only mean centering")

A_svd_o <- svd(X_p6)
A_d_o = A_svd_o$d
A_u_o = A_svd_o$u
A_v_o = A_svd_o$v
v1_o = A_v_o[,1,drop=FALSE]
v2_o = A_v_o[,2,drop=FALSE]
u1_o = A_u_o[,1,drop=FALSE]
u2_o = A_u_o[,2,drop=FALSE]

plot(u1_o,u2_o,xlab='v1_o',ylab = 'v2_o',data = X_p6)
 text(u1_o,u2_o,labels = rownames(X_p6))
 title('Projection onto v1 and v2 - with Original data')

plot(v1_o,v2_o,xlab='u1_o',ylab = 'u2_o',data = X_p6,type = "n")
 arrows(0, 0, v1_o, v2_o,col="red")
 text(v1_o,v2_o,labels = colnames(X_p6))
 title('Projection onto u1 and u2 - with Original data')

plot(u1_o,u2_o,xlab='v1_o',ylab = 'v2_o',data = X_p6)
 text(u1_o,u2_o,labels = rownames(X_p6))
par(new=TRUE)
plot(v1_o,v2_o,xlab='u1_o',ylab = 'u2_o',data = X_p6,type = "n")
 arrows(0, 0, v1_o, v2_o,col="red")
 text(v1_o,v2_o,labels = colnames(X_p6))
title('Biplot - with Original data')
