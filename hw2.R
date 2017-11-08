setwd("~/Desktop/hw2")
#Utility function for ploting pac Chart
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}
#Problem 1
track = read.table("p1.txt")
colnames(track) = c("Country","100m","200m","400m","800m","1500m","3000m","Marathon")
track[5:8] <- track[5:8]*60
X <- data.matrix(track[-1])
n <- nrow(X)
p <- ncol(X)

#1(a)
#X_mean <- (1/n)* t(X) %*% matrix(1, n, 1)
X_mean <- colMeans(X)
X_mean
#Sample mean is meaningful for all variables after transfer all unit in seconds 

#1(b)
X_center = X - matrix(1, n, 1) %*% t(X_mean)
#S <- (1/(n-1)) * t(X_center) %*% X_center
S <- cov(X)
S

#1(c)
#D <- diag(1/diag(S))
#R1 <- D^(0.5) %*% S %*% D^(0.5)
R <- cor(X)
R

R_log <- cor(log(X))
R_log

#Problem 3
sigma = matrix(c(2,-1,1,-1,5,0,1,0,3),3,3)
e3 <- eigen(sigma)
e3$values
e3$vectors


#Problem 4
#4(a)
e4 <- eigen(R)
e4_value <- e4$values
Q <- e4$vectors
e4_value
Q
print(sum(e4_value))
dim(X)
#Sum of all eigenvalues is 7, which is exactly the number of col(parameter) in data matrix 

#4(b)(i)
X.pc = princomp(X,cor=T)
summary(X.pc,loading=T)
q1 = Q[,1,drop=FALSE]
q2 = Q[,2,drop=FALSE]
print(q1)
print(q2)
# We can see that q1 = Comp.1; q2 = Comp.2; 
# Therefore PC1 and PC2 are same with the first two eigenvectors in (a)

#4(b)(ii)
# 90.12% of total sample variation is explained by the first and second PCs.

#4(c)(i)
library(ggplot2)
track = read.table("p1.txt")
colnames(track) = c("Country","100m","200m","400m","800m","1500m","3000m","Marathon")
# The first column corresponds to the country
countryClasses <- factor(track$Country)
trackPCA <- princomp(track[,-1],cor=T)
plot(trackPCA)
plot(trackPCA$scores[,1:2])
dev.off() 

#4(c)(ii)
trackPCA$scores[,1]
country_score <- matrix(trackPCA$scores[,1], nrow = n, dimnames = list(countryClasses, c("Score")))
country_rank <- as.data.frame(country_score)
country_rank[order(-country_rank$Score), , drop = FALSE]
# Top 6 are:USA,GER,RUS,CHN,FRA,GBR   
# Last 3 are:PNG,COK,SAM 
# Yes, the rank corresponds with athletic excellence of countries.

#5(a)
air = read.table("p5.txt")
colnames(air) = c("Wind", "Radiation", "CO", "NO", "NO2", "O3", "HC")
airPCA_o <- princomp(air)
airPCA_s <- princomp(air,cor=T)
summary(airPCA_o,loadings = T)
summary(airPCA_s,loadings = T)
#For original data: The first 2 components explain more than 90% of the variability in the data
#For standardized data:The firat 7 components explain more than 90% of the variability in the data
#The reasoning can be showned in the below graph in 5(b)

#5(b)
pcaCharts(airPCA_o)
pcaCharts(airPCA_s)

#5(c)
#PCA based on original data: the variability in the data is mainly due to first two components. 
#PCA baed on standardized data:the variability in the data is not concentrated in a certain component.
# -> PCA based on original data capture the variability better

#6(a)
X = read.csv("p6-data.txt")
n = nrow(X)
p = ncol(X)
E <- colMeans(X)
X_center = X - matrix(1, n, 1) %*% t(E)
S <- cov(X)
D <- diag(1/diag(S))
R <- cor(X)
A_hat = data.matrix(X_center) %*% (D^0.5)

#6(b)
A_svd <- svd(A_hat)
A_d = A_svd$d
A_u = A_svd$u
A_v = A_svd$v
v1 = A_v[1,,drop=FALSE]
v2 = A_v[2,,drop=FALSE]
v1
v2
plot(A_d[1]*A_u[,1],A_d[2]*A_u[,2],xlab='v1',ylab = 'v2',data = A_hat)
text(A_d[1]*A_u[,1],A_d[2]*A_u[,2],labels = row.names(A_hat))
title('Projection onto v1 and v2')
#Outlier: Hongkong and Singapore

#6(c)
u1 = A_u[,1,drop=FALSE]
u2 = A_u[,2,drop=FALSE]
u1
u2
plot(A_d[1]*A_v[,1], A_d[2]*A_v[,2], xlab='u1', ylab = 'u2')
text(A_d[1]*A_v[,1], A_d[2]*A_v[,2],labels = row.names(A_hat))
title('Projection onto u1 and u2')

#6(d)
biplot( cbind(A_d[1]*A_u[,1],A_d[2]*A_u[,2]),
        cbind(A_d[1]*A_v[,1], A_d[2]*A_v[,2]),
        xlab = 'v1/u1', ylab = 'v2/u2')
text(A_d[1]*A_v[,1], A_d[2]*A_v[,2],labels = row.names(A_hat))
title('Biplot of standardized data onto v1/u1 and v2/u2')

jpeg('rplot.jpg')
#Population.sq.km and Population.1000HectarAgri are near two outlier countries Hong Kong
and Singapore.
