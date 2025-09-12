
#|llabel: vectors

#1
#1.a
p_la<- 1:20
p_la
#|b
p_lb<- 20:1
p_lb
#|c
p_lc <- c(1:20,19:1)
p_lc
#|d
p_ld <- c(4,6,3)
tem<- p_ld
tem
#| e
tem <- c(4,6,3)
p_le <- rep(tem,10)
p_le
#|f
p_lf<- c(rep(tem,10),4)
p_lf
#|g
p_lg<-c(rep(4,10),rep(6,20),rep(3,30))
p_lg
length(which(p_le==4))

#2
x<-seq(3,6,by=0.1)
result=exp(x)*cos(x)
result

#3
#3.a
x<-seq(3,36,by=3)
y<-seq(1,34,by=3)
a<-c((0.1^x)*(0.2)^y)
a
#3.b
x<-1:25
b<-c((2^x)/x)
b

#4
#4.ax<-10:100
su<-sum(x^3+4*x^2)
su
#4.b
i<-1:25
su<-sum((2^i)/i+(3^i)/(i^2))
su

#5
#5.a
a<-paste('label',1:30)
a
#5.b
b<-paste("fn",1:30,sep='')
b

#6
#6.a
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)
n=length(xVec)
y_xVec <- yVec[2:n]-xVec[1:(n-1)]
y_xVec
#6.b
siny_cosxVec <- c(sin(yVec[1:(n-1)])/cos(xVec[2:n]))
siny_cosxVec
#6.c
abc<-(xVec[1:(n-2)]+2*xVec[2:(n-1)]-xVec[3:n])
abc
#6.d
d<-sum(exp(-xVec[2:n])/((xVec[1:(n-1)])+10))
d

#7
#7.a
a<-yVec[yVec>600]
a
#7.b
b<-which(yVec>600)
b
#7.c
c<-xVec[b]
c
#7.d
d<-sqrt(abs(xVec-mean(xVec)))
d
#7.e
e<-sum((max(yVec)-yVec)<=200)
e
#7.f
f<-sum(xVec%%2==0)
f
#7.g
g<-xVec[order(yVec)]
g
#7.h
h<-yVec[seq(1,n,by=3)]
h

#8
res<-1+sum(cumprod(seq(2,38,by=2)/seq(3,39,by=2)))
res




