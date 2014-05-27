
R-language at University of Eastern Finland

#2. Graph

vuodet1=observations$first
vuodet2=observations$second
plot(vuodet1, main="Population dynamics", xlab ="time", ylab="population size", xlim=c(0,100),ylim=c(0,1700), pch="|")
lines(logistic_model)
points(51:100,vuodet2, col="blue", pch=3)
maksimi1=max(observations$first)
text(tail(order(vuodet1),1), maksimi1+50, label=maksimi1)
curve(((logistic_model[51]-100)/exp(-50))*exp(-x)+100, 50,100, col=2, add=TRUE)
nimet=c("logistic growth model", "exponential decay model","observations in years 0-50", "observations in years 51-100")
legend(60,1300, nimet, lwd=c(2.5,2.5,NA,NA), col=c("black", "red", "black", "blue"), pch=c(NA,NA,"|","+")) 



#7.1. Count the n:th Fibonacci

fibo=c(1,1)
vast=function(n){
  
  
  while (length(fibo) < n) { 
    sij= length(fibo)
    uusi = fibo[sij]+fibo[sij-1]
    fibo=c(fibo, uusi)
  }
  fibo
}
vast(8)
#[1]  1  1  2  3  5  8 13 21

#7.2. Make a two column matrix of indices
#for entries in matrix A that have value 1.

B=c()
vast=function(A){
  for(i in 1:nrow(A)){
    for(j in 1:ncol(A)){
      if  (A[i,j]==1) C=c(i,j) else C=c(NA,NA)
      B=na.omit(rbind(B,C))
      D=matrix(B,nrow(B),2)
      
    }
  }
  D
}

A=rbind(c(1,0,1,0,1),c(0,0,1,0,0))
vast(A)

#     [,1] [,2]
#[1,]    1    1
#[2,]    1    3
#[3,]    1    5
#[4,]    2    3

