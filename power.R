# power calculation for surveys
errs <- vector()
for (N in 10:400) {
  ts <- vector()
  for (sims in 1:2000) {
    y1 <- rbinom(N,1,prob = 0.2)
    y2 <- rbinom(N,1,prob = 0.1)
    t <- t.test(y1,y2)
    ts[sims] <- t$p.value > 0.05
  }
  errs[N] <- sum(ts)/1000
}
plot(1-errs,xlab="#surveys per group")
abline(h=.80,col='red')
abline(h=.90,col='red')


invlogit <- function(x) {
  return(exp(x)/(1+exp(x)))
}

# surveys per person
M = 12
# N is the number of people in the group
s <- 1
ts <- vector()
for (N in 70:70) {
  for (sims in 1:1000) {
    re <- rnorm(N,0,s)
    p1 <- invlogit(-2 + re )
    p2 <- invlogit(-1.4 + re)
    y1 <- rbinom(N,size=M,prob=p1)
    y2 <- rbinom(N,size=M,prob=p2)
    
    t <- t.test(y1,y2)
    ts[sims] <- t$p.value > 0.05
  }
  errs[N] <- sum(ts)/1000
}
1-errs[N]
plot(1-errs,xlab="#surveys per group")
abline(h=.80,col='red')
abline(h=.90,col='red')

