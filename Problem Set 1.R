# preparing the data
setwd("Z:/")
lifetable <- read.csv("ps1_data_F2023.csv")
attach(lifetable)
library(readr)
nDx <- as.numeric(gsub(",", "", nDx))
nNx <- as.numeric(gsub(",", "", nNx))

# Constructing life table 

n <- c(diff(x), NA)
 
nmx <- nDx/nNx

nqx <- numeric(length(n))
for(i in 1:(length(n)-1)) {
nqx[i] <- (n[i] * nmx[i])/(1+(n[i]-nax[i])*nmx[i])
}
nqx[length(n)]<-1 

lx <- numeric(length(n))
lx[1] <- 100000
for(i in 2:length(n)) {
  lx[i] <- lx[i-1]*(1-nqx[i-1])
}

ndx <- numeric(length(n))
for(i in 1:(length(n)-1)) {
  ndx[i] <- lx[i] - lx[i+1]
}
ndx[length(n)] <- lx[length(n)]

nLx <- numeric(length(n))
for(i in 1:(length(n)-1)) {
  nLx[i] <- lx[i+1]*n[i] + nax[i]*ndx[i]
}
nLx[length(n)] <- lx[length(n)]/nmx[length(n)]

Tx <- rev(cumsum(rev(nLx)))

ex <- Tx/lx 

lifetable$nqx <- nqx
lifetable$lx  <- lx
lifetable$ndx <- ndx
lifetable$nLx <- nLx
lifetable$nmx <- nmx
lifetable$Tx  <- Tx
lifetable$ex  <- ex

install.packages("openxlsx")
library(openxlsx)
write.xlsx(lifetable, "lifetable.xlsx")

# plotting the life table
library(ggplot2)

png("lx.png", width=400, height=600)
ggplot(lifetable, aes(x=seq_along(lx), y=lx)) + 
  geom_line() + 
  ggtitle("lx over Age") + 
  xlab("Age") + 
  ylab("lx")
dev.off()

png("ndx.png", width=400, height=600)
ggplot(lifetable, aes(x=seq_along(ndx), y=ndx)) + 
  geom_line() + 
  ggtitle("ndx over Age") + 
  xlab("Age") + 
  ylab("ndx")
dev.off()

png("nmx.png", width=400, height=600)
ggplot(lifetable, aes(x=seq_along(nmx), y=nmx)) + 
  geom_line() + 
  ggtitle("nmx over Age") + 
  xlab("Age") + 
  ylab("nmx")
dev.off()

# problem set c
lifetable[lifetable$x == 40, "ex"]

# problem set d 
lifetable[lifetable$x == 30, "lx"]/lx[1]

# problem set e 
lifetable[lifetable$x==65, "lx"]/lifetable[lifetable$x==30, "lx"]

# problem set f 
lifetable[lifetable$x==50, "nqx"]

# problem set g 
T_15 <- lifetable$Tx[lifetable$x == 15]
T_65 <- lifetable$Tx[lifetable$x == 65]
l_15 <- lifetable$lx[lifetable$x == 15]

(T_15 - T_65) / l_15

# problem set i 
1/lifetable[lifetable$x==0, "ex"]



# life table package
install.packages("LifeTables")
library(LifeTables)
lt.mx(nmx=lifetable$nmx, age=c(0,1,seq(5,85,5)), nax=nax)
lt.mx(nmx=lifetable$nmx, age=c(0,1,seq(5,85,5)), nax=NULL)
