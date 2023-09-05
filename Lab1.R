##Relative Odering Test
ROTest <- function(vec)
{
  n <- length(vec)
  qij <- matrix(0, nrow = n, ncol = n)
  
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      if(i < j)
      {
        if(vec[i] > vec[j])
        {
          qij[i,j] <- 1
        }
      }
    }
  }
  Q <- sum(qij)
  Tau <- 1 - (4*Q/(n*(n-1)))
  Z <- Tau/sqrt((2*(2*n+5))/(9*n*(n-1)))
  alpha <- 0.05
  crit.val <- qnorm(1-alpha/2)
  decision <- ifelse(abs(Z) > crit.val, 'trend is present', 'No trend')
  output <- list(Q = Q, tau = Tau, Z = Z, decision = decision)
  return(output)
}


######################### Problem 1 ###########################
airpass <- read.csv("airpass_USA.csv", header = T)
head(airpass)
airpass <- as.matrix(airpass[,-1])
  
arp_vec <- c(airpass)

plot(arp_vec, type ="l", col = 'blue', ylab = 'Number of passenger bookings')

ROTest(arp_vec)
## Trend is present in the data

######################### Problem 2 #########################
unemp <- read.csv("Maine_unemployment.csv", header = T)
head(unemp)
plot(unemp$unemploy, type = 'l', ylab = "Unemployment Rate", main = "Monthly unemployment rate for the US state of Maine from January
1996 until August 2006", col = 'blue')

######### First part of the data (decreasing trend)
dat1 <- unemp[1:which.min(unemp$unemploy),]
plot(dat1, type = 'l', ylab = "Unemployment Rate", col = 'red')
ROTest(dat1)

######### Second part of the data (increasing trend)
dat2 <- unemp[which.min(unemp$unemploy):length(unemp),]
plot(dat2, type = 'l', ylab = "Unemployment Rate")
ROTest(dat2)

######################### Problem 3 #########################
nifty <- read.csv("nifty.csv", header = T)
head(nifty)
plot(nifty$Nifty.Returns, type = 'l', ylab = "Nifty Return Series")
ROTest(nifty$Nifty.Returns)










