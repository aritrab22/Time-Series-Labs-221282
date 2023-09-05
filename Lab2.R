############## Question 1##############
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

#Question 1
gdp <- read.csv('GDP_per_capita_Scandinavia.csv')
head(gdp)

data_swe = gdp[gdp$counry == 'SWE',]$GDP..USD_per_capita.
data_den = gdp[gdp$counry == 'DNK',]$GDP..USD_per_capita.
data_nor = gdp[gdp$counry == 'NOR',]$GDP..USD_per_capita.
data_fin = gdp[gdp$counry == 'FIN',]$GDP..USD_per_capita.

ROTest(data_swe)
ROTest(data_den)
ROTest(data_nor)
ROTest(data_fin)


#polynomial trend estimation test and estimated trend values
polynomial_trend_est <- function(data, k){
  x <- 1:length(data)
  formula <- as.formula(paste("data ~ poly(x, degree = ", k, ")", sep = ""))
  model <- summary(lm(formula))
  detrended_data <- data - fitted(lm(formula))
  
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
    return(decision)
  }
  output <- list(decision = ROTest(detrended_data), est_trend_value = fitted(lm(formula)), detrended_series = detrended_data)
  return(output)
}

polynomial_trend_est(data_swe, k=1)
polynomial_trend_est(data_den, k=1)
polynomial_trend_est(data_nor, k=1)
polynomial_trend_est(data_fin, k=1)

polynomial_trend_est(data_swe, k=2)
polynomial_trend_est(data_den, k=2)
polynomial_trend_est(data_nor, k=2)
polynomial_trend_est(data_fin, k=2)

#residual_Series
res_swe = as.vector(polynomial_trend_est(data_swe, k=2)$detrended_series)
res_den = as.vector(polynomial_trend_est(data_den, k=2)$detrended_series)
res_nor = as.vector(polynomial_trend_est(data_nor, k=2)$detrended_series)
res_fin = as.vector(polynomial_trend_est(data_fin, k=2)$detrended_series)

#test for randomness of residual series
random_test <- function(vec){
  n <- length(vec)
  U <- rep(0, n-2)
  for(i in 2:(n-1))
  {
    if((vec[i]>vec[i-1] & vec[i]>vec[i+1]) | (vec[i]<vec[i-1] & vec[i]<vec[i+1])){
      U[i] = 1
    }
  }
  Q <- sum(U)
  Z <- (Q - (2*(n-2)/3)) / ((16*n - 29)/90)
  alpha <- 0.05
  crit.val <- qnorm(1-alpha/2)
  decision <- ifelse(abs(Z) > crit.val, 'series is not purely random', 'series is purely random')
  return(decision)
}


random_test(res_swe)
random_test(res_den)
random_test(res_nor)
random_test(res_fin)


#differenced series and test of existence of trend
swe_diff1 = diff(data_swe)
den_diff1 = diff(data_den)
nor_diff1 = diff(data_nor)
fin_diff1 = diff(data_fin)

ROTest(swe_diff1)
ROTest(den_diff1)
ROTest(nor_diff1)
ROTest(fin_diff1)

swe_diff2 = diff(swe_diff1)
den_diff2 = diff(den_diff1)
nor_diff2 = diff(nor_diff1)
fin_diff2 = diff(fin_diff1)

ROTest(swe_diff2)
ROTest(den_diff2)
ROTest(nor_diff2)
ROTest(fin_diff2)

#moving avg (exponentially weighted)
#-----------------------------------

#plots

year_swe = gdp$year[gdp$counry == 'SWE']
year_den = gdp$year[gdp$counry == 'DNK']
year_nor = gdp$year[gdp$counry == 'NOR']
year_fin = gdp$year[gdp$counry == 'FIN']

par(mfrow = c(2,2))
plot(year_swe, data_swe, type = 'l', col = 'blue', xlab = 'Year', ylab = 'GDP Per Capita (in USD)', main = 'Original')
plot(year_swe, res_swe, type = 'l', col = 'blue', xlab = 'Year', ylab = 'GDP Per Capita (in USD)', main = 'Detrended using Quadratic fit')
plot(c(1:length(swe_diff2)), swe_diff2, type = 'l', col = 'blue', xlab = 'Index', main = 'Detrended using differencing')
mtext('Sweden', side = 3, line = -1, outer = T, font = 4)

par(mfrow = c(2,2))
plot(year_den, data_den, type = 'l', col = 'blue', xlab = 'Year', ylab = 'GDP Per Capita (in USD)', main = 'Original')
plot(year_den, res_den, type = 'l', col = 'blue', xlab = 'Year', ylab = 'GDP Per Capita (in USD)', main = 'Detrended using Quadratic fit')
plot(c(1:length(den_diff2)), den_diff2, type = 'l', col = 'blue', xlab = 'Index', main = 'Detrended using differencing')
mtext('Denmark', side = 3, line = -1, outer = T, font = 4)

par(mfrow = c(2,2))
plot(year_nor, data_nor, type = 'l', col = 'blue', xlab = 'Year', ylab = 'GDP Per Capita (in USD)', main = 'Original')
plot(year_nor, res_nor, type = 'l', col = 'blue', xlab = 'Year', ylab = 'GDP Per Capita (in USD)', main = 'Detrended using Quadratic fit')
plot(c(1:length(nor_diff2)), nor_diff2, type = 'l', col = 'blue', xlab = 'Index', main = 'Detrended using differencing')
mtext('Norway', side = 3, line = -1, outer = T, font = 4)

par(mfrow = c(2,2))
plot(year_fin, data_fin, type = 'l', col = 'blue', xlab = 'Year', ylab = 'GDP Per Capita (in USD)', main = 'Original')
plot(year_fin, res_fin, type = 'l', col = 'blue', xlab = 'Year', ylab = 'GDP Per Capita (in USD)', main = 'Detrended using Quadratic fit')
plot(c(1:length(fin_diff2)), fin_diff2, type = 'l', col = 'blue', xlab = 'Index', main = 'Detrended using differencing')
mtext('Finland', side = 3, line = -1, outer = T, font = 4)