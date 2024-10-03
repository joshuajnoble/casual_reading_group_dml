library(data.table) 
library(MASS)
library(stargazer)

x_matrix <- MASS::mvrnorm(
  n = 10000, 
  mu = c(1,2,3), 
  Sigma = matrix(
    data = c(
      10, -5, 1,     # diagonal are variance; 
      -5,  5, 2,     # off diagnoals are covariances
      1,  2, 3), 
    nrow = 3, ncol = 3
  )
)

d <- data.table(
  id  = 1:10000
)

d[ , ':='(
  x1      = x_matrix[,1], 
  x2      = x_matrix[,2], 
  epsilon = x_matrix[,3]
)]

d[ , treat_continuous := 0 + rnorm(n=.N, mean=3, sd=1) + (1*x1) + (2*x2) + (15*epsilon)]
d[ , treat_binary     := treat_continuous > quantile(treat_continuous, 0.9)]

d[ , Y := 0 * 1*x1 + 2*x2 + 3*treat_binary + 15*epsilon + rnorm(n=.N, mean=0, sd=1)]
d[ , t.test(Y ~ treat_binary)]

model_1 <- d[ , lm(Y ~ treat_binary)]
model_2 <- d[ , lm(Y ~ x1 + treat_binary)]
model_3 <- d[ , lm(Y ~ x2 + treat_binary)]
model_4 <- d[ , lm(Y ~ x1 + x2 + treat_binary)]
model_5 <- d[ , lm(Y ~ x1 + x2  + epsilon + treat_binary)]

stargazer(
  model_1, model_2, model_3, model_4, model_5,
  type = 'text', 
  omit.stat = c("ser", "F")
)


################################################################################################
d2 <- data.table(
  id  = 1:10000
)

d2[ , ':='(
  x1      = x_matrix[,1], 
  x2      = x_matrix[,2], 
  epsilon = x_matrix[,3]
)]

d2[ , treat_continuous := 0 + rnorm(n=.N, mean=3, sd=1) + (1*x1) + (2*x2) + (15*epsilon)]
d2[ , treat_binary     := treat_continuous > quantile(treat_continuous, 0.9)]

d2[ , Y := 0 * 1*x1 + 2*x2 + 6*treat_binary + 15*epsilon + rnorm(n=.N, mean=0, sd=1)]
d2[ , t.test(Y ~ treat_binary)]

model_1a <- d2[ , lm(Y ~ treat_binary)]
model_2a <- d2[ , lm(Y ~ x1 + treat_binary)]
model_3a <- d2[ , lm(Y ~ x2 + treat_binary)]
model_4a <- d2[ , lm(Y ~ x1 + x2 + treat_binary)]
model_5a <- d2[ , lm(Y ~ x1 + x2  + epsilon + treat_binary)]

stargazer(
  model_1a, model_2a, model_3a, model_4a, model_5a,
  type = 'text', 
  omit.stat = c("ser", "F")
)

write.csv(d2, "ate_6.csv")
