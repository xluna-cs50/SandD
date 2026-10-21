# SandD

# Prac-1

```r
# Prac-1
# question. population sample complete enumeration simple random sampling with and without replacement.
# Create a population of 50 students and display the first 6 observations.
set.seed(51)
students <- data.frame(ID = 1:50, MARKS = round(rnorm(50, 70, 5)))
head(students, 6)

# Find population size
N <- nrow(students)
print(N)

# Find the population mean
m <- mean(students$MARKS)
print(m)

# Draw a sample size of 10
set.seed(20)
sample <- sample(students$MARKS, 10, replace = FALSE)
sample

# calculate sampling error (SE)
sample_mean <- mean(sample)
SE <- sample_mean
SE

# Question 2
# 1. select SRSWOR sample of size 8.
SRSWOR <- sample(1:30, 8, replace = FALSE)
SRSWOR

# 2. select SRSWR sample of size 8.
SRSWR <- sample(1:30, 8, replace = TRUE)
SRSWR

# 3. compute their sample means
m1 <- mean(SRSWOR)
m2 <- mean(SRSWR)
m1
m2

# 4. find sample variance
var(SRSWOR)

# 5. find standard deviation
SD <- sd(SRSWOR)
SD
```

# Prac-2

```r
# Prac-2
# question. estimation of population mean and total, estimation of population and sample size determination.
# 1.) Create income data for 100 individuals
set.seed(123)
income <- round(rnorm(100, 50000, 6000))
head(income)

# select sample size of 20
samp <- sample(income, 20, replace = FALSE)
samp

# estimate population mean
m <- mean(samp)
m
# estimate population total
N <- 100
Mean <- N * m
Mean

# Calculate variance of estimated mean.
n <- 20
S2 <- var(samp)
var <- ((1 - n / N) * S2) / n
var

# Question 2.
# Create Pass/Fail data with passing probability of 70%
set.seed(500)
students <- sample(c(1, 0), 200, replace = TRUE, prob = c(0.7, 0.3))
head(students)

# estimate population proportion.
Mean <- mean(students)
Mean

# estimate variance of population.
N <- 200
n <- 40
var <- ((1 - n / N) * Mean * (1 - Mean)) / n
var
```

# Prac-3

```r
# Prac-3
# Q1. Draw a proportional stratified random sample from a population consisting of 3 strata and estimate the population mean.
set.seed(101)
stratum <- rep(c("A", "B", "C"), times = c(100, 150, 50))
income <- c(rnorm(100, 50, 5), rnorm(150, 60, 7), rnorm(50, 75, 6))
population <- data.frame(stratum, income)

Nh <- table(population$stratum)
N <- sum(Nh)
n <- 60
nh <- round(n * Nh / N)
nh

sample_data <- data.frame()
for (i in names(Nh)) {
  temp <- subset(population, stratum == i)
  sample_data <- rbind(sample_data, temp[sample(1:nrow(temp), nh[i]), ])
}
aggregate(income ~ stratum, sample_data, mean)
mean(sample_data$income)

# Q2. Estimate the population total from a proportional stratified sample.
set.seed(101)
Nh <- c(100, 150, 50)
Wh <- sum(Nh)
sample_mean <- c(50.41, 60.85, 74.72)
population_mean <- sum(Wh * sample_mean)
population_total <- population_mean * sum(Nh)
population_total

# Q3. Estimate the variance of the stratified sample mean.
Sh2 <- c(25, 49, 36)
Nh <- c(100, 150, 50)
nh <- c(20, 30, 10)
Wh <- Nh / sum(Nh)
variance <- sum((Wh^2) * (1 - nh / Nh) * (Sh2 / nh))
variance

# Q4. Perform optimum (Neyman) allocation for three strata.
Nh <- c(100, 150, 50)
Sh <- c(5, 7, 6)
n <- 60
nh <- round(Nh * Sh / sum(Nh * Sh) * n)
nh

# Q5. Compare proportional allocation with optimum allocation.
prop <- c(16, 34, 10)
optimum <- c(18, 38, 10)
comparison <- data.frame(
  stratum = c("A", "B", "C"),
  proportional = prop,
  Optimum = optimum
)

comparison
```

# Prac-4

```r
# Prac-4
# Q1.) systematic sampling
# select a systematic random sample from a population of size 100 using interval 5.
population <- 1:100
k <- 5
set.seed(10)
r <- sample(1:k, 1)
sample <- seq(r, 100, by = k)
sample

# Q2.) estimate the population mean using systematic sampling.
set.seed(10)
income <- round(rnorm(100, 60, 8))
r <- sample(1:5, 1)
sample <- income[seq(r, 100, 5)]
mean(sample)

# Q3.) estimate the population total using systematic sampling.
N <- 100
mean_sample <- 60.1
population_total <- N * mean_sample
population_total

# Q4.) compare systematic sampling mean with SRS mean.
set.seed(20)
Income <- round(rnorm(100, 60, 8))
r <- sample(1:5, 1)
SYS <- Income[seq(r, 100, 5)]
SRS <- sample(Income, 20)
mean(SYS)
mean(SRS)

# Q5.) illustrate systematic sampling in the presence of a linear trend and compare with SRS.
set.seed(2)
r <- sample(1:5, 1)
SYS <- population[seq(r, 100, 5)]
SRS <- sample(population, 20)
mean(SYS)
mean(SRS)
mean(population)
```

# Prac-5

```r
# Q1: Population mean using the ratio estimator
# Enter the values of Y
Y <- c(52, 60, 63, 75, 80)
# Enter the values of x
x <- c(50, 58, 61, 72, 78)

# Known population mean of x
Xbar <- 70

# Sample means
ybar <- mean(Y)
xbar <- mean(x)

# Ratio estimator
R <- ybar / xbar

# Estimated population mean
Y_ratio <- R * Xbar
Y_ratio

# Q2: Population total using the ratio estimator (N = 100)
N <- 100
Y_ratio <- (mean(Y) / mean(x)) * 70
Total_ratio <- N * Y_ratio
Total_ratio

# Q3: Population mean using the regression estimator
# Regression coefficient
b <- cov(x, Y) / var(x)

xbar <- mean(x)
ybar <- mean(Y)
Xbar <- 70

Y_reg <- ybar + b * (Xbar - xbar)
Y_reg

# Q4: MSE of the ratio estimator
n <- length(Y)

Sy2 <- var(Y)
Sx2 <- var(x)
Sxy <- cov(x, Y)

R <- mean(Y) / mean(x)

# Finite population correction (f = 0 as in notebook)
f <- 0

MSE_ratio <- ((1 - f) / n) * (Sy2 + (R^2) * Sx2 - 2 * R * Sxy)
MSE_ratio

# Q5: Compare regression estimator with SRS
n <- length(Y)
Sy2 <- var(Y)
rho <- cor(x, Y)

Var_SRS <- Sy2 / n
Var_Reg <- (Sy2 * (1 - rho^2)) / n

Var_SRS
Var_Reg
```

# Prac-6

```r
# Q1. Create population consisting of 6 different clusters each having four observations, display the population and identify the number of clusters and cluster size.
population <- matrix(c(12, 15, 14, 13,
                      18, 17, 20, 19,
                      25, 24, 23, 25,
                      30, 29, 21, 28,
                      35, 36, 34, 33,
                      40, 39, 41, 42),
                    nrow = 6, byrow = TRUE)
population

# Number of clusters
nrow(population)

# Cluster size
ncol(population)

# Q2. Select 3 clusters randomly using SRSWOR and display the sample clusters.
set.seed(100)
sample <- sample(1:6, 3)
sample
sample_data <- population[sample, ]
sample_data

# Q3. Estimate the population mean using the selected clusters.
cluster_means <- rowMeans(sample_data)
cluster_means
estimated_mean <- cluster_means
estimated_mean

# Q4. Estimate the variance of cluster sampling mean.
var_cluster_means <- var(cluster_means)
var_cluster_means

# Q5. Estimate the variance of the estimated population mean under cluster sampling.
N <- 6
n <- 3
var_mean <- (1 - n / N) * var_cluster_means / n
var_mean
```

# Prac-7

```r
# Q1: Create population (5 villages x 4 households)
population <- matrix(c(42, 45, 38, 44,
                       51, 55, 53, 57,
                       62, 65, 61, 64,
                       70, 72, 75, 71,
                       80, 84, 82, 86),
                     nrow = 5, byrow = TRUE)

colnames(population) <- paste0("Household-", 1:4)
rownames(population) <- paste0("Village-", 1:5)
population

N <- nrow(population)
M <- ncol(population)
N
# Q2: Select 3 villages using SRSWOR (first stage)
set.seed(123)
n <- 3
selected_villages <- sample(1:N, n, replace = FALSE)
selected_villages
sample_stage1 <- population[selected_villages, ]
sample_stage1

# Q3: Select 2 households from each selected village (second stage)
m <- 2
selected_households <- lapply(selected_villages, function(v) {
  sample(1:M, m, replace = FALSE)
})
selected_households

sample_data <- do.call(rbind, lapply(1:n, function(i) {
  v <- selected_villages[i]
  h <- selected_households[[i]]
  population[v, h]
}))
sample_data

# Q4: Estimate population mean
V_samp_mean <- rowMeans(sample_data)
V_samp_mean
est_mean <- mean(V_samp_mean)
est_mean

# Q5: Variance of two-stage estimator
N <- 5
n <- 3
M <- 4
m <- 2
V <- var(V_samp_mean)
w_var <- apply(sample_data, 1, var)
V1 <- (1 - n / N) * V / n
V2 <- (1 / (n * N)) * sum((1 - m / M) * w_var / m)
V_est <- V1 + V2
V
w_var
V1
V2
V_est
```

# Prac-8

```r
# Q1: Linear trend model (non-seasonal data)
year <- 2018:2024
sales <- c(120, 128, 135, 142, 151, 160, 168)

# Create time variable
t <- 1:length(sales)

# Fit linear trend model
model <- lm(sales ~ t)

# Display model summary
summary(model)

# Display coefficients
coef(model)

# Q2: Predict sales for 2025 and 2026
future_t <- data.frame(t = c(8, 9))
predicted_sales <- predict(model, newdata = future_t)
predicted_sales

# Q3: Create and plot seasonal time series
sales <- c(52, 48, 45, 50, 60, 65, 70, 68, 62, 58, 55, 72,
           58, 54, 51, 56, 66, 71, 76, 74, 68, 64, 61, 78)

sales_ts <- ts(sales,
               start = c(2024, 1),
               frequency = 12)
sales_ts

plot(sales_ts,
     main = "Monthly Retail Sales",
     xlab = "Year",
     ylab = "Sales")

# Q4: Regression with trend and seasonal effects (January = reference)
t <- 1:length(sales)
month <- factor(rep(1:12, 2))

seasonal_model <- lm(sales ~ t + month)
summary(seasonal_model)
coef(seasonal_model)

# Q5: Forecast Jan, Feb, Mar 2026
future_data <- data.frame(
  t = c(25, 26, 27),
  month = factor(c(1, 2, 3), levels = 1:12)
)

forecast <- predict(seasonal_model, newdata = future_data)

data.frame(
  Month = c("January 2026", "February 2026", "March 2026"),
  Forecast = forecast
)
```
