---
title: "Part 1"
author: "262STG09_조유나"
date: "2026-09-13"
output: word_document
---



```{r}
# ============================================================
# 이론통계 Project #2
# Part 1. 상해보험료 계산
#
# Data : 상해보험자료2.xlsx
# 단위 : 만원
# ============================================================


# ============================================================
# 0. 패키지
# ============================================================

library(readxl)


# ============================================================
# 1. 데이터 불러오기
# ============================================================

file <- "상해보험자료2.xls"

# 시트 확인
excel_sheets(file)


# ------------------------------------------------------------
# Table 2 : 자기부담금 없음
#
# 주의:
# 엑셀의 마지막 행에는 "50만원 이상", "전체 건수"가
# 문자로 들어 있으므로 전체 열을 그대로 읽으면
# amount가 character로 인식될 수 있다.
#
# 따라서 1~50만원 미만에 해당하는 행만 읽는다.
# ------------------------------------------------------------

data_full <- read_excel(
  file,
  sheet = "Table2-No-Deductible",
  range = "A1:B51"
)

names(data_full) <- c(
  "amount",
  "count"
)

# 숫자형 확인
str(data_full)


# ------------------------------------------------------------
# Table 3 : 자기부담금 5만원
# ------------------------------------------------------------

data_truncated <- read_excel(
  file,
  sheet = "Table3-with-Deductible",
  range = "A1:B51"
)

names(data_truncated) <- c(
  "amount",
  "count"
)

str(data_truncated)


# ============================================================
# 2. Table 2 데이터 확인
# ============================================================

data_full


# 50만원 미만 자료
full_below50 <- data_full[!is.na(data_full$count) & data_full$amount <= 50, ]

full_below50


# 50만원 미만 사고건수
claims_below50 <- sum(
  full_below50$count
)

claims_below50


# 전체 사고건수
total_claims <- 8443


# 50만원 이상 사고건수
claims_50plus <- total_claims - claims_below50

claims_50plus


# ============================================================
# 3. Histogram
#    X < 50
# ============================================================

hist_pos <- barplot(
  height = full_below50$count, 
  names.arg = rep("", nrow(full_below50)), 
  main = "Histogram of Medical Expenses (X < 50)", 
  xlab = "Medical Expense (10,000 KRW)", 
  ylab = "Frequency", xaxt = "n" ) 

axis( side = 1, 
      at = hist_pos[c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)], 
      labels = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50) )


# ============================================================
# 4. Q-Q plot용 자료 생성
# ============================================================

X <- full_below50$amount


# 표본 크기

n <- 8443


# Plotting position
r <- cumsum(full_below50$count)

p <- r / (n + 1)


# ============================================================
# 5. 5개 후보분포 Q-Q plot
# ============================================================


# ------------------------------------------------------------
# (1) Lognormal
#
# ln X_(r) = mu + sigma * Phi^(-1)(p_r)
# ------------------------------------------------------------

x_lognormal <- qnorm(p)

y_lognormal <- log(X)


fit_lognormal <- lm(y_lognormal ~ x_lognormal)

summary(fit_lognormal)


# ------------------------------------------------------------
# (2) Pareto
#
# ln(1 + X/lambda) = -(1/alpha) ln(1-p)
#
# lambda를 변화시키면서 R^2가 가장 큰 값을 선택
# ------------------------------------------------------------

lambda_grid <- seq(0.1, 100, length.out = 1000)


pareto_r2 <- numeric(length(lambda_grid))


for (i in seq_along(lambda_grid)) {

  lambda <- lambda_grid[i]

  y <- log(1 + X / lambda)

  x <- -log(1 - p)

  fit <- lm(y ~ x - 1)

  pareto_r2[i] <- summary(fit)$r.squared
}


# 최적 lambda
lambda_hat <- lambda_grid[which.max(pareto_r2)]

lambda_hat


# 최종 Pareto Q-Q 회귀
x_pareto <- -log(1 - p)

y_pareto <- log(1 + X / lambda_hat)


fit_pareto <- lm(y_pareto ~ x_pareto - 1)

summary(fit_pareto)


# alpha 추정
alpha_hat <- 1 / coef(fit_pareto)[1]

alpha_hat


# ------------------------------------------------------------
# (3) Weibull
#
# ln X_(r) = mu + sigma ln[-ln(1-p_r)]
# ------------------------------------------------------------

x_weibull <- log(-log(1 - p))

y_weibull <- log(X)


fit_weibull <- lm(y_weibull ~ x_weibull)

summary(fit_weibull)


# ------------------------------------------------------------
# (4) Inverse Weibull / Frechet
#
# ln X_(r) = mu - sigma ln[-ln(p_r)]
# ------------------------------------------------------------

x_frechet <- -log(-log(p))

y_frechet <- log(X)


fit_frechet <- lm(y_frechet ~ x_frechet)

summary(fit_frechet)


# ------------------------------------------------------------
# (5) Log-logistic
#
# ln X_(r) = mu + sigma log[p/(1-p)]
# ------------------------------------------------------------

x_loglogistic <- log(p / (1 - p))

y_loglogistic <- log(X)


fit_loglogistic <- lm(y_loglogistic ~ x_loglogistic)

summary(fit_loglogistic)


# ============================================================
# 6. 5개 Q-Q plot
# ============================================================

par(
  mfrow = c(2, 3),
  mar = c(4, 4, 3, 1)
)


# Lognormal
plot(
  x_lognormal,
  y_lognormal,
  pch = 16,
  cex = 0.35,
  main = "Lognormal",
  xlab = expression(Phi^{-1}(p)),
  ylab = "log(X)"
)

abline(
  fit_lognormal,
  col = "red",
  lwd = 2
)


# Pareto
plot(
  x_pareto,
  y_pareto,
  pch = 16,
  cex = 0.35,
  main = "Pareto",
  xlab = "-log(1-p)",
  ylab = "log(1+X/lambda)"
)

abline(
  fit_pareto,
  col = "red",
  lwd = 2
)


# Weibull
plot(
  x_weibull,
  y_weibull,
  pch = 16,
  cex = 0.35,
  main = "Weibull",
  xlab = "log(-log(1-p))",
  ylab = "log(X)"
)

abline(
  fit_weibull,
  col = "red",
  lwd = 2
)


# Inverse Weibull / Frechet
plot(
  x_frechet,
  y_frechet,
  pch = 16,
  cex = 0.35,
  main = "Inverse Weibull / Frechet",
  xlab = "-log(-log(p))",
  ylab = "log(X)"
)

abline(
  fit_frechet,
  col = "red",
  lwd = 2
)


# Log-logistic
plot(
  x_loglogistic,
  y_loglogistic,
  pch = 16,
  cex = 0.35,
  main = "Log-logistic",
  xlab = "log(p/(1-p))",
  ylab = "log(X)"
)

abline(
  fit_loglogistic,
  col = "red",
  lwd = 2
)


par(
  mfrow = c(1, 1)
)


# ============================================================
# 7. 5개 후보분포의 R^2 비교
# ============================================================

qq_result <- data.frame(

  Model = c(
    "Lognormal",
    "Pareto",
    "Weibull",
    "Inverse Weibull",
    "Log-logistic"
  ),

  R_squared = c(

    summary(fit_lognormal)$r.squared,

    summary(fit_pareto)$r.squared,

    summary(fit_weibull)$r.squared,

    summary(fit_frechet)$r.squared,

    summary(fit_loglogistic)$r.squared
  )
)


qq_result <- qq_result[
  order(
    -qq_result$R_squared
  ),
]


qq_result


# ============================================================
# 8. 최종 선택 모형
# ============================================================

# Q-Q plot의 직선성 및 R^2를 기준으로
# 상위 2개 모형을 선택

selected_models <- head(qq_result$Model, 2)

selected_models


# ============================================================
# 9. 선택된 모형의 모수 추정
# ============================================================

# ============================================================
# (1) Pareto
#
# ln(1 + X/lambda) = -(1/alpha) ln(1-p)
#
# 회귀식: y = beta * x
#
# beta = 1/alpha
# ============================================================

alpha_hat <- 1 / unname(coef(fit_pareto)[1])


lambda_hat

alpha_hat


# ============================================================
# (2) Inverse Weibull / Frechet
#
# ln X_(r) = mu - sigma ln[-ln(p_r)]
#
# 우리가 정의한 변수:
#
# x_frechet = -ln[-ln(p)]
#
# 따라서
#
# ln X = mu + sigma * x_frechet
# ============================================================

mu_frechet <- unname(coef(fit_frechet)[1])

sigma_frechet <- unname(coef(fit_frechet)[2])

# Frechet의 shape parameter
alpha_frechet <- 1 / sigma_frechet

# Frechet의 scale parameter
scale_frechet <- exp(mu_frechet)

mu_frechet
sigma_frechet
alpha_frechet
scale_frechet


# ============================================================
# 선택모형 모수 결과
# ============================================================

parameter_result <- data.frame(

  Model = c(
    "Pareto",
    "Inverse Weibull / Frechet"
  ),

  Lambda = c(lambda_hat, NA),

  Alpha = c(alpha_hat, alpha_frechet),

  Mu = c(NA, mu_frechet),

  Sigma = c(NA, sigma_frechet),

  Scale = c(NA, scale_frechet)
)

parameter_result


# ============================================================
# 10. 선택된 분포의 CDF / PDF
# ============================================================

# ============================================================
# (1) Pareto
# ============================================================

F_pareto <- function(
    x,
    alpha = alpha_hat,
    lambda = lambda_hat
    ) {

  ifelse(x >= 0,
    1 - (1 + x / lambda)^(-alpha),
    0)

}


f_pareto <- function(x) {

  ifelse(x >= 0,
    (alpha_hat / lambda_hat) * (1 + x / lambda_hat)^(-alpha_hat - 1),
     0)

}


# ============================================================
# (2) Inverse Weibull / Frechet
# ============================================================

# F(x) = exp[-(scale/x)^alpha]

F_frechet <- function(x) {

  ifelse(x > 0,
    exp(-(scale_frechet / x)^alpha_frechet),
    0
  )

}


# f(x)
f_frechet <- function(x) {

  ifelse(x > 0,
    alpha_frechet * scale_frechet^alpha_frechet *
      x^(-alpha_frechet - 1) * exp(-(scale_frechet / x)^alpha_frechet),
    0
  )

}


# ============================================================
# 11. 평균 사고빈도 E(N)
# ============================================================

# 표 1
#
# 계약건수 = 271,306
# 사고건수 = 8,443

contract_count <- 271306

claim_count <- 8443

EN <- claim_count / contract_count

EN


# ============================================================
# 12. 평균사고심도 E(Y)
#
# Y = min[(X-A)+, B]
#
# E(Y) = ∫[A,A+B] (x-A)f(x)dx + B[1-F(A+B)]
# ============================================================

expected_payment <- function(A, B, f, F) {

  integral_part <- integrate(

    function(x) {
      (x - A) * f(x)
    },

    lower = A,
    upper = A + B
    
  )$value


  tail_part <- B * (1 -F(A + B))


  integral_part + tail_part
}


# ============================================================
# 13. 보험료 계산
# ============================================================

A_values <- c(0, 10, 20)

B_values <- c(50, 100, 200, 500, 1000)


# ============================================================
# 보험료 계산 함수
# ============================================================

make_premium_table <- function(
    f,
    F,
    model_name
) {

  result <- expand.grid(

    A = A_values,

    B = B_values

  )


  result$Expected_Payment <-
    mapply(

      function(A, B) {

        expected_payment(
          A = A,
          B = B,
          f = f,
          F = F
        )

      },

      result$A,
      result$B

    )

  result$Premium <- EN * result$Expected_Payment

  result$Model <- model_name


  result

}


# ============================================================
# Pareto
# ============================================================

premium_pareto <- make_premium_table(

    f = f_pareto,
    F = F_pareto,
    model_name = "Pareto"

  )


# ============================================================
# Frechet
# ============================================================

premium_frechet <- make_premium_table(

    f = f_frechet,
    F = F_frechet,
    model_name = "Frechet"

  )


# 결과
premium_pareto

premium_frechet


# ============================================================
# 14. 두 모형의 보험료 비교
# ============================================================

premium_comparison <- merge(

  premium_pareto[
    ,
    c(
      "A",
      "B",
      "Premium"
    )
  ],

  premium_frechet[
    ,
    c(
      "A",
      "B",
      "Premium"
    )
  ],

  by = c(
    "A",
    "B"
  )

)


names(premium_comparison) <- c(

  "Deductible_A",

  "Limit_B",

  "Pareto",

  "Frechet"

)


premium_comparison


# ============================================================
# 원 단위로 변환
# ============================================================

premium_comparison_won <- premium_comparison

premium_comparison_won$Pareto <- premium_comparison_won$Pareto * 10000

premium_comparison_won$Frechet <- premium_comparison_won$Frechet * 10000


premium_comparison_won


# ============================================================
# 15. A=0, B=50에서 손해율 비교
# ============================================================

# 실제 총손해액
total_loss <- 259699


# 기존 보험료
# 3,130원 = 0.313만원
old_premium <- 0.313


# 기존 총보험료
old_total_premium <- contract_count * old_premium


# 기존 손해율
old_loss_ratio <- total_loss / old_total_premium * 100


# ============================================================
# Pareto 보험료
# A = 0, B = 50
# ============================================================

P_pareto <- premium_pareto$Premium[
  
  premium_pareto$A == 0 & premium_pareto$B == 50

  ]


# ============================================================
# Frechet 보험료
# A = 0, B = 50
# ============================================================

P_frechet <- premium_frechet$Premium[
  
  premium_frechet$A == 0 & premium_frechet$B == 50

  ]


# ============================================================
# 총보험료
# ============================================================

total_premium_pareto <- contract_count * P_pareto

total_premium_frechet <- contract_count * P_frechet


# ============================================================
# 손해율
# ============================================================

loss_ratio_pareto <- total_loss / total_premium_pareto * 100

loss_ratio_frechet <- total_loss / total_premium_frechet * 100


# ============================================================
# 결과표
# ============================================================

loss_ratio_result <- data.frame(

  Method = c(
    "Existing Premium",
    "Pareto",
    "Frechet"
  ),

  Premium_KRW = c(

    old_premium,

    P_pareto,

    P_frechet

  ) * 10000,

  Loss_Ratio_Percent = c(

    old_loss_ratio,

    loss_ratio_pareto,

    loss_ratio_frechet

  )

)


loss_ratio_result


# ============================================================
# 16. Part 1 - (6)
# 자기부담금 5만원이 있는 경우
# ============================================================

data_truncated


observed_below50 <- data_truncated[
  !is.na(data_truncated$count) &
    data_truncated$amount <= 50,
]

observed_below50


observed_claims_below50 <- sum(observed_below50$count)

observed_claims_below50


claims_50plus_truncated <- 3541

observed_n <- 7404


# ============================================================
# 5개 후보분포 Q-Q plot
# ============================================================

X <- observed_below50$amount


# 표본 크기

n <- 7404


# Plotting position
r <- cumsum(observed_below50$count)

p <- r / (n + 1)


# ------------------------------------------------------------
# (1) Lognormal
#
# ln X_(r) = mu + sigma * Phi^(-1)(p_r)
# ------------------------------------------------------------

x_lognormal <- qnorm(p)

y_lognormal <- log(X)


fit_lognormal <- lm(y_lognormal ~ x_lognormal)

summary(fit_lognormal)


# ------------------------------------------------------------
# (2) Pareto
#
# ln(1 + X/lambda) = -(1/alpha) ln(1-p)
#
# lambda를 변화시키면서 R^2가 가장 큰 값을 선택
# ------------------------------------------------------------

lambda_grid <- seq(0.1, 100, length.out = 1000)


pareto_r2 <- numeric(length(lambda_grid))


for (i in seq_along(lambda_grid)) {

  lambda <- lambda_grid[i]

  y <- log(1 + X / lambda)

  x <- -log(1 - p)

  fit <- lm(y ~ x - 1)

  pareto_r2[i] <- summary(fit)$r.squared
}


# 최적 lambda
lambda_hat <- lambda_grid[which.max(pareto_r2)]

lambda_hat


# 최종 Pareto Q-Q 회귀
x_pareto <- -log(1 - p)

y_pareto <- log(1 + X / lambda_hat)


fit_pareto <- lm(y_pareto ~ x_pareto - 1)

summary(fit_pareto)


# alpha 추정
alpha_hat <- 1 / coef(fit_pareto)[1]

alpha_hat


# ------------------------------------------------------------
# (3) Weibull
#
# ln X_(r) = mu + sigma ln[-ln(1-p_r)]
# ------------------------------------------------------------

x_weibull <- log(-log(1 - p))

y_weibull <- log(X)


fit_weibull <- lm(y_weibull ~ x_weibull)

summary(fit_weibull)


# ------------------------------------------------------------
# (4) Inverse Weibull / Frechet
#
# ln X_(r) = mu - sigma ln[-ln(p_r)]
# ------------------------------------------------------------

x_frechet <- -log(-log(p))

y_frechet <- log(X)


fit_frechet <- lm(y_frechet ~ x_frechet)

summary(fit_frechet)


# ------------------------------------------------------------
# (5) Log-logistic
#
# ln X_(r) = mu + sigma log[p/(1-p)]
# ------------------------------------------------------------

x_loglogistic <- log(p / (1 - p))

y_loglogistic <- log(X)


fit_loglogistic <- lm(y_loglogistic ~ x_loglogistic)

summary(fit_loglogistic)


# ============================================================
# 5개 Q-Q plot
# ============================================================

par(
  mfrow = c(2, 3),
  mar = c(4, 4, 3, 1)
)


# Lognormal
plot(
  x_lognormal,
  y_lognormal,
  pch = 16,
  cex = 0.35,
  main = "Lognormal",
  xlab = expression(Phi^{-1}(p)),
  ylab = "log(X)"
)

abline(
  fit_lognormal,
  col = "red",
  lwd = 2
)


# Pareto
plot(
  x_pareto,
  y_pareto,
  pch = 16,
  cex = 0.35,
  main = "Pareto",
  xlab = "-log(1-p)",
  ylab = "log(1+X/lambda)"
)

abline(
  fit_pareto,
  col = "red",
  lwd = 2
)


# Weibull
plot(
  x_weibull,
  y_weibull,
  pch = 16,
  cex = 0.35,
  main = "Weibull",
  xlab = "log(-log(1-p))",
  ylab = "log(X)"
)

abline(
  fit_weibull,
  col = "red",
  lwd = 2
)


# Inverse Weibull / Frechet
plot(
  x_frechet,
  y_frechet,
  pch = 16,
  cex = 0.35,
  main = "Inverse Weibull / Frechet",
  xlab = "-log(-log(p))",
  ylab = "log(X)"
)

abline(
  fit_frechet,
  col = "red",
  lwd = 2
)


# Log-logistic
plot(
  x_loglogistic,
  y_loglogistic,
  pch = 16,
  cex = 0.35,
  main = "Log-logistic",
  xlab = "log(p/(1-p))",
  ylab = "log(X)"
)

abline(
  fit_loglogistic,
  col = "red",
  lwd = 2
)


par(
  mfrow = c(1, 1)
)


# ============================================================
# 7. 5개 후보분포의 R^2 비교
# ============================================================

qq_result <- data.frame(

  Model = c(
    "Lognormal",
    "Pareto",
    "Weibull",
    "Inverse Weibull",
    "Log-logistic"
  ),

  R_squared = c(

    summary(fit_lognormal)$r.squared,

    summary(fit_pareto)$r.squared,

    summary(fit_weibull)$r.squared,

    summary(fit_frechet)$r.squared,

    summary(fit_loglogistic)$r.squared
  )
)


qq_result <- qq_result[order(-qq_result$R_squared),]

qq_result


# ============================================================

true_n1 <- 8443 - 7404

true_m <- 8443


c(observed_below50 = observed_claims_below50,

  observed_50plus = claims_50plus_truncated,

  observed_total = observed_n,

  true_n1 = true_n1,

  true_m = true_m
)


# ============================================================
# 17. 절단자료의 구간 구성
# ============================================================

truncated_data <- data_truncated[
  !is.na(data_truncated$count) &
    data_truncated$amount >= 6 &
    data_truncated$amount <= 50,
]


lower_bounds <- truncated_data$amount - 1

upper_bounds <- truncated_data$amount


obs_counts <- truncated_data$count


# ============================================================
# 18. 절단자료 Pareto MLE
# ============================================================

loglik_truncated_pareto <- function(
    par
) {

  # alpha > 0
  alpha <- exp(par[1])
  
  
  # lambda
  lambda <- exp(par[2])


  # 5만원 미만 사고건수
  n1 <- exp(par[3])


  # 전체 사고건수
  m <- observed_n + n1


  # ==========================================================
  # 5만원 미만
  # ==========================================================

  p0 <- F_pareto(5, alpha, lambda)


  # ==========================================================
  # 5~50만원 구간
  # ==========================================================

  p_interval <- F_pareto(upper_bounds, alpha, lambda) - F_pareto(lower_bounds, alpha, lambda)


  # ==========================================================
  # 50만원 이상
  # ==========================================================

  p_50plus <- 1 - F_pareto(50, alpha, lambda)


  probabilities <- c(

    p0,

    p_interval,

    p_50plus

  )


  counts <- c(

    n1,

    obs_counts,

    claims_50plus_truncated

  )


  # 유효성 검사
  if (
    any(probabilities <= 0) ||
    any(!is.finite(probabilities))
  ) {

    return(1e100)

  }


  # ==========================================================
  # Multinomial log-likelihood
  # ==========================================================

  loglik <- lgamma(m + 1) - sum(lgamma(counts + 1)) +
            sum(counts * log(probabilities))


  return(-loglik)
}


# ============================================================
# 초기값
# ============================================================

start_pareto <- c(

  log(alpha_hat),
  
  log(lambda_hat),

  log(true_n1)

)


# ============================================================
# 최적화
# ============================================================

fit_trunc_pareto <-
  optim(

    par = start_pareto,

    fn = loglik_truncated_pareto,

    method = "Nelder-Mead",

    control = list(
      maxit = 10000
    )

  )


fit_trunc_pareto$convergence


# ============================================================
# 추정값
# ============================================================

alpha_pareto_mle <- exp(fit_trunc_pareto$par[1])

lambda_pareto_mle <- exp(fit_trunc_pareto$par[2])

n1_pareto_mle <- exp(fit_trunc_pareto$par[3])

m_pareto_mle <- observed_n + n1_pareto_mle


# ============================================================
# 결과
# ============================================================

pareto_truncated_result <- data.frame(

  Parameter = c(
    "lambda",
    "alpha",
    "n1",
    "m"
  ),

  Estimate = c(

    lambda_pareto_mle,

    alpha_pareto_mle,

    n1_pareto_mle,

    m_pareto_mle

  )

)

pareto_truncated_result


# ============================================================
# 19. 절단자료 Frechet MLE
# ============================================================

loglik_truncated_frechet <- function(par) {

  # mu
  mu <- par[1]


  # sigma > 0
  sigma <- exp(par[2])


  # n1 > 0
  n1 <- exp(par[3])


  # Frechet parameters
  alpha <- 1 / sigma

  scale <- exp(mu)


  # ==========================================================
  # CDF
  # ==========================================================

  F_frechet_temp <- function(x) {

    ifelse(
      x > 0,

      exp(-(scale / x)^alpha),

      0

    )

  }


  # ==========================================================
  # 전체 사고건수
  # ==========================================================

  m <- observed_n + n1


  # ==========================================================
  # 5만원 미만
  # ==========================================================

  p0 <- F_frechet_temp(5)


  # ==========================================================
  # 5~50만원 구간
  # ==========================================================

  p_interval <- F_frechet_temp(upper_bounds) - F_frechet_temp(lower_bounds)


  # ==========================================================
  # 50만원 이상
  # ==========================================================

  p_50plus <- 1 - F_frechet_temp(50)

  probabilities <- c(

    p0,

    p_interval,

    p_50plus

  )


  counts <- c(

    n1,

    obs_counts,

    claims_50plus_truncated

  )


  # ==========================================================
  # 유효성 검사
  # ==========================================================

  if (

    any(probabilities <= 0) || any(!is.finite(probabilities))

  ) {

    return(1e100)

  }


  # ==========================================================
  # Multinomial log-likelihood
  # ==========================================================

  loglik <- lgamma(m + 1) - sum(lgamma(counts + 1)) +
            sum(counts * log(probabilities))

  return(-loglik)
}


# ============================================================
# 초기값
# ============================================================

start_frechet <- c(

  mu_frechet,

  log(sigma_frechet),

  log(true_n1)

)


# ============================================================
# 최적화
# ============================================================

fit_trunc_frechet <-
  optim(
    par = start_frechet,
    fn = loglik_truncated_frechet,
    method = "Nelder-Mead",
    control = list(
      maxit = 10000
    )
  )


fit_trunc_frechet$convergence


# ============================================================
# 추정값
# ============================================================

mu_frechet_mle <- fit_trunc_frechet$par[1]

sigma_frechet_mle <- exp(fit_trunc_frechet$par[2])

n1_frechet_mle <- exp(fit_trunc_frechet$par[3])

m_frechet_mle <- observed_n + n1_frechet_mle

alpha_frechet_mle <- 1 / sigma_frechet_mle

scale_frechet_mle <- exp(mu_frechet_mle)


# ============================================================
# 결과
# ============================================================

frechet_truncated_result <- data.frame(

  Parameter = c(
    "mu",
    "sigma",
    "alpha",
    "scale",
    "n1",
    "m"
  ),

  Estimate = c(

    mu_frechet_mle,

    sigma_frechet_mle,

    alpha_frechet_mle,

    scale_frechet_mle,

    n1_frechet_mle,

    m_frechet_mle

  )

)


frechet_truncated_result


# ============================================================
# 20. Part 1 - (6) 최종 비교
# ============================================================

truncated_result <- data.frame(

  Method = c(
    "Actual",
    "Pareto MLE",
    "Frechet MLE"
  ),

  n1_under_5 = c(

    true_n1,

    n1_pareto_mle,

    n1_frechet_mle

  ),

  Total_m = c(

    true_m,

    m_pareto_mle,

    m_frechet_mle

  )

)


truncated_result


# ============================================================
# 21. Part 1 - 최종 결과 한 번에 출력
# ============================================================

cat("\n")
cat("============================================\n")
cat("             PART 1 FINAL RESULTS\n")
cat("============================================\n")


cat("\n[1] 50만원 미만 사고건수\n")
print(claims_below50)


cat("\n[2] 후보분포 Q-Q plot R^2\n")
print(qq_result)


cat("\n[3] 선택된 분포\n")
print(
  c(
    "Pareto",
    "Inverse Weibull / Frechet"
  )
)


cat("\n[4] 선택모형 모수 추정\n")
print(parameter_result)


cat("\n[5] 보험료 비교 (원)\n")
print(premium_comparison_won)


cat("\n[6] A=0, B=50 손해율 비교\n")
print(loss_ratio_result)


cat("\n[7] 자기부담금 5만원 절단자료 추정\n")
print(truncated_result)


cat("\n")
cat("============================================\n")
```




