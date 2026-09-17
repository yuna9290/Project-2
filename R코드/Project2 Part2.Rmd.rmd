---
title: "Project2 Part2"
output: html_document
date: "2026-09-14"
---

```{r}
setwd('/Users/hwangsuyeon/Desktop/석사/석사 4학기/이통2/Project 2')
```

```{r}
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
```


# 1. Q-Q plot

```{r}
df_m <- read_excel("아파트 화재보험료.xlsx", sheet = "피해금액")
df_c <- read_excel("아파트 화재보험료.xlsx", sheet = "피해건수")
```

```{r}
# 아파트 자료만 추출
apt <- df_c %>%
  select(구분, 아파트) %>%
  filter(!is.na(구분)) %>%
  filter(아파트 > 0) %>% 
  filter(!구분 %in% c("합계")) %>%
  filter(!str_detect(구분, "^※"))

# 전체 아파트 화재건수
n <- sum(apt$아파트, na.rm = TRUE)
n
```

```{r}
apt_qq <- apt %>%
  # 상한이 존재하는 "미만" 구간만 사용
  filter(str_detect(구분, "미만")) %>%
  mutate(
    # 1,000미만 -> 1000
    X = as.numeric(str_remove_all(str_remove(구분, "미만"), ",")),
    # 누적 화재건수
    r = cumsum(아파트),
    # plotting position
    p = r / (n + 1),
    # Q-Q plot에서 사용할 log(X)
    logX = log(X)
  )

apt_qq
```

```{r}
# Pareto lambda 추정
apt_qq$pareto <- -log(1 - apt_qq$p)

lambda <- seq(2000, 4000, 5)

r2 <- c()

for(k in lambda){
  y <- log(1 + apt_qq$X / k)
  x <- apt_qq$pareto
  fit <- lm(y ~ x - 1)
  r2 <- c(r2, summary(fit)$r.squared)
}

# 최적 lambda
best_lam <- lambda[which.max(r2)]

best_lam
max(r2)
```

```{r}
qq_data <- bind_rows(

  # Lognormal
  apt_qq %>%
    transmute(
      distribution = "Lognormal",
      quantile = qnorm(p),
      Xr = logX
    ),

  # Pareto
  apt_qq %>%
    transmute(
      distribution = "Pareto",
      quantile = -log(1 - p),
      Xr = log(1 + X / best_lam)
    ),

  # Weibull
  apt_qq %>%
    transmute(
      distribution = "Weibull",
      quantile = log(-log(1 - p)),
      Xr = logX
    ),

  # Inverse Weibull / Frechet
  apt_qq %>%
    transmute(
      distribution = "iweibull",
      quantile = -log(-log(p)),
      Xr = logX
    ),

  # Log-logistic
  apt_qq %>%
    transmute(
      distribution = "Log-logistic",
      quantile = log(p / (1 - p)),
      Xr = logX
    )
)
```

```{r}
qq_data$distribution <- factor(
  qq_data$distribution,
  levels = c(
    "Lognormal",
    "Pareto",
    "Weibull",
    "iweibull",
    "Log-logistic"
  )
)
```

```{r}
# R-square 계산

r2_table <- qq_data %>%
  group_by(distribution) %>%
  summarise(
    R2 = summary(lm(Xr ~ quantile))$r.squared,
    .groups = "drop"
  )

r2_table
```

```{r}
ggplot(
  qq_data,
  aes(x = quantile, y = Xr)
) +
  geom_point(
    size = 2.5
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "red",
    linewidth = 1.2
  ) +
  facet_wrap(
    ~ distribution,
    nrow = 1,
    scales = "free"
  ) +
  geom_text(
    data = r2_table,
    aes(
      x = -Inf,
      y = Inf,
      label = paste0(
        "R² = ",
        round(R2, 2)
      )
    ),
    inherit.aes = FALSE,
    hjust = -0.15,
    vjust = 1.5,
    size = 5
  ) +
  labs(
    x = "quantile",
    y = "X(r)"
  ) +
  theme_gray(base_size = 14) +
  theme(
    strip.text = element_text(size = 15),
    axis.title = element_text(size = 14)
  )
```

# 2. 

```{r}
# Weibull 모수 추정
weibull_data <- qq_data %>%
  filter(distribution == "Weibull")

fit_weibull <- lm(Xr ~ quantile, data = weibull_data)

summary(fit_weibull)
coef(fit_weibull)
```

```{r}
mu_weibull <- coef(fit_weibull)[1]
sigma_weibull <- coef(fit_weibull)[2]
```
```{r}
tau_weibull <- 1 / sigma_weibull
c_weibull <- exp(-mu_weibull / sigma_weibull)
```

```{r}
# Pareto 모수 추정
pareto_data <- qq_data %>%
  filter(distribution == "Pareto")

fit_pareto <- lm(Xr ~ quantile - 1, data = pareto_data)

summary(fit_pareto)

# lambda
lambda_pareto <- best_lam

# slope = 1 / alpha
slope_pareto <- coef(fit_pareto)[1]
alpha_pareto <- 1 / slope_pareto
```

```{r}
parameter_result <- data.frame(
  Distribution = c("Weibull", "Pareto"),
  Parameter1 = c(
    paste0("c = ", round(c_weibull, 6)),
    paste0("lambda = ", round(lambda_pareto, 3))
  ),
  Parameter2 = c(
    paste0("tau = ", round(tau_weibull, 4)),
    paste0("alpha = ", round(alpha_pareto, 4))
  ),
  R2 = c(
    summary(fit_weibull)$r.squared,
    summary(fit_pareto)$r.squared
  )
)

parameter_result
```

# 3. 자기부담금(Deductible)과 보상한도(Limit)에 따른 적정 보험료(Premium)계산

```{r}
A_values <- c(0, 1000, 5000)
B_values <- c(10000, 20000, 50000, 100000, 200000)

# Pareto
alpha <- 2.0817
lambda <- 3825

# Weibull
c <- 0.041626
tau <- 0.4479
```

```{r}
# 평균 사고빈도
EN <- 617 / 5777

# Pareto survival function
S_pareto <- function(x){
  (lambda / (lambda + x))^alpha
}

# Weibull survival function
S_weibull <- function(x){
  exp(-c * x^tau)
}

# 한 사고당 평균 실제보상액 E(Y)
EY_pareto <- function(A, B){
  integrate(
    function(x) S_pareto(x),
    lower = A,
    upper = A + B
  )$value
}

EY_weibull <- function(A, B){
  integrate(
    function(x) S_weibull(x),
    lower = A,
    upper = A + B
  )$value
}
```

```{r}
result_premium <- expand.grid(
  A = A_values,
  B = B_values
)

result_premium$Pareto <- mapply(
  function(A, B){
    EN * EY_pareto(A, B)
  },
  result_premium$A,
  result_premium$B
)

result_premium$Weibull <- mapply(
  function(A, B){
    EN * EY_weibull(A, B)
  },
  result_premium$A,
  result_premium$B
)

# 천원 → 원으로 변환
result_premium$Pareto_won <- result_premium$Pareto * 1000
result_premium$Weibull_won <- result_premium$Weibull * 1000

result_premium
```

# 4. 자기부담금이 없고 보상한도가 없는 보험료의 적정보험료

## 방법 a) 추정한 분포를 이용한 보험료

```{r}
# Weibull 평균 피해액 E(X)
EX_weibull <- c^(-1/tau) * gamma(1 + 1/tau)

# 적정 보험료 Pa
Pa_weibull <- EN * EX_weibull

EX_weibull
Pa_weibull
```

```{r}
# Pareto 평균 피해액
EX_pareto <- lambda / (alpha - 1)

# 적정 보험료
Pa_pareto <- EN * EX_pareto

EX_pareto
Pa_pareto
```

## 방법 b) 2010년 경험 자료

```{r}
total_loss <- df_m %>%
  filter(구분 == "합계") %>%
  pull(아파트)

total_loss
```

```{r}
n_contract <- 5777
Pb <- total_loss / n_contract
Pb
```

# 5. 

## (가)

```{r}
df_r <- read_excel("아파트 화재보험료.xlsx", sheet = "화재보험 손해자료") %>%
  filter(구분 == "주택")

avg_premium <- df_r %>%
  filter(항목 == "평균보험료") %>%
  slice(1)

premium_5yr <- mean(
  as.numeric(avg_premium[1, c("2005년", "2006년", "2007년",
                             "2008년", "2009년")])
)

premium_5yr
```

```{r}
comparison_5a <- data.frame(
  방법 = c("최근 5년 실제 평균보험료",
           "Pa - Weibull",
           "Pa - Pareto",
           "Pb"),
  보험료_만원 = c(
    premium_5yr,
    323.4569 / 10,
    377.6656 / 10,
    326.0357 / 10
  )
)

comparison_5a
```

## (나)

```{r}
loss_row <- df_r %>%
  filter(항목 == "손해액") %>%
  slice(1)

# 계약건수
contract_row <- df_r %>%
  filter(항목 == "계약건수") %>%
  slice(1)

event_row <- df_r %>%
  filter(항목 == "손해건수") %>%
  slice(1)

# 실제 손해율
actual_lr_row <- df_r %>%
  filter(항목 == "손해율") %>%
  slice(1)

years <- c("2005년", "2006년", "2007년", "2008년", "2009년")


result_5b <- data.frame(
  연도 = years,
  손해액 = as.numeric(loss_row[1, years]),
  계약건수 = as.numeric(contract_row[1, years]),
  손해건수 = as.numeric(event_row[1, years]),
  실제 = as.numeric(actual_lr_row[1, years])
)

result_5b
```

```{r}
result_5b <- result_5b %>%
  mutate(
    EN_year = 손해건수 / 계약건수,

    Pa_weibull = EN_year * EX_weibull,
    Pa_pareto  = EN_year * EX_pareto
  )

result_5b
```

```{r}
result_5b <- result_5b %>%
  mutate(
    `Pa(파레토)` =
      (손해액 * 1000) /
      (계약건수 * Pa_pareto) * 100,

    `Pa(웨이블)` =
      (손해액 * 1000) /
      (계약건수 * Pa_weibull) * 100,

    `Pb` =
      (손해액 * 1000) /
      (계약건수 * Pb) * 100
  )
```

```{r}
result_5b_final <- result_5b %>%
  select(
    연도,
    `Pa(파레토)`,
    `Pa(웨이블)`,
    Pb,
    실제
  ) %>%
  mutate(
    across(
      c(`Pa(파레토)`, `Pa(웨이블)`, Pb, 실제),
      ~ round(.x, 2)
    )
  )

result_5b_final
```

```{r}
loss_2010 <- 1883508   # 천원
target_2010 <- 5777    # 건


row_2010 <- data.frame(
  연도 = "2010년",
  손해액 = NA,
  계약건수 = NA,
  
  `Pa(파레토)` =
    loss_2010 / (target_2010 * Pa_pareto) * 100,
  
  `Pa(웨이블)` =
    loss_2010 / (target_2010 * Pa_weibull) * 100,
  
  Pb =
    loss_2010 / (target_2010 * Pb) * 100,
  
  실제 = NA,
  
  check.names = FALSE
)
```

```{r}
result_final <- result_5b %>%
  select(
    연도,
    `Pa(파레토)`,
    `Pa(웨이블)`,
    Pb,
    실제
  ) %>%
  bind_rows(
    row_2010 %>%
      select(
        연도,
        `Pa(파레토)`,
        `Pa(웨이블)`,
        Pb,
        실제
      )
  ) %>%
  mutate(
    across(
      c(`Pa(파레토)`, `Pa(웨이블)`, Pb, 실제),
      ~ round(.x, 3)
    )
  )

result_final
```

