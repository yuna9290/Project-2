# =====================================================================
# Part 3) 학교화재보험료 산정 -- R 코드 (로버스트 버전)
# 5개 모형: Log-normal / Pareto / Weibull / Inverse-Weibull(Frechet,log-Gumbel) / Log-logistic
# 엑셀의 빈 행/열이 자동으로 잘려나가도 안전하도록, 좌표를 하드코딩하지 않고
# 셀 "내용"을 찾아서 위치를 자동으로 탐지합니다.
# =====================================================================

library(readxl)

## ---- 0. 데이터 불러오기 ----
dir  <- "/Users/jaeseokang"
path <- file.path(dir, "2008-2017-학교화재.xlsx")
stopifnot(file.exists(path))

raw <- read_excel(path, col_names = FALSE)
raw <- as.data.frame(raw, stringsAsFactors = FALSE)

## ---- 헤더 행/열 자동탐지 ----
# "피해액" 이라는 글자가 들어있는 셀 = 계급구간 라벨 열의 헤더
hdr_pos <- which(apply(raw, c(1,2), function(v) grepl("피해액", as.character(v))), arr.ind = TRUE)
hdr_row <- hdr_pos[1, "row"]
label_col <- hdr_pos[1, "col"]
cat("헤더 행 =", hdr_row, " / 계급라벨 열 =", label_col, "\n")

# 헤더행에서 연도(2008~2017) 셀의 열 위치 찾기
year_vals <- suppressWarnings(as.numeric(raw[hdr_row, ]))
col_2013_2017 <- which(year_vals %in% 2013:2017)
col_2008_2012 <- which(year_vals %in% 2008:2012)
cat("2013~2017 열 =", col_2013_2017, "\n")
cat("2008~2012 열 =", col_2008_2012, "\n")

## ---- 계급구간 행 자동탐지 ----
# 주의: as.character(100000) 은 "1e+05" 가 되어 엑셀의 "100000" 텍스트와
#       안 맞을 수 있으므로, 반드시 "숫자값"으로 변환해서 비교한다.
labels_chr <- as.character(raw[, label_col])
labels_num <- suppressWarnings(as.numeric(labels_chr))
finite_bins <- c(50, 100, 1000, 5000, 10000, 100000)
bin_rows  <- sapply(finite_bins, function(v) which(labels_num == v)[1])
open_row  <- which(grepl("10억", labels_chr))[1]
cat("유한 계급 행 =", bin_rows, " / 개방구간 행 =", open_row, "\n")
stopifnot("계급 행을 못 찾았습니다 - label_col/헤더 탐지를 확인하세요" = !any(is.na(bin_rows)))
stopifnot("개방구간(10억이상) 행을 못 찾았습니다" = !is.na(open_row))

## ---- 도수 집계 ----
freq_1317 <- rowSums(sapply(raw[bin_rows, col_2013_2017], as.numeric))
freq_open_1317 <- sum(as.numeric(raw[open_row, col_2013_2017]))

freq_0812 <- rowSums(sapply(raw[bin_rows, col_2008_2012], as.numeric))
freq_open_0812 <- sum(as.numeric(raw[open_row, col_2008_2012]))

x <- finite_bins
n <- sum(freq_1317) + freq_open_1317
n_0812 <- sum(freq_0812) + freq_open_0812

cat("\n계급상한 x =", x, "\n")
cat("도수(2013~2017) =", freq_1317, " / 개방구간 =", freq_open_1317, " / n =", n, "\n")
cat("도수(2008~2012) =", freq_0812, " / 개방구간 =", freq_open_0812, " / n_0812 =", n_0812, "\n")

r   <- cumsum(freq_1317)        # 누적도수 (순위 r)
p   <- r / (n + 1)              # p_r = r/(n+1)
lnx <- log(x)

## =====================================================================
## a) 5개 모형 Q-Q plot 회귀분석
## =====================================================================

## 1. 로그정규분포 (Lognormal): ln X_(r) = mu + sigma*Phi^-1(p_r)
fit_lognormal <- lm(lnx ~ qnorm(p))

## 3. 와이블분포 (Weibull): ln X_(r) = mu + sigma*ln(-ln(1-p_r))
fit_weibull <- lm(lnx ~ log(-log(1 - p)))

## 4. 역와이블분포 (Inverse Weibull; Frechet, log-Gumbel):
##    ln X_(r) = mu - sigma*ln(-ln(p_r))   (기울기 = -sigma)
fit_frechet <- lm(lnx ~ log(-log(p)))

## 5. 로그로지스틱 (Log-logistic): ln X_(r) = mu + sigma*ln(r/[(n+1)-r])
fit_loglogis <- lm(lnx ~ log(p / (1 - p)))

## 2. 파레토분포 (Pareto): ln(1+X_(r)/lambda) = -ln(1-p_r)/alpha + e_r
T_pareto <- -log(1 - p)
pareto_R2 <- function(loglambda) {
  lam <- exp(loglambda)
  y <- log(1 + x / lam)
  -summary(lm(y ~ T_pareto))$r.squared
}
opt <- optimize(pareto_R2, interval = c(log(0.01), log(1e7)))
lambda_hat <- exp(opt$minimum)
y_pareto <- log(1 + x / lambda_hat)
fit_pareto <- lm(y_pareto ~ T_pareto)
alpha_hat <- 1 / coef(fit_pareto)[2]

## ---- R^2 비교 ----
R2 <- c(
  Lognormal   = summary(fit_lognormal)$r.squared,
  Pareto      = summary(fit_pareto)$r.squared,
  Weibull     = summary(fit_weibull)$r.squared,
  Frechet     = summary(fit_frechet)$r.squared,
  LogLogistic = summary(fit_loglogis)$r.squared
)
print(round(sort(R2, decreasing = TRUE), 4))

best2 <- names(sort(R2, decreasing = TRUE))[1:2]
cat("\n최적합 2개 분포:", best2, "\n\n")

## ---- Q-Q plot 시각화 ----
par(mfrow = c(2, 3))
plot(qnorm(p), lnx, main = paste0("Lognormal (R2=", round(R2["Lognormal"],4),")"),
     xlab = "Phi^-1(p)", ylab = "ln(x)"); abline(fit_lognormal, col = "red")
plot(T_pareto, y_pareto, main = paste0("Pareto (R2=", round(R2["Pareto"],4),")"),
     xlab = "-ln(1-p)", ylab = "ln(1+x/lambda_hat)"); abline(fit_pareto, col = "red")
plot(log(-log(1-p)), lnx, main = paste0("Weibull (R2=", round(R2["Weibull"],4),")"),
     xlab = "ln(-ln(1-p))", ylab = "ln(x)"); abline(fit_weibull, col = "red")
plot(log(-log(p)), lnx, main = paste0("Frechet (R2=", round(R2["Frechet"],4),")"),
     xlab = "ln(-ln(p))", ylab = "ln(x)"); abline(fit_frechet, col = "red")
plot(log(p/(1-p)), lnx, main = paste0("Log-logistic (R2=", round(R2["LogLogistic"],4),")"),
     xlab = "ln(p/(1-p))", ylab = "ln(x)"); abline(fit_loglogis, col = "red")

## ---- 파라미터 정리 ----
mu_ln  <- coef(fit_lognormal)[1]; sigma_ln  <- coef(fit_lognormal)[2]
mu_ll  <- coef(fit_loglogis)[1];  sigma_ll  <- coef(fit_loglogis)[2]
mu_fr  <- coef(fit_frechet)[1];   sigma_fr  <- -coef(fit_frechet)[2]
tau_fr <- 1/sigma_fr;             c_fr      <- exp(mu_fr/sigma_fr)
mu_wb  <- coef(fit_weibull)[1];   sigma_wb  <- coef(fit_weibull)[2]
tau_wb <- 1/sigma_wb;             c_wb      <- exp(-mu_wb/sigma_wb)
alpha_pa <- as.numeric(alpha_hat); lambda_pa <- lambda_hat

cat("Lognormal:    mu =", mu_ln, " sigma =", sigma_ln, "\n")
cat("Log-logistic: mu =", mu_ll, " sigma =", sigma_ll, "\n")
cat("Frechet:      mu =", mu_fr, " sigma =", sigma_fr, " (tau=",tau_fr,", c=",c_fr,")\n")
cat("Weibull:      mu =", mu_wb, " sigma =", sigma_wb, " (tau=",tau_wb,", c=",c_wb,")\n")
cat("Pareto:       lambda =", lambda_pa, " alpha =", alpha_pa, "\n")

## =====================================================================
## b) Log-Normal 모형: 평균피해액 E(X) 및 적정보험료
## =====================================================================
EX_manwon <- exp(mu_ln + sigma_ln^2 / 2)
EX_won    <- EX_manwon * 10000

N_school <- 21162                    # 전체 학교 수
lambda_freq <- n / (5 * N_school)    # 평균사고빈도

premium_b <- lambda_freq * EX_won
cat("\n[b] E(X) =", EX_won, "원,  평균사고빈도 =", lambda_freq,
    ",  적정보험료 =", premium_b, "원/교/년\n")

## =====================================================================
## c) 보상한도 B (1억/5억/10억) 별 적정보험료 -- a)의 최적합 2개 모형 사용
## =====================================================================
f_loglogis <- function(x, mu, sigma) {
  z <- (log(x) - mu) / sigma
  exp(z) / (1 + exp(z))^2 / (sigma * x)
}
F_loglogis <- function(x, mu, sigma) 1 / (1 + exp(-(log(x) - mu) / sigma))

f_pareto <- function(x, lambda, alpha) alpha * lambda^alpha * (lambda + x)^(-alpha - 1)
F_pareto <- function(x, lambda, alpha) 1 - (lambda / (lambda + x))^alpha

f_frechet <- function(x, c, tau) c * tau * exp(-c / x^tau) / x^(tau + 1)
F_frechet <- function(x, c, tau) exp(-c / x^tau)

f_weibull <- function(x, c, tau) c * tau * x^(tau - 1) * exp(-c * x^tau)
F_weibull <- function(x, c, tau) 1 - exp(-c * x^tau)

EY_capped <- function(B_manwon, dist) {
  if (dist == "LogLogistic") {
    integrand <- function(x) x * f_loglogis(x, mu_ll, sigma_ll)
    Fb <- F_loglogis(B_manwon, mu_ll, sigma_ll)
  } else if (dist == "Pareto") {
    integrand <- function(x) x * f_pareto(x, lambda_pa, alpha_pa)
    Fb <- F_pareto(B_manwon, lambda_pa, alpha_pa)
  } else if (dist == "Frechet") {
    integrand <- function(x) x * f_frechet(x, c_fr, tau_fr)
    Fb <- F_frechet(B_manwon, c_fr, tau_fr)
  } else if (dist == "Weibull") {
    integrand <- function(x) x * f_weibull(x, c_wb, tau_wb)
    Fb <- F_weibull(B_manwon, c_wb, tau_wb)
  }
  integ <- integrate(integrand, lower = 1e-6, upper = B_manwon, subdivisions = 1000)$value
  integ + B_manwon * (1 - Fb)     # E[Y], 만원 단위
}

limits_won <- c("1억" = 1e8, "5억" = 5e8, "10억" = 1e9)
cat("\n[c] 보상한도별 적정보험료 (최적합 2개 모형:", paste(best2, collapse=", "), ")\n")
for (nm in names(limits_won)) {
  B_manwon <- limits_won[nm] / 10000
  for (dist in best2) {
    EY_won  <- EY_capped(B_manwon, dist) * 10000
    premium <- lambda_freq * EY_won
    cat(sprintf("  한도=%-4s 모형=%-12s E[Y]=%12.0f원  보험료=%10.0f원/교/년\n",
                nm, dist, EY_won, premium))
  }
}

## =====================================================================
## d) 2008~2012년 실적 적용, 손해율
## =====================================================================
loss_ratio <- n_0812 / n
cat(sprintf("\n[d] 2008~2012 총사고건수=%d, 2013~2017 총사고건수=%d, 손해율=%.4f (%.1f%%)\n",
            n_0812, n, loss_ratio, loss_ratio * 100))
cat("   (E[Y]가 분자·분모에서 소거되어 보상한도·모형에 관계없이 손해율은 항상 동일)\n")
