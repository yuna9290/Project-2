# ============================================================
# Insurance Premium Lab v2
# Part 1: 상해보험 + Part 3: 학교화재보험
#
# 배포용 구조:
#   app.R
#   data/injury_data.csv
#   data/school_fire_data.csv   <- 실제 학교 원자료 변환본
#
# 학교 원자료가 아직 없으면:
#   Rscript prepare_school_data.R
# 로 변환하여 data/school_fire_data.csv를 만든 뒤 배포하세요.
# ============================================================

library(readxl)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)

options(scipen = 999)

# ------------------------------------------------------------
# 1. Theme
# ------------------------------------------------------------
COL <- list(
  navy = "#16324F", blue = "#2878B5", green = "#2E8B57",
  orange = "#E69F00", red = "#C94C4C", purple = "#7A5AA6",
  light = "#F5F7FA", gray = "#6B7280"
)

models_choices <- c(
  "Pareto" = "PA",
  "Frechet" = "FR",
  "Weibull" = "WE",
  "Log-normal" = "LN",
  "Log-logistic" = "LL"
)

model_label <- function(code) unname(models_choices[code])

# ------------------------------------------------------------
# 2. Deployment-safe data paths
# ------------------------------------------------------------
data_dir <- file.path(getwd(), "data")
injury_path <- file.path(data_dir, "injury_data.xls")
school_path <- file.path(data_dir, "school_fire_data.csv")

# Injury data -------------------------------------------------
if (file.exists(injury_path)) {
  injury_data <- read_excel(injury_path, sheet=1, range='A1:B51') 
  names(injury_data)[1:2] <- c("x", "count") 
  injury_data %>%
    transmute(x = as.numeric(x), count = as.numeric(count))
} else {
  injury_data <- data.frame(
    x = seq(1, 51, by = 1),
    count = c(
      51,150,218,300,320,305,307,222,159,158,
      172,136,113,91,83,95,69,66,87,76,
      124,61,62,64,66,71,73,66,60,54,
      105,51,56,40,54,57,51,58,45,58,
      70,44,49,48,46,38,34,42,42,35
    )
  )
}

INJURY_N <- 8443
INJURY_CONTRACTS <- 271306
INJURY_FREQ <- INJURY_N / INJURY_CONTRACTS
INJURY_HIGH_50 <- 1570
INJURY_HIGH_OVER_50 <- 1971
INJURY_TOTAL_PAID_10K <- sum((injury_data$x-0.5) * injury_data$count) +
  50 * (INJURY_HIGH_50 + INJURY_HIGH_OVER_50)

# School data -------------------------------------------------
school_data_available <- file.exists(school_path)

if (school_data_available) {
  school_raw <- read_csv(school_path, show_col_types = FALSE)
  school_raw$count <- as.numeric(school_raw$count)
  school_raw$damage_upper_manwon <- ifelse(
    toupper(as.character(school_raw$damage_upper_manwon)) %in% c("INF", "INFINITY"),
    Inf,
    as.numeric(school_raw$damage_upper_manwon)
  )
  school_raw$year <- as.integer(school_raw$year)
} else {
  school_raw <- data.frame(
    year = integer(0),
    damage_upper_manwon = numeric(0),
    count = numeric(0),
    is_open = logical(0)
  )
}

SCHOOL_COUNT_DEFAULT <- 21162

# ------------------------------------------------------------
# 3. Common grouped-data model fitting
# ------------------------------------------------------------
fit_lognorm <- function(df, n) {
  d <- df %>% mutate(p = cumsum(count) / (n + 1), theo = qnorm(p), samp = log(x))
  fit <- lm(samp ~ theo, data = d)
  list(model="LN", label="Log-normal", fit=fit, data=d,
       r2=summary(fit)$r.squared, mu=unname(coef(fit)[1]), sig=unname(coef(fit)[2]))
}

fit_weibull <- function(df, n) {
  d <- df %>% mutate(p = cumsum(count) / (n + 1), theo = log(-log(1-p)), samp = log(x))
  fit <- lm(samp ~ theo, data = d)
  mu <- unname(coef(fit)[1]); sig <- unname(coef(fit)[2])
  list(model="WE", label="Weibull", fit=fit, data=d, r2=summary(fit)$r.squared,
       c=exp(-mu/sig), tau=1/sig)
}

fit_frechet <- function(df, n) {
  d <- df %>% mutate(p = cumsum(count) / (n + 1), theo = -log(-log(p)), samp = log(x))
  fit <- lm(samp ~ theo, data = d)
  mu <- unname(coef(fit)[1]); slope <- unname(coef(fit)[2]); sig <- slope
  list(model="FR", label="Frechet", fit=fit, data=d, r2=summary(fit)$r.squared,
       c=exp(mu/sig), tau=1/sig)
}

fit_loglogis <- function(df, n) {
  d <- df %>% mutate(p = cumsum(count) / (n + 1), theo = log(p/(1-p)), samp = log(x))
  fit <- lm(samp ~ theo, data = d)
  mu <- unname(coef(fit)[1]); sig <- unname(coef(fit)[2])
  list(model="LL", label="Log-logistic", fit=fit, data=d, r2=summary(fit)$r.squared,
       alpha=1/sig, lambda=exp(mu))
}

# Pareto: use continuous lambda optimization for BOTH injury and school data.
# The objective is to maximize the Q-Q regression R^2 over lambda > 0.
fit_pareto <- function(df, n, interval = c(log(0.01), log(1e7))) {
  p <- cumsum(df$count) / (n + 1)
  z <- -log(1-p)

  objective <- function(loglambda) {
    lam <- exp(loglambda)
    y <- log1p(df$x / lam)
    -summary(lm(y ~ z + 0))$r.squared
  }

  opt <- optimize(objective, interval = interval)
  lambda_hat <- exp(opt$minimum)

  samp <- log1p(df$x / lambda_hat)
  fit <- lm(samp ~ z + 0)

  # Continuous curve around the optimum for visualization.
  lambda_grid <- exp(seq(
    log(max(0.01, lambda_hat / 10)),
    log(lambda_hat * 10),
    length.out = 250
  ))
  lambda_r2 <- vapply(lambda_grid, function(lambda) {
    y <- log1p(df$x / lambda)
    summary(lm(y ~ z + 0))$r.squared
  }, numeric(1))

  list(
    model="PA",
    label="Pareto",
    fit=fit,
    data=df %>% mutate(p=p, theo=z, samp=samp),
    r2=summary(fit)$r.squared,
    alpha=1/unname(coef(fit)[1]),
    lambda=lambda_hat,
    lambda_grid=lambda_grid,
    lambda_r2=lambda_r2
  )
}

fit_models_part1 <- function(df, n) {
  list(PA=fit_pareto(df,n), FR=fit_frechet(df,n), WE=fit_weibull(df,n),
       LN=fit_lognorm(df,n), LL=fit_loglogis(df,n))
}

# School insurance uses the same continuous Pareto optimization.
fit_pareto_school <- function(df, n) {
  fit_pareto(df, n)
}

fit_models_school <- function(df, n) {
  list(PA=fit_pareto_school(df,n), FR=fit_frechet(df,n), WE=fit_weibull(df,n),
       LN=fit_lognorm(df,n), LL=fit_loglogis(df,n))
}

# ------------------------------------------------------------
# 4. PDF / CDF and expected payment
# ------------------------------------------------------------
pdf_cdf <- function(model, p) {
  switch(model,
    PA=list(pdf=function(x) p$alpha*p$lambda^p$alpha*(p$lambda+x)^(-p$alpha-1),
            cdf=function(x) 1-(p$lambda/(p$lambda+x))^p$alpha),
    FR=list(pdf=function(x) p$c*p$tau*exp(-p$c/x^p$tau)/x^(p$tau+1),
            cdf=function(x) exp(-p$c/x^p$tau)),
    WE=list(pdf=function(x) p$c*p$tau*x^(p$tau-1)*exp(-p$c*x^p$tau),
            cdf=function(x) 1-exp(-p$c*x^p$tau)),
    LN=list(pdf=function(x) (1/(x*p$sig*sqrt(2*pi)))*exp(-0.5*((log(x)-p$mu)/p$sig)^2),
            cdf=function(x) pnorm((log(x)-p$mu)/p$sig)),
    LL=list(pdf=function(x) p$alpha*x^(p$alpha-1)*p$lambda^(-p$alpha)/(1+(x/p$lambda)^p$alpha)^2,
            cdf=function(x) 1-1/(1+(x/p$lambda)^p$alpha))
  )
}

# Distribution-specific mean E[X].
# Used when B = Inf (no policy limit).
distribution_mean <- function(model, params) {
  switch(model,
    PA = {
      if (params$alpha <= 1) Inf else params$lambda / (params$alpha - 1)
    },
    FR = {
      if (params$tau <= 1) Inf else
        params$c^(1 / params$tau) * gamma(1 - 1 / params$tau)
    },
    WE = {
      params$c^(-1 / params$tau) * gamma(1 + 1 / params$tau)
    },
    LN = {
      exp(params$mu + 0.5 * params$sig^2)
    },
    LL = {
      if (params$alpha <= 1) Inf else
        params$lambda * (pi / params$alpha) / sin(pi / params$alpha)
    }
  )
}

# Expected insurer payment:
# Y = min((X-A)+, B)
# If B = Inf, Y = (X-A)+ and the infinite-limit case is handled explicitly.
expected_payment <- function(model, params, A, B) {
  fns <- pdf_cdf(model, params)
  f <- fns$pdf
  F <- fns$cdf

  # No policy limit: B = Inf
  if (is.infinite(B)) {
    EX <- distribution_mean(model, params)

    # If E[X] is infinite, E[(X-A)+] is also infinite.
    if (!is.finite(EX)) return(Inf)

    if (A <= 0) return(EX)

    lower_integral <- tryCatch(
      integrate(
        function(x) x * f(x),
        lower = 1e-8,
        upper = A,
        subdivisions = 1000,
        rel.tol = 1e-8
      )$value,
      error = function(e) NA_real_
    )

    if (is.na(lower_integral)) return(NA_real_)

    return(
      EX -
        lower_integral -
        A * (1 - F(A))
    )
  }

  # Finite policy limit.
  val <- tryCatch(
    integrate(
      function(x) x*f(x),
      lower=max(A, 1e-8),
      upper=A+B,
      subdivisions=1000,
      rel.tol=1e-8
    )$value,
    error=function(e) NA_real_
  )

  if (is.na(val)) return(NA_real_)

  val + B*(1-F(A+B)) - A*(F(A+B)-F(A))
}

params_from_model <- function(code, z) {
  switch(code,
    PA=list(alpha=z$alpha, lambda=z$lambda),
    FR=list(c=z$c, tau=z$tau),
    WE=list(c=z$c, tau=z$tau),
    LN=list(mu=z$mu, sig=z$sig),
    LL=list(alpha=z$alpha, lambda=z$lambda)
  )
}

# ------------------------------------------------------------
# 5. School data aggregation
# ------------------------------------------------------------
fire_grouped <- function(years) {
  validate(need(school_data_available, "학교화재 데이터가 없습니다. data/school_fire_data.csv를 추가하세요."))
  d <- school_raw %>% filter(year %in% years)
  finite <- d %>% filter(is.infinite(damage_upper_manwon) == FALSE) %>%
    group_by(damage_upper_manwon) %>% summarise(count=sum(count, na.rm=TRUE), .groups="drop") %>%
    arrange(damage_upper_manwon) %>% transmute(x=damage_upper_manwon, count=count)
  open_count <- d %>% filter(is.infinite(damage_upper_manwon)) %>% summarise(count=sum(count, na.rm=TRUE)) %>% pull(count)
  n <- sum(finite$count, na.rm=TRUE) + open_count
  list(data=finite, open_count=open_count, n=n, raw=d)
}

# Optional total-loss field support if the CSV contains it.
school_total_loss <- function(years) {
  if (!school_data_available) return(NA_real_)
  if ("total_loss_10k" %in% names(school_raw)) {
    return(sum(as.numeric(school_raw$total_loss_10k[school_raw$year %in% years]), na.rm=TRUE))
  }
  # Fallback: grouped upper-bound proxy. This is NOT exact actual loss.
  d <- school_raw %>% filter(year %in% years)
  sum(ifelse(is.infinite(d$damage_upper_manwon), 100000, d$damage_upper_manwon) * d$count, na.rm=TRUE)
}

# ------------------------------------------------------------
# 6. UI
# ------------------------------------------------------------
ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title="Insurance Premium Lab v2"),
  dashboardSidebar(
    width=300,
    sidebarMenu(
      menuItem("보험료 계산", tabName="dashboard", icon=icon("calculator")),
      menuItem("모형 비교", tabName="models", icon=icon("chart-line")),
      menuItem("보험료 시나리오", tabName="scenarios", icon=icon("table")),
      menuItem("원자료", tabName="data", icon=icon("database"))
    ),
    hr(),
    div(style="padding:0 15px 15px 15px;",
      h4("보험 및 계약 조건", style="color:white;"),
      radioButtons("insurance", "보험 종류",
                   choices=c("상해보험"="injury", "학교화재보험"="fire"),
                   selected="injury"),
      conditionalPanel("input.insurance == 'injury'",
        numericInput("A", "자기부담금 A (만원)", 0, min=0, max=10000, step=5),

        conditionalPanel("input.unlimited_injury == false",
          numericInput("B", "보상한도 B (만원)", 50, min=1, max=100000, step=50)
        ),
        checkboxInput("unlimited_injury", "보상한도 없음 (∞)", FALSE)
      ),
      conditionalPanel("input.insurance == 'fire'",
        numericInput("A_fire", "자기부담금 A (만원)", 0, min=0, max=100000, step=100),

        conditionalPanel("input.unlimited_fire == false",
          numericInput("B_fire", "보상한도 B (만원)", 10000, min=1, max=1000000, step=1000)
        ),
        checkboxInput("unlimited_fire", "보상한도 없음 (∞)", FALSE),
        numericInput("school_count", "전체 학교 수", SCHOOL_COUNT_DEFAULT, min=1, step=1),
        helpText("기본값은 사용자가 제공한 Part 3 코드의 21,162교입니다.", style="color:#ddd;"),
        uiOutput("fire_data_status")
      ),
      selectInput("dist", "현재 화면에서 사용할 분포", choices=models_choices, selected="PA"),
      actionButton("goButton", "계산 실행", icon=icon("play"), class="btn-success btn-block")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(paste0(
      "body{background:",COL$light,";}",
      ".content-wrapper{background:",COL$light,";}",
      ".small-box{border-radius:12px;}",
      ".box{border-radius:12px;box-shadow:0 2px 8px rgba(0,0,0,.06);border-top:0;}",
      ".box-header{font-weight:700;}",
      ".btn-success{background:",COL$green,";border-color:",COL$green,";}",
      ".metric-note{color:",COL$gray,";font-size:12px;}",
      ".section-title{font-size:18px;font-weight:700;color:",COL$navy,";}",
      ".callout-info{background:#eef6fb;border-left-color:",COL$blue,";}",
      ".callout-warning{background:#fff8e8;border-left-color:",COL$orange,";}",
      ".tab-pane{padding-top:5px;}")
    ))),
    tabItems(
      tabItem("dashboard",
        fluidRow(valueBoxOutput("premium_box",3), valueBoxOutput("r2_box",3),
                 valueBoxOutput("p1_box",3), valueBoxOutput("p2_box",3)),
        fluidRow(
          box(width=8,title="손해액 분포와 적합 PDF",status="primary",solidHeader=TRUE,
              plotOutput("hist_plot",height=430)),
          box(width=4,title="계약 / 모형 요약",status="primary",solidHeader=TRUE,
              uiOutput("summary_ui"))
        ),
        fluidRow(box(width=12,title="보험료 계산 구조",status="info",solidHeader=TRUE,htmlOutput("formula_ui"))),
        conditionalPanel("input.insurance == 'fire'",
          fluidRow(
            box(width=6,title="학교화재 Part 3(b): Log-normal 평균피해액",status="success",solidHeader=TRUE,uiOutput("fire_mean_ui")),
            box(width=6,title="학교화재 Part 3(d) 주의사항",status="warning",solidHeader=TRUE,uiOutput("fire_loss_note"))
          )
        )
      ),
      tabItem("models",
        fluidRow(
          box(width=7,title="5개 후보분포 Q-Q Plot",status="primary",solidHeader=TRUE,
              selectInput("qq_model","분포 선택",choices=models_choices,selected="PA"),
              plotOutput("qq_plot",height=420)),
          box(width=5,title="모형 적합도 비교",status="primary",solidHeader=TRUE,
              DTOutput("model_table"),br(),plotOutput("r2_plot",height=260))
        ),
        fluidRow(box(width=12,title="Pareto λ 연속 최적화",status="warning",solidHeader=TRUE,
                     plotOutput("lambda_plot",height=300),
                     p("상해보험과 학교화재보험 모두 λ를 연속적인 실수 범위에서 최적화합니다.",class="metric-note")))
      ),
      tabItem("scenarios",
        fluidRow(box(width=12,title="보험료 시나리오",status="success",solidHeader=TRUE,
                     DTOutput("scenario_table"))),
        fluidRow(box(width=12,title="경험 손해율",status="info",solidHeader=TRUE,
                     DTOutput("loss_ratio_table"),
                     p("학교화재보험은 원자료에 정확한 연도별 총손해액이 포함된 경우 실제 손해율을 계산합니다. 그렇지 않으면 그룹 상한을 이용한 근사값을 표시합니다.",class="metric-note")))
      ),
      tabItem("data",
        fluidRow(box(width=12,title="현재 선택 보험의 원자료",status="primary",solidHeader=TRUE,DTOutput("raw_data_table")))
      )
    )
  )
)

selected_param_info <- function(code, model_obj) {
  switch(code,
    PA = list(
      labels=c("α (shape)", "λ (scale)"),
      values=c(model_obj$alpha, model_obj$lambda)
    ),
    FR = list(
      labels=c("c", "τ"),
      values=c(model_obj$c, model_obj$tau)
    ),
    WE = list(
      labels=c("c", "τ"),
      values=c(model_obj$c, model_obj$tau)
    ),
    LN = list(
      labels=c("μ", "σ"),
      values=c(model_obj$mu, model_obj$sig)
    ),
    LL = list(
      labels=c("α (shape)", "λ (scale)"),
      values=c(model_obj$alpha, model_obj$lambda)
    )
  )
}

# ------------------------------------------------------------
# 7. Server
# ------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$insurance, {
    if (input$insurance == "injury") {
      updateSelectInput(session,"dist",selected="PA")
      updateNumericInput(session,"B",value=50)
      updateCheckboxInput(session,"unlimited_injury",value=FALSE)
      updateCheckboxInput(session,"unlimited_fire",value=FALSE)
    } else {
      updateSelectInput(session,"dist",selected="LN")
      updateNumericInput(session,"B_fire",value=10000)
      updateCheckboxInput(session,"unlimited_fire",value=FALSE)
      updateCheckboxInput(session,"unlimited_injury",value=FALSE)
    }
  }, ignoreInit=TRUE)

  output$fire_data_status <- renderUI({
    if (school_data_available) {
      div(style="color:#b9f6ca;", icon("check-circle"), " 학교화재 CSV 연결됨")
    } else {
      div(style="color:#ffccbc;", icon("exclamation-triangle"), " school_fire_data.csv 없음")
    }
  })

  calc <- eventReactive(input$goButton, {
    if (input$insurance == "injury") {
      A <- input$A; B <- if (isTRUE(input$unlimited_injury)) Inf else input$B; n <- INJURY_N; freq <- INJURY_FREQ
      df <- injury_data
      models <- fit_models_part1(df,n)
      selected <- models[[input$dist]]
      params <- params_from_model(input$dist,selected)
      prem_10k <- freq * expected_payment(input$dist,params,A,B)
      sorted <- sort(sapply(models,function(z) z$r2),decreasing=TRUE)
      return(list(type="injury",A=A,B=B,n=n,freq=freq,df=df,models=models,
                  selected=selected,params=params,premium_10k=prem_10k,sorted=sorted,
                  school=NULL))
    }

    validate(need(school_data_available,"학교화재 데이터가 없습니다. 먼저 data/school_fire_data.csv를 추가하세요."))
    A <- input$A_fire; B <- if (isTRUE(input$unlimited_fire)) Inf else input$B_fire
    g <- fire_grouped(2013:2017)
    freq <- g$n / (5 * input$school_count)
    models <- fit_models_school(g$data,g$n)
    selected <- models[[input$dist]]
    params <- params_from_model(input$dist,selected)
    prem_10k <- freq * expected_payment(input$dist,params,A,B)
    sorted <- sort(sapply(models,function(z) z$r2),decreasing=TRUE)
    list(type="fire",A=A,B=B,n=g$n,freq=freq,df=g$data,models=models,
         selected=selected,params=params,premium_10k=prem_10k,sorted=sorted,
         school=list(g13=g,g08=fire_grouped(2008:2012),freq=freq,
                     total_loss_1317=school_total_loss(2013:2017),
                     total_loss_0812=school_total_loss(2008:2012),
                     school_count=input$school_count))
  })

  output$premium_box <- renderValueBox({
    req(calc()); z <- calc()
    valueBox(ifelse(is.finite(z$premium_10k),paste0(format(round(z$premium_10k*10000),big.mark=","),"원"),"∞"),
             "적정보험료",icon=icon("won-sign"),color="green")
  })

  output$r2_box <- renderValueBox({
    req(calc()); z <- calc()
    valueBox(sprintf("%.4f",z$selected$r2),paste0("R² - ",z$selected$label),icon=icon("bullseye"),color="blue")
  })

  output$p1_box <- renderValueBox({
    req(calc()); z <- calc()
    info <- selected_param_info(input$dist, z$selected)
    valueBox(
      sprintf("%.5f", info$values[1]),
      paste0(z$selected$label, " · ", info$labels[1]),
      icon=icon("sliders-h"), color="yellow"
    )
  })

  output$p2_box <- renderValueBox({
    req(calc()); z <- calc()
    info <- selected_param_info(input$dist, z$selected)
    valueBox(
      sprintf("%.5f", info$values[2]),
      paste0(z$selected$label, " · ", info$labels[2]),
      icon=icon("sliders-h"), color="aqua"
    )
  })

  output$hist_plot <- renderPlot({
    req(calc())
    z <- calc()
    m <- z$selected
    fns <- pdf_cdf(input$dist, z$params)

    if (z$type == "fire") {
      # School grouped data spans several orders of magnitude.
      # A categorical x-axis makes every damage class equally visible.
      finite_df <- z$df %>%
        mutate(
          label = format(x, big.mark=",", scientific=FALSE, trim=TRUE)
        )

      if (!is.null(z$school) && is.finite(z$school$g13$open_count)) {
        finite_df <- bind_rows(
          finite_df,
          data.frame(
            x=Inf,
            count=z$school$g13$open_count,
            label="10억원 이상"
          )
        )
      }

      finite_df$label <- factor(
        finite_df$label,
        levels=finite_df$label
      )

      ggplot(finite_df, aes(x=label, y=count)) +
        geom_col(fill=COL$green, width=.72, alpha=.9) +
        labs(
          title=paste0(m$label, " · 학교화재 피해액 그룹분포"),
          subtitle="실제 피해액 구간별 건수 (10억원 이상은 개방구간)",
          x="피해액 상한 / 개방구간",
          y="화재건수"
        ) +
        theme_minimal(base_size=13) +
        theme(
          axis.text.x=element_text(angle=30, hjust=1),
          panel.grid.major.x=element_blank()
        )
    } else {
      grid_max <- if (is.infinite(z$B)) max(150, z$A * 1.2 + 150) else max(150, z$A+z$B)
      grid <- seq(max(0.01, min(z$df$x)/100), grid_max, length.out=1000)
      curve <- data.frame(x=grid,pdf=fns$pdf(grid)*z$n)

      ggplot(z$df,aes(x=x,y=count)) +
        geom_col(fill=COL$green,width=0.9,alpha=.85) +
        geom_line(data=curve,aes(x=x,y=pdf),color=COL$orange,linewidth=1.1) +
        geom_vline(xintercept=z$A,linetype="dashed",color=COL$red) +
        {
          if (is.infinite(z$B)) NULL else
            geom_vline(xintercept=z$A+z$B,linetype="dashed",color=COL$blue)
        } +
        labs(
          title=paste0(m$label," 적합 결과"),
          subtitle="막대: 경험자료 / 주황색: 적합 PDF",
          x="사고금액 X (만원)",
          y="건수"
        ) +
        theme_minimal(base_size=13)
    }
  })

  output$summary_ui <- renderUI({
    req(calc())
    z <- calc()
    p <- z$params
    m <- z$selected
    info <- selected_param_info(input$dist, m)

    b_text <- if (is.infinite(z$B)) "∞ (무한 보상한도)" else paste0(z$B, "만원")

    tagList(
      div(class="section-title",
          if(z$type=="injury") "상해보험 계약" else "학교화재보험 계약"),
      tags$p(paste0("자기부담금 A = ",z$A,"만원")),
      tags$p(paste0("보상한도 B = ",b_text)),
      hr(),
      div(class="section-title","현재 선택 모형"),
      tags$p(strong(m$label)),
      tags$p(paste0(info$labels[1]," = ",round(info$values[1],6))),
      tags$p(paste0(info$labels[2]," = ",round(info$values[2],6))),
      tags$p(sprintf("R² = %.6f",m$r2)),
      hr(),
      div(class="section-title","보험료 계산"),
      tags$p(sprintf("E(N) = %.8f",z$freq)),
      tags$p(sprintf(
        "E[Y] = %s 만원",
        if (is.finite(expected_payment(input$dist,p,z$A,z$B)))
          format(round(expected_payment(input$dist,p,z$A,z$B),4),big.mark=",")
        else "∞"
      )),
      tags$p(
        if(is.finite(z$premium_10k))
          sprintf("Premium = %s 원",
                  format(round(z$premium_10k*10000),big.mark=","))
        else "Premium = ∞"
      )
    )
  })

  output$formula_ui <- renderUI({
    z <- if (isolate(input$goButton)>0) try(calc(),silent=TRUE) else NULL
    freq_txt <- if(!inherits(z,"try-error") && !is.null(z)) sprintf("%.8f",z$freq) else "E(N)"
    b_txt <- if(!inherits(z,"try-error") && !is.null(z) && is.infinite(z$B)) "∞" else "A+B"
    HTML(paste0(
      "<div style='font-size:16px;line-height:2;'>",
      "<b>실제 보험금:</b> Y = min((X − A)<sup>+</sup>, B)<br>",
      "<b>평균사고심도:</b> E(Y) = ∫<sub>A</sub><sup>A+B</sup>xf(x)dx − A[F(A+B)−F(A)] + B[1−F(A+B)]",
      if (b_txt == "∞") "<br><span style='color:#C94C4C;'><b>B = ∞:</b> E(Y) = E[(X−A)<sup>+</sup>]</span>" else "",
      "<br><b>평균사고빈도:</b> E(N) = ",freq_txt,
      "<br><b>적정보험료:</b> Premium = E(N) × E(Y)",
      "</div>"
    ))
  })

  output$fire_mean_ui <- renderUI({
    req(calc()); z <- calc(); validate(need(z$type=="fire","학교화재보험을 선택하세요."))
    ln <- z$models$LN
    EX <- exp(ln$mu + ln$sig^2/2)
    tagList(tags$p(paste0("Log-normal μ = ",round(ln$mu,5),", σ = ",round(ln$sig,5))),
            tags$p(paste0("E(X) = exp(μ + σ²/2) = ",format(round(EX,4),big.mark=","),"만원")),
            tags$p(paste0("평균피해액 = ",format(round(EX*10000),big.mark=","),"원")),
            tags$p(paste0("평균사고빈도 = ",round(z$freq,8))),
            tags$p(paste0(
              "적정보험료 (A=0, B=∞): ",
              ifelse(is.finite(z$freq*EX*10000),
                     paste0(format(round(z$freq*EX*10000),big.mark=","),"원"),
                     "∞")
            )))
  })

  output$fire_loss_note <- renderUI({
    req(calc()); z <- calc()
    if(z$type!="fire") return(tags$p("학교화재보험을 선택하면 Part 3 정보를 표시합니다."))
    tags$div(class="callout callout-warning",style="padding:12px;",
      tags$b("Part 3(d) 주의:"),
      tags$p("원자료에 연도별 total_loss_10k 열이 있으면 실제 손해율을 계산합니다."),
      tags$p("그 열이 없으면 10억 이상 구간을 10억원으로 놓는 그룹상한 근사치를 표시하므로, 과제 제출용 실제 손해율과 구분해야 합니다."))
  })

  output$qq_plot <- renderPlot({
    req(calc()); m <- calc()$models[[input$qq_model]]; d <- m$data
    xlab <- switch(input$qq_model,PA="-log(1-p)",FR="-log(-log(p))",WE="log(-log(1-p))",LN="qnorm(p)",LL="log(p/(1-p))")
    ggplot(d,aes(theo,samp)) + geom_point(size=2,color=COL$navy) +
      geom_smooth(method="lm",se=FALSE,color=COL$orange,linewidth=1.1,
                  formula=if(input$qq_model=="PA") y~x+0 else y~x) +
      labs(title=paste0(m$label," Q-Q Plot"),subtitle=paste0("R² = ",round(m$r2,5)),x=xlab,
           y=if(input$qq_model=="PA")"log(1+x/λ)" else "log(x)") + theme_minimal(base_size=13)
  })

  output$model_table <- renderDT({
    req(calc()); models <- calc()$models; codes <- names(sort(sapply(models,function(z)z$r2),decreasing=TRUE))
    out <- lapply(seq_along(codes),function(i){code<-codes[i];z<-models[[code]];data.frame(
      순위=i,모형=z$label,R2=z$r2,
      모수1=switch(code,PA=paste0("α = ",round(z$alpha,4)),FR=paste0("c = ",round(z$c,4)),WE=paste0("c = ",round(z$c,4)),LN=paste0("μ = ",round(z$mu,4)),LL=paste0("α = ",round(z$alpha,4))),
      모수2=switch(code,PA=paste0("λ = ",round(z$lambda,4)),FR=paste0("τ = ",round(z$tau,4)),WE=paste0("τ = ",round(z$tau,4)),LN=paste0("σ = ",round(z$sig,4)),LL=paste0("λ = ",round(z$lambda,4)))
    )}) |> bind_rows()
    datatable(out,rownames=FALSE,options=list(dom='t',pageLength=5),colnames=c('순위','모형','R²','모수 1','모수 2')) %>% formatRound('R2',5)
  })

  output$r2_plot <- renderPlot({
    req(calc()); models<-calc()$models; d<-data.frame(Model=sapply(models,`[[`,"label"),R2=sapply(models,`[[`,"r2"))%>%arrange(R2)
    ggplot(d,aes(reorder(Model,R2),R2))+geom_col(fill=COL$blue)+coord_flip()+geom_text(aes(label=sprintf('%.4f',R2)),hjust=-.05)+ylim(0,1)+labs(x=NULL,y='R²')+theme_minimal(base_size=12)
  })

  output$lambda_plot <- renderPlot({
    req(calc())
    pa <- calc()$models$PA
    d <- data.frame(lambda=pa$lambda_grid,R2=pa$lambda_r2)

    ggplot(d,aes(lambda,R2)) +
      geom_line(color=COL$blue, linewidth=1) +
      geom_vline(xintercept=pa$lambda,linetype='dashed',color=COL$red) +
      geom_point(
        data=data.frame(lambda=pa$lambda,R2=pa$r2),
        color=COL$red,size=3
      ) +
      scale_x_log10() +
      labs(
        x='λ (log scale)',
        y='Q-Q regression R²',
        title=paste0(calc()$selected$label, " / 연속 λ 최적화"),
        subtitle=paste0("최적 λ = ", signif(pa$lambda,6),
                        " · 현재 보험종류: ",
                        ifelse(calc()$type=="injury","상해보험","학교화재보험"))
      ) +
      theme_minimal()
  })

  # Scenario table -------------------------------------------
  output$scenario_table <- renderDT({
    req(calc())
    z<-calc()
    codes<-names(z$sorted)[1:2]

    if(z$type=="injury"){
      grid<-expand.grid(
        A=c(0,10,20),
        B=c(50,100,200,500,1000,Inf)
      )
    } else {
      grid<-data.frame(
        A=0,
        B=c(10000,50000,100000,Inf)
      )
    }

    for(code in codes){
      m<-z$models[[code]]
      p<-params_from_model(code,m)
      grid[[m$label]]<-mapply(
        function(a,b)
          expected_payment(code,p,a,b)*z$freq*10000,
        grid$A,grid$B
      )
    }

    names(grid)[1:2]<-c('자기부담금 A (만원)','보상한도 B (만원)')
    grid$`보상한도 B (만원)` <-
      ifelse(is.infinite(grid$`보상한도 B (만원)`),
             "∞ (무제한)",
             as.character(grid$`보상한도 B (만원)`))

    datatable(
      grid,
      rownames=FALSE,
      options=list(pageLength=20,dom='t')
    ) %>%
      formatCurrency(
        3:ncol(grid),
        currency='₩',
        interval=3,
        mark=',',
        digits=0
      )
  })

  # Loss ratio ------------------------------------------------
  output$loss_ratio_table <- renderDT({
    req(calc()); z<-calc()
    if(z$type=="injury"){
      codes<-names(z$sorted)[1:2]
      premiums<-c(3130,sapply(codes,function(code){m<-z$models[[code]];p<-params_from_model(code,m);expected_payment(code,p,0,50)*z$freq*10000}))
      rows<-data.frame(모형=c('기존 보험료',sapply(codes,function(c)model_label(c))),보험료원=premiums)
      rows$손해율퍼센트<-INJURY_TOTAL_PAID_10K*10000/(INJURY_CONTRACTS*rows$보험료원)*100
      datatable(rows,rownames=FALSE,options=list(dom='t'))%>%formatCurrency('보험료원',currency='₩',interval=3,mark=',',digits=0)%>%formatRound('손해율퍼센트',2)
    } else {
      validate(need(z$school!=NULL,"학교 데이터가 없습니다."))
      codes <- names(z$sorted)[1:2]
      limits <- c(`1억원`=10000, `5억원`=50000, `10억원`=100000)
      total_loss_won <- z$school$total_loss_0812 * 10000

      rows <- list()
      k <- 1
      for (code in codes) {
        m <- z$models[[code]]
        p <- params_from_model(code, m)
        for (nm in names(limits)) {
          B <- unname(limits[[nm]])
          premium_school <- expected_payment(code, p, 0, B) * z$freq * 10000
          total_premium <- premium_school * z$school$school_count * 5
          lr <- if (is.finite(premium_school) && premium_school > 0) total_loss_won / total_premium * 100 else NA_real_
          rows[[k]] <- data.frame(
            모형 = model_label(code),
            보상한도 = nm,
            `2013-2017 적정보험료(원/교/년)` = premium_school,
            `2008-2012 손해액(원)` = total_loss_won,
            `2008-2012 손해율(%)` = lr,
            check.names = FALSE
          )
          k <- k + 1
        }
      }
      out <- bind_rows(rows)
      datatable(out, rownames=FALSE, options=list(dom='t', pageLength=10)) %>%
        formatCurrency('2013-2017 적정보험료(원/교/년)', currency='₩', interval=3, mark=',', digits=0) %>%
        formatCurrency('2008-2012 손해액(원)', currency='₩', interval=3, mark=',', digits=0) %>%
        formatRound('2008-2012 손해율(%)', 2)
    }
  })

  output$raw_data_table <- renderDT({
    if(input$insurance=="injury"){
      d<-injury_data;d$누적건수<-cumsum(d$count);d$누적비율<-d$누적건수/INJURY_N
      datatable(d,rownames=FALSE,options=list(pageLength=15),colnames=c('사고금액 X (만원)','건수','누적건수','누적비율'))%>%formatPercentage('누적비율',2)
    } else {
      validate(need(school_data_available,'data/school_fire_data.csv가 없습니다.'))
      datatable(school_raw,rownames=FALSE,options=list(pageLength=20))
    }
  })
}

shinyApp(ui,server)
