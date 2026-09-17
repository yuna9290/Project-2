
# 이론통계학2 – Project #2

3 조  242STG28 황수연 252STG26 이휘민 262STG01 강재서 262STG09 조유나

## Overview

본 프로젝트에서는 경험자료를 이용하여 손해액(Loss)의 확률분포를 추정하고,
자기부담금(Deductible)과 보상한도(Limit)에 따른 적정 손해보험료를
계산하였다.

또한 Q-Q plot과 회귀분석을 이용하여 손해액에 적합한 확률분포를 선정하고,
수치적분 및 MLE 등의 통계적 방법을 적용하여 보험료를 산출하였다.

분석 대상은 다음과 같다.

- 상해보험 사고 치료비
- 특수건물(아파트) 화재 피해액
- 학교 화재 피해액
- 손해보험료 계산 R Shiny Application

각 자료에 대해 손해액의 분포를 분석하고,
적합한 확률분포의 모수를 추정한 후
자기부담금과 보상한도에 따른 적정 보험료를 계산하였다.

## Project Structure

### Part 1 – 상해보험

상해보험 사고 치료비 자료를 이용하여 손해액 분포를 분석하였다.

- 50만원 미만 사고 치료비 Histogram
- Q-Q plot을 이용한 분포 적합도 비교
- Lognormal / Pareto / Weibull / Frechet / Log-logistic 비교
- Pareto 및 Frechet 모수 추정
- 자기부담금 및 보상한도에 따른 적정 보험료 계산
- 기존 보험료와 추정 보험료의 손해율 비교
- 자기부담금으로 인한 절단자료(Truncated/Censored Data) 분석
- Pareto / Frechet MLE를 이용한 미관측 사고건수 및 전체 사고건수 추정

### Part 2 – 특수건물(아파트) 화재보험

한국화재보험협회의 특수건물 화재현황 자료를 이용하여
아파트 화재 피해액의 분포와 보험료를 분석하였다.

- 아파트 화재 피해액 분포 분석
- Q-Q plot을 이용한 적합분포 선정
- 선택된 2개 분포의 모수 추정
- 자기부담금 및 보상한도에 따른 적정 보험료 계산
- 무한 보상한도(B = ∞)에서의 적정 보험료 계산
- 경험자료를 이용한 보험료와 모형 기반 보험료 비교
- 최근 연도별 손해율 분석
- 실제 손해율과 추정 손해율 비교

### Part 3 – 학교 화재보험

국가화재정보시스템의 학교 화재 피해액 자료를 이용하여
학교 화재보험의 적정 보험료를 산정하였다.

- 학교 화재 피해액의 그룹화 자료 분석
- Log-Normal / Log-Logistic / Log-Laplace /
  Log-Gumbel(Frechet) / Log-Exponential(Pareto) 비교
- Q-Q plot을 이용한 최적분포 2개 선정
- 선택된 분포의 모수 추정
- Log-Normal 모형을 이용한 평균 피해액 추정
- 평균 사고빈도 및 평균 사고심도 계산
- 보상한도 1억원 / 5억원 / 10억원에 따른 보험료 계산
- 과거 자료에 적용한 손해율 분석
- 결과의 의미와 문제점 및 개선방안 검토

### Part 4 – 손해보험료 계산 Application

R Shiny를 이용하여 사용자가 자기부담금과 보상한도를
입력하면 자동으로 적정 보험료를 계산할 수 있는
웹 애플리케이션을 구현하였다.

## Methods

| Model / Method | Description | Formula |
|---|---|---|
| Lognormal | 로그 변환된 손해액이 정규분포를 따른다고 가정 | $\log X \sim N(\mu,\sigma^2)$ |
| Pareto | 오른쪽 꼬리가 긴 손해액 분포를 표현하는 모형 | $F(x)=1-\left(\frac{\lambda}{\lambda+x}\right)^\alpha$ |
| Weibull | 손해액의 비대칭적 분포를 표현하는 모형 | $F(x)=1-\exp\left[-\left(\frac{x}{\lambda}\right)^\alpha\right]$ |
| Frechet | 역와이블(Inverse Weibull) 분포로 오른쪽 꼬리가 긴 손해액 자료에 적용 | $F(x)=\exp\left[-\left(\frac{\lambda}{x}\right)^\alpha\right]$ |
| Log-logistic | 로그 변환된 손해액의 Logistic 구조를 이용한 분포 | $F(x)=\frac{1}{1+\left(\frac{\lambda}{x}\right)^\alpha}$ |


## Data

### Part 1
- 상해보험 사고 치료비 자료
- 계급화된 사고건수 자료
- 계약건수 및 사고건수 자료

### Part 2
- 한국화재보험협회 특수건물 화재현황 자료
- 아파트 화재 피해액 및 보험 관련 자료

### Part 3
- 국가화재정보시스템 학교 화재현황 자료
- 2013–2017년 학교 화재 피해액 자료
- 학교 수 및 연도별 화재 발생 자료


## Authors

- 조유나 – <Part 1>
- 황수연 – <Part 2>
- 강재서 – <Part 3>
- 이휘민 – <Part 4>
