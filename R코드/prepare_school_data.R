# =====================================================================
# Convert 2008-2017 학교화재보험 XLSX -> deployment-ready CSV
# =====================================================================
# 필요 패키지: readxl, dplyr, readr

library(readxl)
library(dplyr)
library(readr)

xlsx_path <- file.choose()
raw <- read_excel(xlsx_path, col_names = FALSE) |> as.data.frame()

# 헤더 행 자동 탐지
hdr_pos <- which(apply(raw, c(1,2), function(v) grepl('피해액', as.character(v))), arr.ind = TRUE)
stopifnot(nrow(hdr_pos) >= 1)
hdr_row <- hdr_pos[1, 'row']
label_col <- hdr_pos[1, 'col']

# 연도 열 탐지
# 헤더행에 숫자로 들어 있는 2008~2017 열을 찾음
year_vals <- suppressWarnings(as.numeric(as.character(unlist(raw[hdr_row, ]))))
years <- 2008:2017
year_cols <- setNames(lapply(years, function(y) which(year_vals == y)[1]), years)
stopifnot(all(!is.na(unlist(year_cols))))

# 금액 구간 행 자동 탐지
labels_chr <- as.character(raw[, label_col])
labels_num <- suppressWarnings(as.numeric(labels_chr))
finite_bins <- c(50, 100, 1000, 5000, 10000, 100000)
bin_rows <- sapply(finite_bins, function(v) which(labels_num == v)[1])
open_row <- which(grepl('10억', labels_chr))[1]
stopifnot(!any(is.na(bin_rows)), !is.na(open_row))

out <- list()
for (y in years) {
  col <- year_cols[[as.character(y)]]
  vals <- suppressWarnings(as.numeric(raw[, col]))
  freq <- vals[bin_rows]
  open_freq <- vals[open_row]
  tmp <- data.frame(
    year = y,
    damage_upper_manwon = c(finite_bins, Inf),
    count = c(freq, open_freq),
    is_open = c(rep(FALSE, length(finite_bins)), TRUE)
  )
  out[[as.character(y)]] <- tmp
}

school <- bind_rows(out)
write_csv(school, file.path(xlsx_path,'/data/school_fire_data.csv'))
cat('Created: data/school_fire_data.csv\n')
