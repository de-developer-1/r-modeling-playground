# パッケージのインストール
packages <- c("survival", "survminer", "gt", "dplyr")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# ライブラリ読み込み
library(survival)
library(survminer)
library(gt)
library(dplyr)

# ---- 架空の生存データ作成 ----
set.seed(42)
n <- 100
sample_data <- data.frame(
  time = round(rexp(n, rate = 0.1), 1),  # 生存時間（exponential分布）
  status = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),  # 0 = 打ち切り, 1 = イベント発生
  group = sample(c("治療A", "治療B"), n, replace = TRUE)
)

# ---- モデル作成：Kaplan-Meier ----
surv_obj <- Surv(time = sample_data$time, event = sample_data$status)
km_fit <- survfit(surv_obj ~ group, data = sample_data)

# ---- 生存曲線のプロット（ggsurvplot） ----
ggsurvplot(
  km_fit,
  data = sample_data,
  pval = TRUE,
  risk.table = TRUE,
  conf.int = TRUE,
  surv.median.line = "hv",
  palette = c("#E69F00", "#56B4E9"),
  ggtheme = theme_minimal(),
  title = "生存曲線（Kaplan-Meier法）"
)

# ---- 生存率の要約表（中位生存時間など） ----
summary_df <- summary(km_fit)$table %>%
  as.data.frame() %>%
  tibble::rownames_to_column("群") %>%
  select(群, records, events, median, "0.95LCL", "0.95UCL")

# ---- 表を整形して出力 ----
summary_df %>%
  rename(
    登録数 = records,
    事象数 = events,
    中央生存時間 = median,
    下限95%CI = `0.95LCL`,
    上限95%CI = `0.95UCL`
  ) %>%
  gt() %>%
  tab_header(title = "生存曲線の統計要約（群別）") %>%
  fmt_number(columns = where(is.numeric), decimals = 1)
