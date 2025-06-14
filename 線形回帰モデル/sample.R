# --- 必要パッケージ ---
packages <- c("ggplot2", "dplyr", "broom", "gt", "performance", "see", "modelsummary")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# --- ライブラリ読み込み ---
library(ggplot2)
library(dplyr)
library(broom)
library(gt)
library(performance)
library(see)
library(modelsummary)

# --- 架空の不動産データ（価格を面積や部屋数で予測） ---
set.seed(123)
n <- 100
sample_data <- data.frame(
  price = rnorm(n, mean = 3000, sd = 500),
  area = rnorm(n, mean = 70, sd = 10),
  rooms = sample(1:4, n, replace = TRUE)
)

# --- 線形回帰モデルの構築 ---
model <- lm(price ~ area + rooms, data = sample_data)

# --- 回帰係数の表示（modelsummary） ---
modelsummary(model, output = "gt",
             statistic = "({std.error})", stars = TRUE,
             title = "重回帰モデルの回帰係数")

# --- 残差の診断とモデル指標（performance + see） ---
check_model(model)  # 多様な診断図を自動表示（残差・正規性・線形性など）

# --- 散布図 + 回帰線（面積 vs 価格） ---
ggplot(sample_data, aes(x = area, y = price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(title = "面積と価格の関係", x = "面積 (㎡)", y = "価格 (万円)") +
  theme_minimal()

# --- 回帰係数をbroomで整形しgtで表示 ---
tidy(model, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  gt() %>%
  tab_header(title = "回帰係数と信頼区間") %>%
  fmt_number(columns = where(is.numeric), decimals = 2) %>%
  cols_label(
    term = "変数",
    estimate = "係数",
    conf.low = "95% CI下限",
    conf.high = "95% CI上限",
    p.value = "p値"
  )