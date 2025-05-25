# 必要なパッケージ
packages <- c("psych", "GPArotation", "corrplot", "gt", "dplyr", "ggplot2")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# ライブラリ読み込み
library(psych)
library(GPArotation)
library(corrplot)
library(gt)
library(dplyr)
library(ggplot2)

# ---- サンプルデータ作成 ----
set.seed(42)
df <- data.frame(
  verbal     = rnorm(100, 10, 2),
  math       = rnorm(100, 10, 2),
  logic      = rnorm(100, 10, 2),
  spatial    = rnorm(100, 10, 2),
  memory     = rnorm(100, 10, 2),
  attention  = rnorm(100, 10, 2)
)
df$logic     <- df$math * 0.6 + rnorm(100, 0, 1)
df$spatial   <- df$memory * 0.5 + rnorm(100, 0, 1)
df$attention <- df$memory * 0.4 + rnorm(100, 0, 1)

# ---- 相関行列の可視化 ----
corrplot(cor(df), method = "circle", type = "upper", tl.col = "black", title = "相関行列")

# ---- 平行分析で因子数決定 ----
fa.parallel(df, fa = "fa", n.iter = 100, show.legend = FALSE, main = "因子数の決定（平行分析）")

# ---- 因子分析（斜交回転：oblimin） ----
fa_result_oblimin <- fa(df, nfactors = 2, rotate = "oblimin", fm = "ml")

# ---- 因子負荷量の表示 ----
loading_df <- as.data.frame(unclass(fa_result_oblimin$loadings))
loading_df <- loading_df %>%
  mutate(変数 = rownames(loading_df)) %>%
  relocate(変数)

# 表形式で出力（gt）
loading_df %>%
  gt() %>%
  tab_header(
    title = "因子負荷量（斜交回転：oblimin）",
    subtitle = "2因子モデル"
  ) %>%
  fmt_number(columns = -1, decimals = 3)

# ---- 因子構造図（パス図） ----
fa.diagram(fa_result_oblimin, main = "因子構造図（斜交回転）")

# ---- 因子得点の散布図 ----
scores <- as.data.frame(fa_result_oblimin$scores)
colnames(scores) <- c("FA1", "FA2")  # 安定化のために明示的に命名

ggplot(scores, aes(x = FA1, y = FA2)) +
  geom_point(alpha = 0.7) +
  labs(title = "因子得点の散布図（斜交回転）", x = "因子1", y = "因子2") +
  theme_minimal()
