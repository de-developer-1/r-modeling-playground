# 必要なパッケージをインストール（初回のみ）
packages <- c("ggplot2", "plotly", "gt", "factoextra", "dplyr")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# ライブラリ読み込み
library(ggplot2)
library(plotly)
library(gt)
library(factoextra)
library(dplyr)

# ---- データ生成 ----
set.seed(123)
df <- data.frame(
  height = rnorm(100, mean = 170, sd = 10),
  weight = rnorm(100, mean = 65, sd = 15),
  age    = rnorm(100, mean = 40, sd = 12),
  income = rnorm(100, mean = 50000, sd = 15000)
)

# ---- 主成分分析 ----
df_scaled <- scale(df)  # 標準化
pca <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# ---- 固有値・寄与率の表形式表示 ----
summary_df <- summary(pca)$importance
pca_table <- data.frame(
  PC = rownames(t(summary_df)),
  t(summary_df)
)

# gtパッケージで表示
pca_table %>%
  gt() %>%
  tab_header(
    title = "主成分分析の結果",
    subtitle = "固有値・寄与率・累積寄与率"
  ) %>%
  fmt_number(columns = -1, decimals = 3)

# ---- 寄与率の棒グラフ ----
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) +
  ggtitle("主成分ごとの寄与率 (%)")

# ---- 主成分バイプロット ----
fviz_pca_biplot(pca, repel = TRUE) +
  ggtitle("主成分バイプロット")

# ---- 3次元プロット（Plotly使用） ----
pca_df <- as.data.frame(pca$x)
plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3,
        type = "scatter3d", mode = "markers",
        marker = list(size = 4)) %>%
  layout(title = "主成分空間での3次元プロット")
