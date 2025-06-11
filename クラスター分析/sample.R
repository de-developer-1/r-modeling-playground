# パッケージのインストール
packages <- c("ggplot2", "dplyr", "gt", "factoextra", "cluster")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# ライブラリ読み込み
library(ggplot2)
library(dplyr)
library(gt)
library(factoextra)
library(cluster)

# ---- サンプルデータ作成（顧客の年齢・年収） ----
set.seed(123)
n <- 150
customer_data <- data.frame(
  年齢 = c(rnorm(n/3, 25, 3), rnorm(n/3, 45, 4), rnorm(n/3, 60, 5)),
  年収 = c(rnorm(n/3, 300, 30), rnorm(n/3, 600, 50), rnorm(n/3, 900, 70))
)

# ---- k-meansクラスタリング（k=3） ----
k_result <- kmeans(customer_data, centers = 3, nstart = 25)
customer_data$クラスタ <- as.factor(k_result$cluster)

# ---- クラスタリング結果の可視化 ----
fviz_cluster(k_result, data = customer_data, geom = "point",
             ellipse.type = "norm", palette = "jco",
             main = "k-meansクラスタリング結果", repel = TRUE) +
  theme_minimal()

# ---- 各クラスタの重心をテーブル表示 ----
centers_df <- as.data.frame(k_result$centers)
centers_df$クラスタ <- paste0("Cluster", 1:nrow(centers_df))

centers_df %>%
  relocate(クラスタ) %>%
  gt() %>%
  tab_header(title = "各クラスタの中心値（重心）") %>%
  fmt_number(columns = where(is.numeric), decimals = 1)

# ---- 各クラスタのサイズ（人数） ----
cluster_sizes <- data.frame(
  クラスタ = names(k_result$size),
  人数 = as.integer(k_result$size)
)

cluster_sizes %>%
  gt() %>%
  tab_header(title = "各クラスタのサンプル数")

# ---- 階層的クラスタリング（オプション） ----
d <- dist(scale(customer_data[, 1:2]))
hc <- hclust(d, method = "ward.D2")

fviz_dend(hc, k = 3, rect = TRUE, main = "階層的クラスタリングのデンドログラム")