# 必要パッケージ
packages <- c("smacof", "ggplot2", "gt", "dplyr", "ggrepel")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# ライブラリ読み込み
library(smacof)
library(ggplot2)
library(gt)
library(dplyr)
library(ggrepel)

# ---- サンプルデータ作成（ブランドごとの属性スコア） ----
set.seed(123)
brands <- c("BrandA", "BrandB", "BrandC", "BrandD", "BrandE", "BrandF")
df <- data.frame(
  品質       = c(8, 6, 7, 5, 6, 9),
  デザイン   = c(7, 5, 8, 6, 4, 8),
  コスパ     = c(5, 9, 6, 8, 9, 4),
  信頼性     = c(9, 6, 8, 5, 6, 9),
  アフターサービス = c(8, 5, 7, 4, 5, 8)
)
rownames(df) <- brands

# ---- 距離行列（ユークリッド距離） ----
dist_mat <- dist(df, method = "euclidean")

# ---- 古典的MDS（cmdscale） ----
mds_res <- cmdscale(dist_mat, k = 2, eig = TRUE)

mds_df <- data.frame(
  ブランド = rownames(df),
  Dim1 = mds_res$points[, 1],
  Dim2 = mds_res$points[, 2]
)

# ---- 散布図で可視化 ----
ggplot(mds_df, aes(x = Dim1, y = Dim2, label = ブランド)) +
  geom_point(size = 4, color = "steelblue") +
  geom_text_repel(size = 5) +
  labs(title = "多次元尺度法（MDS）によるブランド配置図", x = "次元1", y = "次元2") +
  theme_minimal()

# ---- 距離行列をgtで表形式表示 ----
as.matrix(dist_mat) %>%
  round(2) %>%
  as.data.frame() %>%
  mutate(ブランド = rownames(.)) %>%
  relocate(ブランド) %>%
  gt() %>%
  tab_header(
    title = "ブランド間のユークリッド距離（基礎データ）"
  )

# ---- smacof による非線形MDS（拡張） ----
mds_nonlinear <- mds(dist_mat, type = "interval")  # 距離の順序情報のみ使用

mds_nl_df <- data.frame(
  ブランド = rownames(df),
  Dim1 = mds_nonlinear$conf[, 1],
  Dim2 = mds_nonlinear$conf[, 2]
)

# ---- 非線形MDSもプロット比較 ----
ggplot(mds_nl_df, aes(x = Dim1, y = Dim2, label = ブランド)) +
  geom_point(size = 4, color = "darkgreen") +
  geom_text_repel(size = 5) +
  labs(title = "非線形MDS（smacof）によるブランド配置図", x = "次元1", y = "次元2") +
  theme_minimal()