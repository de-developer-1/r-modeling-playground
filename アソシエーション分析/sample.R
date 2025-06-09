# パッケージのインストール
packages <- c("arules", "arulesViz", "gt", "dplyr")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# ライブラリ読み込み
library(arules)
library(arulesViz)
library(gt)
library(dplyr)

# ---- サンプルデータ作成（取引） ----
# 各行が1取引、各列が商品（TRUE = 購入）
set.seed(123)
items <- c("パン", "牛乳", "ビール", "おむつ", "チーズ", "卵", "シリアル")
n_transactions <- 100
transactions_df <- as.data.frame(matrix(sample(c(TRUE, FALSE), n_transactions * length(items), replace = TRUE, prob = c(0.2, 0.8)), 
                                        nrow = n_transactions, ncol = length(items)))
colnames(transactions_df) <- items

# transactions型に変換
trans <- as(transactions_df, "transactions")

# ---- アソシエーションルールの抽出 ----
rules <- apriori(trans, parameter = list(supp = 0.1, conf = 0.5, target = "rules"))

# ---- 信頼度でソートして上位5件を表示 ----
top_rules <- sort(rules, by = "confidence", decreasing = TRUE)[1:5]

# ---- gt形式でルール表示 ----
rules_df <- as(top_rules, "data.frame")
rules_df <- rules_df %>%
  mutate(
    lhs = gsub("[\\{\\}]", "", sub("=>.*", "", rules)),
    rhs = gsub("[\\{\\}]", "", sub(".*=>", "", rules))
  ) %>%
  select(lhs, rhs, support, confidence, lift)

rules_df %>%
  gt() %>%
  tab_header(title = "上位アソシエーションルール（信頼度順）") %>%
  fmt_number(columns = c("support", "confidence", "lift"), decimals = 3)

# ---- ネットワークプロット ----
plot(top_rules, method = "graph", engine = "igraph", shading = "confidence")

# ---- マトリクスプロット（オプション） ----
plot(rules, method = "grouped", control = list(k = 15))

