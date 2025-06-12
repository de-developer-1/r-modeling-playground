# 必要パッケージ
packages <- c("igraph", "tidygraph", "ggraph", "dplyr", "gt", "ggrepel")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# ライブラリ読み込み
library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)
library(gt)
library(ggrepel)

# ---- サンプルデータ：エッジリスト（人間関係） ----
edges <- data.frame(
  from = c("Alice", "Alice", "Bob", "Carol", "Dave", "Eve", "Frank", "Eve", "Grace", "Grace"),
  to   = c("Bob", "Carol", "Dave", "Eve", "Frank", "Grace", "Alice", "Bob", "Alice", "Frank")
)

# ---- igraphオブジェクト作成 ----
graph <- graph_from_data_frame(edges, directed = FALSE)

# ---- ネットワーク指標計算 ----
metrics <- data.frame(
  name = V(graph)$name,
  degree = degree(graph),
  betweenness = betweenness(graph),
  closeness = closeness(graph)
)

# ---- 指標を表形式で整形・表示 ----
metrics %>%
  arrange(desc(degree)) %>%
  gt() %>%
  tab_header(title = "ネットワーク指標（中心性）") %>%
  fmt_number(columns = c("betweenness", "closeness"), decimals = 3)

# ---- tidygraph + ggraph による可視化 ----
tbl_graph <- as_tbl_graph(graph) %>%
  mutate(
    degree = centrality_degree(),
    label_size = scales::rescale(degree, to = c(3, 8))
  )

# ネットワーク図（Fruchterman-Reingold レイアウト）
set.seed(123)
ggraph(tbl_graph, layout = "fr") +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(size = degree), color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  labs(title = "ネットワーク可視化（Fruchterman-Reingold Layout）") +
  theme_graph()
