library(magrittr)
library(igraph)
library(grid)
library(ggplot2)

set.seed(123)
g <- sample_grg(100, 0.1, torus=FALSE)
df_edges <- g %>% as_data_frame(what = "edges")
set.seed(123)
df_edges$.alpha <- runif(nrow(df_edges), 3, 5)
df_vertices <- g %>% as_data_frame(what = "vertices")
df_vertices$id <- rownames(df_vertices)
df_vertices$degree <- g %>% degree()
df_vertices$betweenness <- g %>% betweenness()
df_vertices$closeness <- g %>% closeness()
df_vertices$cc <- g %>% transitivity(type = "local")
df_vertices$branching <- g %>% neighborhood.size()

lp <- gghive(  # short for list_plot
    df_edges, df_vertices, 
    # axis_normalize = TRUE, 
    # axis_rank = TRUE, 
    v_x = "cc", 
    v_y = "degree",
    # v_y = "betweenness",
    # v_y = "closeness",
    # v_y = "cc",
    # v_y = "branching",
    # e_size = "size", 
    bezier_jit_in = 0.4, axis_jit = 0.1, 
    what = "place_holder"
)
p <- gghive_plot(lp)
ggsave("test.pdf", p, scale = 2)
ggsave("test.png", p, scale = 2)
p_d3 <- d3_render(lp = lp)

