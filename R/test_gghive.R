library(magrittr)
library(igraph)
library(grid)
library(ggplot2)

df_ori <- "lemis.txt" %>% read.delim(stringsAsFactors = FALSE) %>%
    set_colnames(c("x", "y", "w"))
g <- df_ori %>% graph_from_data_frame
df_edges <- g %>% as_data_frame(what = "edges")
df_vertices <- g %>% as_data_frame(what = "vertices")
df_vertices$degree <- g %>% degree()
df_vertices$betweenness <- g %>% betweenness()
df_vertices$closeness <- g %>% closeness()
df_vertices$cc <- g %>% transitivity()
df_vertices$branching <- g %>% neighborhood.size()

lp <- gghive(  # short for list_plot
    df_edges, df_vertices, 
    # label_rel_pos = 135, 
    # axis_normalize = FALSE, axis_rank = TRUE, 
    # v_y = "degree",
    v_y = "betweenness",
    # v_y = "closeness", 
    # v_y = "cc", 
    # v_y = "branching", 
    e_size = "w", 
    bezier_jit_in = 0.4, 
    what = "place_holder"
)
p <- gghive_plot(lp)
p
