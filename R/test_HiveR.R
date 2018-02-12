library(magrittr)
library(igraph)
library(HiveR)


test4 <- ranHiveData(nx = 4)
set.seed(123)
test2 <- ranHiveData(nx = 2)
test6 %>% plotHive(ch= 50, bkgnd = scales::alpha("black", 0))
test4_3d <- ranHiveData(type = "3D", nx = 4)
sumHPD(test4)
plotHive(test4)
plot3dHive(test4_3d)



df_e <- "./_/real_edge_table.txt" %>% read.delim(stringsAsFactors = FALSE)
# df_v <- "./_/real_node_table.txt" %>% read.delim(stringsAsFactors = FALSE)

df_e <- "./_/lemis.txt" %>% read.delim(stringsAsFactors = FALSE)

# g <- graph_from_data_frame(df_e)

hpd <- df_e %>% edge2HPD(axis.cols = rep("black", 3))
# TODO my_edge2HPD function
hpd$nodes$axis<- hpd$nodes$id %in% hpd$edges$id1 + 
    hpd$nodes$id %in% hpd$edges$id2 + 
    1 %>% as.integer()

# hpd$nodes$radius <- g %>% betweenness() %>% as.numeric
hpd$nodes$radius <- g %>% degree() %>% as.numeric

# hpd$edges$weight <- 1


b <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point()

df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
b +
    # geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"), data = df) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df) +
    coord_polar()



