# plot function
ggplot() +
    geom_path(  # major grid line
        aes(x, y, group = group), lp$df_grid_major, 
        color = alpha("black", 0.4), size = 0.6, linetype = 2, 
        show.legend = FALSE
    ) +
    geom_path(  # minor grid line
        aes(x, y, group = group), lp$df_grid_minor,
        color = alpha("black", 0.2), size = 0.5, linetype = 2, 
        show.legend = FALSE
    ) +
    geom_label(  # grid label
        aes(x, y, label = label), lp$df_grid_major_label, size = 2, 
        color = alpha("black", 0.5), 
        show.legend = FALSE
    ) + 
    geom_label(  # grid label
        aes(x, y, label = label), lp$df_grid_minor_label, size = 2, 
        color = alpha("black", 0.3), 
        show.legend = FALSE
    ) + 
    geom_segment(  # axis line
        aes(x, y, xend = xend, yend = yend), lp$df_axis, 
        show.legend = FALSE
    ) +
    geom_bezier(  # edge line
        aes(coord_x, coord_y, group = id, alpha = e_size), lp$df_bezier
    ) + 
    geom_point(aes(x, y, color = as.factor(axis_ori)), lp$dfv) +
    # scale_x_continuous(
    #     breaks = df$x, labels = df$label, position = "top"
    # ) + 
    theme_hive() +
    coord_fixed()

ggsave("test.pdf", scale = 2)

# test plotly & htmlwidgets

# pp <- ggplotly(p, layerData = 4)
# saveWidget(pp, file = "test.html")

# test zone
