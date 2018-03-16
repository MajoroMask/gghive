# plot function
gghive_plot <- function(
    lp, method = "rmw", 
    plot_point = TRUE, plot_bezier = TRUE, plot_axis = TRUE, 
    panel_grid_major = NULL, panel_grid_minor = NULL, 
    panel_grid_major_label = NULL, panel_grid_minor_label = NULL, 
    ...
) {
    # arguments
    if (is.null(panel_grid_major) & !is.null(lp$df_grid_major)) {
        panel_grid_major <- TRUE
    } else {
        panel_grid_major <- FALSE
    }
    if (is.null(panel_grid_minor) & !is.null(lp$df_grid_minor)) {
        panel_grid_minor <- TRUE
    } else {
        panel_grid_minor <- FALSE
    }
    if (is.null(panel_grid_major_label) & !is.null(lp$df_grid_major_label)) {
        panel_grid_major_label <- TRUE
    } else {
        panel_grid_major_label <- FALSE
    }
    if (is.null(panel_grid_minor_label) & !is.null(lp$df_grid_minor_label)) {
        panel_grid_minor_label <- TRUE
    } else {
        panel_grid_minor_label <- FALSE
    }
    
    # plot
    p <- ggplot()
    if (panel_grid_major) {  # major grid line
        p <- p + 
            geom_path(
                aes(.x, .y, group = .group), lp$df_grid_major, 
                color = alpha("black", 0.4), size = 0.6, linetype = 2, 
                show.legend = FALSE
            )
    }
    if (panel_grid_minor) {  # minor grid line
        p <- p + 
            geom_path(
                aes(.x, .y, group = .group), lp$df_grid_minor,
                color = alpha("black", 0.2), size = 0.5, linetype = 2, 
                show.legend = FALSE
            )
    }
    if (panel_grid_major_label) {
        p <- p + 
            geom_label(  # major grid label
                aes(.x, .y, label = .label), lp$df_grid_major_label, size = 2, 
                color = alpha("black", 0.5), 
                label.padding = unit(0.15, "lines"), label.size = 0, 
                show.legend = FALSE
            )
    }
    if (panel_grid_minor_label) {
        p <- p + 
            geom_label(  # minor grid label
                aes(.x, .y, label = .label), lp$df_grid_minor_label, size = 2, 
                color = alpha("black", 0.3), 
                label.padding = unit(0.15, "lines"), label.size = 0, 
                show.legend = FALSE
            )
    }
    if (plot_axis) {  # axis line
        p <- p + 
            geom_segment(
                aes(.x, .y, xend = .xend, yend = .yend), lp$df_axis, 
                show.legend = FALSE
            )
    }
    p <- p + 
        geom_bezier(  # edge line
            aes(.coord_x, .coord_y, group = .id, alpha = .e_size), 
            lp$df_bezier, show.legend = FALSE
        )
    if (plot_point) {
        p <- p + 
            geom_point(
                aes(.x, .y, color = as.factor(.axis_ori)), lp$dfv, 
                show.legend = FALSE
            )
    }
    p <- p + 
        theme_hive() +
        coord_fixed()
}
