theme_hive <- function(base_size = 11, base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
    theme_grey(
        base_size = base_size,
        base_family = base_family,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
    ) %+replace%
        theme(
            # white background and dark border
            panel.background = element_rect(fill = "white", colour = NA),
            panel.border     = element_blank(),
            # make gridlines dark, same contrast with white as in theme_grey
            panel.grid       = element_blank(),
            # remove all axis element
            axis.text        = element_blank(), 
            axis.title       = element_blank(), 
            axis.ticks       = element_blank(), 
            # match legend key to background
            legend.key       = element_rect(fill = "white", colour = NA),
            
            complete = TRUE
        )
}
