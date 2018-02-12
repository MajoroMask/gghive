# function ----
library(grid)
library(ggplot2)

ggname <- function(prefix, grob) {
    grob$name <- grobName(grob, prefix)
    grob
}
geom_bezier <- function(
    mapping = NULL, data = NULL,
    stat = "identity", position = "identity",
    ...,
    arrow = NULL,
    lineend = "butt",
    show.legend = NA,
    inherit.aes = TRUE
) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomBezier,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            arrow = arrow,
            lineend = lineend,
            ...
        )
    )
}
GeomBezier <- ggproto(
    "GeomBezier", GeomSegment,
    required_aes = c("x", "y"),
    non_missing_aes = c("linetype", "size", "shape"),
    default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

    draw_panel = function(
        data, panel_params, coord, arrow = NULL, lineend
    ) {
        if (!coord$is_linear()) {
            warning(
                "geom_curve is not implemented for non-linear coordinates",
                call. = FALSE
            )
        }
        munched <- coord_munch(coord, data, panel_params)
        munched <- munched[order(munched$group), ]
        first_idx <- !duplicated(munched$group)
        first_rows <- munched[first_idx, ]
        ggname(
            "geom_bezier",
            grid::bezierGrob(
                munched$x, munched$y,
                default.units = "native",
                id = munched$group,
                gp = grid::gpar(
                    col = alpha(first_rows$colour, first_rows$alpha),
                    lwd = first_rows$size * .pt,
                    lty = first_rows$linetype,
                    lineend = lineend
                ),
                arrow = arrow
            )
        )
    },

    draw_key = draw_key_path
)

# test ----
# df <- data.frame(
#     x = c(0, 2, -0.5, 2.5),
#     y = c(0, 0, 3, 3),
#     group = rep(1, each = 4) %>% factor()
# )
# ggplot(df) +
#     geom_bezier(aes(x, y, group = group), size = 2) +
#     theme_bw()
