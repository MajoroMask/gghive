library(ggplot2)
library(magrittr)
library(igraph)

# functions ----
generate_plot_data <- function(
    df_e, df_v,
    directed = TRUE, method = "rmw",
    e_size = "w",
    v_y = "degree",
    n_axis = NULL, axis_dup = NULL, axis_jit = 0.05, y_expand = c(0.1, 0),
    axis_normalize = FALSE, axis_rank = FALSE, bezier_jit = 0.1, ...
) {
    require(magrittr)
    require(igraph)
    
    dfv <- data.frame(
        rownames(df_v),
        df_v[, v_y, drop = FALSE],
        row.names = rownames(df_v), stringsAsFactors = FALSE
    )
    colnames(dfv) <- c("name", "y_ori")
    
    dfe <- cbind(df_e[, c(1, 2)], df_e[, e_size, drop = FALSE])
    colnames(dfe) <- c("from", "to", "e_size")
    
    if (identical(method, "rmw")) {  # regulator-manager-workhorse
        if (is.null(n_axis)) n_axis <- 3L
        if (is.null(axis_dup)) axis_dup <- 2L
        dfv$axis_ori <- 0L
        t1 <- rownames(dfv) %in% dfe$from
        t2 <- rownames(dfv) %in% dfe$to
        dfv$axis_ori[t1  & !t2] <- 1L
        dfv$axis_ori[t1  &  t2] <- 2L
        dfv$axis_ori[!t1 &  t2] <- 3L
        # dfv <- dfv[]
    } else if (FALSE) {
        # TODO more methods here
    }
    dfv_ori <- dfv
    dfv <- duplicate_row(dfv, axis_dup = axis_dup, axis_jit = axis_jit)
    # dfe <- recode_point(dfe, dfv)
    dfe <- duplicate_axis(
        dfe, dfv_ori, dfv, axis_dup = axis_dup, axis_jit = axis_jit
    )
    dfv <- coord_transfer(
        dfv, return_axis = FALSE,
        n_axis = n_axis, y_expand = y_expand,
        axis_normalize = axis_normalize, axis_rank = axis_rank
    )
    df_axis <- coord_transfer(
        dfv, return_axis = TRUE,
        n_axis = n_axis, y_expand = y_expand,
        axis_normalize = axis_normalize, axis_rank = axis_rank
    )
    df_bezier <- bezier_transfer(dfe, dfv, n_axis = n_axis, bezier_jit = bezier_jit)
    
    ggplot() +
        geom_segment(aes(x, y, xend = xend, yend = yend), df_axis) +
        geom_point(aes(x, y), dfv) +
        # geom_point(aes(x, y), df, color = "blue") +
        geom_bezier(
            aes(coord_x, coord_y, group = id, alpha = e_size), df_bezier, 
            # alpha = I(0.2)
        ) + 
        # geom_text(aes(x, y, label = name), dfv, nudge_x = 1, nudge_y = 0) + 
        theme_bw()
}
duplicate_row <- function(dfv, axis_dup, axis_jit) {
    dfv$axis <- dfv$axis_ori
    t1 <- dfv$axis %in% axis_dup
    dfv_keep <- dfv[!t1, ]
    dfv_keep$axis_stat <- "keep"
    rownames(dfv_keep) <- paste0(rownames(dfv_keep), "_keep")
    dfv_left <- dfv[t1, ]
    dfv_left$axis <- dfv_left$axis - axis_jit
    dfv_left$axis_stat <- "from"
    rownames(dfv_left) <- paste0(rownames(dfv_left), "_from")
    dfv_right <- dfv[t1, ]
    dfv_right$axis <- dfv_right$axis + axis_jit
    dfv_right$axis_stat <- "to"
    rownames(dfv_right) <- paste0(rownames(dfv_right), "_to")
    rbind(dfv_keep, dfv_left, dfv_right)
}
duplicate_axis <- function(dfe, dfv_ori, dfv, axis_dup, axis_jit) {
    x <- dfv_ori[dfe$from, "axis_ori"]
    y <- dfv_ori[dfe$to, "axis_ori"]
    if (any(x > y)) stop("Shit happened @ duplicate axis")
    x_mod <- x
    y_mod <- y
    
    t1 <- x %in% axis_dup  # x need jit
    t2 <- y %in% axis_dup  # y need jit
    t3 <- x != y  # different axes
    t4 <- (y - x) / n_axis >= 1 - (y - x) / n_axis  # forward half is bigger
    t5 <- (y - x) / n_axis <= 1 - (y - x) / n_axis  # reverse half is bigger
    
    x_mod[t1 & t3 &  t4] <- x_mod[t1 & t3 &  t4] - axis_jit
    x_mod[t1 & t3 & !t4] <- x_mod[t1 & t3 & !t4] + axis_jit
    y_mod[t2 & t3 &  t5] <- y_mod[t2 & t3 &  t5] - axis_jit
    y_mod[t2 & t3 & !t5] <- y_mod[t2 & t3 & !t5] + axis_jit
    x_mod[t1 & !t3] <- x_mod[t1 & !t3] - axis_jit
    y_mod[t2 & !t3] <- y_mod[t2 & !t3] + axis_jit
    dfe$axis_ori_from <- x
    dfe$axis_ori_to <- y
    dfe$axis_from <- x_mod
    dfe$axis_to <- y_mod
    danteng <- function(str, axis) {
        rownames(dfv)[dfv$name == str & dfv$axis == axis]
    }
    dfe$from_fix <- mapply(danteng, str = dfe$from, axis = dfe$axis_from)
    dfe$to_fix <- mapply(danteng, str = dfe$to, axis = dfe$axis_to)
    dfe
}
coord_transfer <- function(
    dfv, return_axis = FALSE,
    n_axis, y_expand, axis_normalize, axis_rank
) {
    x <- dfv$axis
    x_fctr <- factor(x, levels = unique(x))
    y <- dfv$y_ori
    if (axis_rank) {
        y <- unlist(by(y, x_fctr, order))
    }
    if (axis_normalize) {
        norm_num <- function(x) {x * (x - min(x)) / (max(x) - min(x))}
        y <- unlist(by(y, x_fctr, norm_num))
    }
    y <- y + (max(y) - min(y)) * y_expand[1] + y_expand[2]
    y_min <- min(y)
    y_max <- as.numeric(by(y, x_fctr, max))
    
    if (return_axis) {
        rel_x <- (unique(x) - 1) / n_axis
        df <- data.frame(
            x = sin(2 * pi * rel_x) * y_min,
            y = cos(2 * pi * rel_x) * y_min,
            xend = sin(2 * pi * rel_x) * y_max,
            yend = cos(2 * pi * rel_x) * y_max,
            stringsAsFactors = FALSE
        )
        rownames(df) <- unique(x)
        df
    } else {
        dfv$y_mod <- y
        dfv$x <- sin(2 * pi * (x - 1) / n_axis) * y
        dfv$y <- cos(2 * pi * (x - 1) / n_axis) * y
        dfv
    }
}
bezier_transfer <- function(dfe, dfv, n_axis, bezier_jit) {
    
    df <- data.frame(
        id = factor(1:nrow(dfe)), 
        x1 = dfv[dfe$from_fix, "x"],
        x2 = 0, 
        x3 = 0, 
        x4 = dfv[dfe$to_fix, "x"],
        y1 = dfv[dfe$from_fix, "y"],
        y2 = 0, 
        y3 = 0, 
        y4 = dfv[dfe$to_fix, "y"],
        stringsAsFactors = FALSE
    )
    t1 <- dfe$axis_ori_from == 1 & dfe$axis_ori_to == 3
    
    df_sub <- dfe[t1, ]
    xa <- dfv[df_sub$from_fix, "axis"]
    xb <- dfv[df_sub$to_fix, "axis"]
    ya <- dfv[df_sub$from_fix, "y_mod"]
    yb <- dfv[df_sub$to_fix, "y_mod"]
    df$x2[t1] <- sin(2 * pi * ((xa - (xa + n_axis - xb) * bezier_jit) - 1) / n_axis) * ya
    df$y2[t1] <- cos(2 * pi * ((xa - (xa + n_axis - xb) * bezier_jit) - 1) / n_axis) * ya
    df$x3[t1] <- sin(2 * pi * ((xb + (xa + n_axis - xb) * bezier_jit) - 1) / n_axis) * yb
    df$y3[t1] <- cos(2 * pi * ((xb + (xa + n_axis - xb) * bezier_jit) - 1) / n_axis) * yb
    
    df_sub <- dfe[!t1, ]
    xa <- dfv[df_sub$from_fix, "axis"]
    xb <- dfv[df_sub$to_fix, "axis"]
    ya <- dfv[df_sub$from_fix, "y_mod"]
    yb <- dfv[df_sub$to_fix, "y_mod"]
    df$x2[!t1] <- sin(2 * pi * ((xa + abs(xa - xb) * bezier_jit) - 1) / n_axis) * ya
    df$y2[!t1] <- cos(2 * pi * ((xa + abs(xa - xb) * bezier_jit) - 1) / n_axis) * ya
    df$x3[!t1] <- sin(2 * pi * ((xb - abs(xa - xb) * bezier_jit) - 1) / n_axis) * yb
    df$y3[!t1] <- cos(2 * pi * ((xb - abs(xa - xb) * bezier_jit) - 1) / n_axis) * yb
    
    df_x <- reshape2::melt(
        cbind(dfe, df[, c("id", "x1", "x2", "x3", "x4")]), 
        measure.vars = c("x1", "x2", "x3", "x4"), 
        variable.name = "x", value.name = "coord_x"
    )
    df_y <- reshape2::melt(
        df[, c("y1", "y2", "y3", "y4")], 
        measure.vars = c("y1", "y2", "y3", "y4"), 
        variable.name = "y", value.name = "coord_y"
    )
    cbind(df_x, df_y)
}
# main ----
df_ori <- "lemis.txt" %>% read.delim(stringsAsFactors = FALSE) %>%
    set_colnames(c("x", "y", "w"))
g <- df_ori %>% graph_from_data_frame
df_edges <- g %>% as_data_frame(what = "edges")
df_vertices <- g %>% as_data_frame(what = "vertices")
df_vertices$degree <- g %>% degree()
df_vertices$betweenness <- g %>% betweenness()
df_vertices$closeness <- g %>% closeness()
p <- generate_plot_data(df_edges, df_vertices)



