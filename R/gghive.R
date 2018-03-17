# functions ----
gghive <- function(
    df_e, df_v,
    directed = TRUE, method = "rmw",
    v_x = NULL, v_y = NULL, 
    n_axis = NULL, axis_dup = NULL, axis_jit = 0.05, y_expand = 0.05, 
    axis_normalize = FALSE, axis_rank = FALSE, 
    bezier_jit_out = 0.1, bezier_jit_in = 0.2, 
    grid_cut = 360, label_rel_pos = 90, 
    ...
) {
    require(magrittr)
    require(igraph)
    
    # TODO more args checking
    
    if (is.null(label_rel_pos)) {
        label_rel_pos <- round(grid_cut / 4)
    } else if (label_rel_pos > grid_cut) {
        stop("label_rel_pos must not be larger than grid_cut")
    }
    
    dfv <- data.frame(
        rownames(df_v), 
        df_v[, v_y, drop = FALSE], 
        row.names = rownames(df_v), stringsAsFactors = FALSE
    )
    colnames(dfv) <- c(".name", ".y_ori")
    dfv <- cbind(df_v, dfv)
    
    dfe <- df_e[, c(1, 2)]
    colnames(dfe) <- c(".from", ".to")
    dfe <- cbind(df_e, dfe)
    
    if (identical(method, "rmw")) {  # regulator-manager-workhorse
        if (is.null(n_axis)) n_axis <- 3L
        if (is.null(axis_dup)) axis_dup <- 2L
        dfv$.axis_ori <- 0L
        t1 <- rownames(dfv) %in% dfe$.from
        t2 <- rownames(dfv) %in% dfe$.to
        dfv$.axis_ori[t1  & !t2] <- 1L
        dfv$.axis_ori[t1  &  t2] <- 2L
        dfv$.axis_ori[!t1 &  t2] <- 3L
    } else if (FALSE) {
        # TODO more methods here
    }
    
    # data reformat
    dfv_ori <- dfv
    dfv <- duplicate_row(dfv, axis_dup = axis_dup, axis_jit = axis_jit)
    dfe <- duplicate_axis(
        dfe, dfv_ori, dfv, 
        n_axis = n_axis, axis_dup = axis_dup, axis_jit = axis_jit
    )
    dfv <- coord_transfer(
        dfv, task = "data",
        n_axis = n_axis, y_expand = y_expand,
        axis_normalize = axis_normalize, axis_rank = axis_rank
    )
    
    # bezier curve
    df_bezier <- bezier_transfer(
        dfe, dfv, n_axis = n_axis, 
        bezier_jit_in = bezier_jit_in, bezier_jit = bezier_jit_out
    )
    
    # axis and panel grid line
    df_axis <- coord_transfer(
        dfv, task = "axis",
        n_axis = n_axis, y_expand = y_expand,
        axis_normalize = axis_normalize, axis_rank = axis_rank
    )
    df_grid_major <- coord_transfer(
        dfv, task = "grid_major",
        n_axis = n_axis, y_expand = y_expand,
        axis_normalize = axis_normalize, axis_rank = axis_rank, 
        grid_cut = grid_cut
    )
    df_grid_major_label <- NULL
    if (!is.null(df_grid_major)) {
        df_grid_major <- df_grid_major[
            df_grid_major$.x >= min(df_bezier$.coord_x) & 
                df_grid_major$.x <= max(df_bezier$.coord_x) & 
                df_grid_major$.y >= min(df_bezier$.coord_y) & 
                df_grid_major$.y <= max(df_bezier$.coord_y), 
            ]
        df_grid_major <- fix_grid_df(df_grid_major, grid_cut = grid_cut)
        df_grid_major_label <- 
            df_grid_major[df_grid_major$.degree == label_rel_pos, ]
    }
    
    df_grid_minor <- coord_transfer(
        dfv, task = "grid_minor",
        n_axis = n_axis, y_expand = y_expand,
        axis_normalize = axis_normalize, axis_rank = axis_rank, 
        grid_cut = grid_cut
    )
    df_grid_minor_label <- NULL
    if (!is.null(df_grid_minor)) {
        df_grid_minor <- df_grid_minor[
            df_grid_minor$.x >= min(df_bezier$.coord_x) & 
                df_grid_minor$.x <= max(df_bezier$.coord_x) & 
                df_grid_minor$.y >= min(df_bezier$.coord_y) & 
                df_grid_minor$.y <= max(df_bezier$.coord_y), 
            ]
        df_grid_minor <- fix_grid_df(df_grid_minor, grid_cut = grid_cut)
        df_grid_minor_label <- 
            df_grid_minor[df_grid_minor$.degree == label_rel_pos, ]
    }
    
    list(
        dfe = dfe, dfv = dfv, df_bezier = df_bezier, df_axis = df_axis, 
        df_grid_major = df_grid_major, df_grid_minor = df_grid_minor, 
        df_grid_major_label = df_grid_major_label, 
        df_grid_minor_label = df_grid_minor_label
    )
}
duplicate_row <- function(dfv, axis_dup, axis_jit) {
    dfv$.axis <- dfv$.axis_ori
    t1 <- dfv$.axis %in% axis_dup
    if (!all(t1)) {
        dfv_keep <- dfv[!t1, ]
        dfv_keep$.axis_stat <- "keep"
        rownames(dfv_keep) <- paste0(rownames(dfv_keep), "_keep")
    } else {
        dfv_keep <- NULL
    }
    if (any(t1)) {
        dfv_left <- dfv[t1, ]
        dfv_left$.axis <- dfv_left$.axis - axis_jit
        dfv_left$.axis_stat <- "from"
        rownames(dfv_left) <- paste0(rownames(dfv_left), "_from")
        dfv_right <- dfv[t1, ]
        dfv_right$.axis <- dfv_right$.axis + axis_jit
        dfv_right$.axis_stat <- "to"
        rownames(dfv_right) <- paste0(rownames(dfv_right), "_to")
    } else {
        dfv_left <- NULL
        dfv_right <- NULL
    }
    rbind(dfv_keep, dfv_left, dfv_right)
}
duplicate_axis <- function(dfe, dfv_ori, dfv, n_axis, axis_dup, axis_jit) {
    x <- dfv_ori[dfe$.from, ".axis_ori"]
    y <- dfv_ori[dfe$.to, ".axis_ori"]
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
    dfe$.axis_ori_from <- x
    dfe$.axis_ori_to <- y
    dfe$.axis_from <- x_mod
    dfe$.axis_to <- y_mod
    danteng <- function(str, axis) {
        rownames(dfv)[dfv$.name == str & dfv$.axis == axis]
    }
    dfe$.from_fix <- mapply(danteng, str = dfe$.from, axis = dfe$.axis_from)
    dfe$.to_fix <- mapply(danteng, str = dfe$.to, axis = dfe$.axis_to)
    dfe
}
coord_transfer <- function(
    dfv, task = "data",
    n_axis, y_expand, axis_normalize, axis_rank, grid_cut = NULL
) {
    if (is.null(grid_cut)) grid_cut <- 360
    
    x <- dfv$.axis
    x_fctr <- factor(x, levels = unique(x))
    y <- dfv$.y_ori
    if (axis_rank) {
        y <- unlist(by(y, x_fctr, order))
    }
    if (axis_normalize) {
        norm_num <- function(x) {(x - min(x)) / (max(x) - min(x))}
        y <- unlist(by(y, x_fctr, norm_num))
    }
    if (!identical(min(y), max(y))) {
        y <- (y - min(y)) / (max(y) - min(y)) + y_expand
    } else {
        y <- y + y_expand
    }
    y_min <- min(y)
    y_max <- as.numeric(by(y, x_fctr, max))
    
    if (identical(task, "axis")) {
        rel_x <- (unique(x) - 1) / n_axis
        df <- data.frame(
            .x = sin(2 * pi * rel_x) * y_min,
            .y = cos(2 * pi * rel_x) * y_min,
            .xend = sin(2 * pi * rel_x) * y_max,
            .yend = cos(2 * pi * rel_x) * y_max,
            stringsAsFactors = FALSE
        )
        rownames(df) <- unique(x)
        df
    } else if (identical(task, "grid_major") | identical(task, "grid_minor")) {
        if (axis_rank | axis_normalize) {
            warning(
                "grid can not be used in normalized or ranked axis, ", 
                "NULL will be returned."
            )
            return(NULL)
        }
        
        y <- dfv$.y_ori
        # major and minor labels
        lb_ma <- pretty(y, h = 2)
        lb_mi <- pretty(y)[!pretty(y) %in% lb_ma]
        
        if (identical(task, "grid_major")) {
            lbs <- lb_ma
        } else if (identical(task, "grid_minor")) {
            lbs <- lb_mi
        }
        if (length(lbs) == 0) {
            return(NULL)
        }
        if (!identical(min(y), max(y))) {
            y <- (lbs - min(y)) / (max(y) - min(y)) + y_expand
        } else {
            y <- lbs + y_expand
        }
        l_df <- mapply(
            function(y_sub, lb, grid_cut) {
                data.frame(
                    ".label" = lb, 
                    ".degree" = 0:grid_cut, 
                    ".x" = sin(2 * pi * 0:grid_cut / grid_cut) * y_sub, 
                    ".y" = cos(2 * pi * 0:grid_cut / grid_cut) * y_sub
                )
            }, 
            y_sub = y, lb = lbs, grid_cut = grid_cut, 
            SIMPLIFY = FALSE
        )
        do.call(rbind, l_df)
    } else {
        dfv$.y_mod <- y
        dfv$.x <- sin(2 * pi * (x - 1) / n_axis) * y
        dfv$.y <- cos(2 * pi * (x - 1) / n_axis) * y
        dfv
    }
}
bezier_transfer <- function(dfe, dfv, n_axis, bezier_jit_in, bezier_jit) {
    
    csin <- function(x, y, n_axis) {
        sin(2 * pi * x / n_axis) * y
    }
    ccos <- function(x, y, n_axis) {
        cos(2 * pi * x / n_axis) * y
    }
    
    df <- data.frame(
        .id = factor(1:nrow(dfe)), 
        x1 = dfv[dfe$.from_fix, ".x"],
        x2 = 0, 
        x3 = 0, 
        x4 = dfv[dfe$.to_fix, ".x"],
        y1 = dfv[dfe$.from_fix, ".y"],
        y2 = 0, 
        y3 = 0, 
        y4 = dfv[dfe$.to_fix, ".y"],
        stringsAsFactors = FALSE
    )
    t1 <- 
        dfe$.axis_ori_from == min(dfe$.axis_ori_from) & 
        dfe$.axis_ori_to == max(dfe$.axis_ori_to)
    t2 <- dfe$.axis_ori_from == dfe$.axis_ori_to
    
    df_sub <- dfe[t1, ]
    xa <- dfv[df_sub$.from_fix, ".axis"]
    xb <- dfv[df_sub$.to_fix, ".axis"]
    ya <- dfv[df_sub$.from_fix, ".y_mod"]
    yb <- dfv[df_sub$.to_fix, ".y_mod"]
    df$x2[t1] <- csin((xa - (xa + n_axis - xb) * bezier_jit) - 1, ya, n_axis)
    df$y2[t1] <- ccos((xa - (xa + n_axis - xb) * bezier_jit) - 1, ya, n_axis)
    df$x3[t1] <- csin((xb + (xa + n_axis - xb) * bezier_jit) - 1, yb, n_axis)
    df$y3[t1] <- ccos((xb + (xa + n_axis - xb) * bezier_jit) - 1, yb, n_axis)
    
    df_sub <- dfe[!t1, ]
    xa <- dfv[df_sub$.from_fix, ".axis"]
    xb <- dfv[df_sub$.to_fix, ".axis"]
    ya <- dfv[df_sub$.from_fix, ".y_mod"]
    yb <- dfv[df_sub$.to_fix, ".y_mod"]
    df$x2[!t1] <- csin((xa + abs(xa - xb) * bezier_jit) - 1, ya, n_axis)
    df$y2[!t1] <- ccos((xa + abs(xa - xb) * bezier_jit) - 1, ya, n_axis)
    df$x3[!t1] <- csin((xb - abs(xa - xb) * bezier_jit) - 1, yb, n_axis)
    df$y3[!t1] <- ccos((xb - abs(xa - xb) * bezier_jit) - 1, yb, n_axis)
    
    df_sub <- dfe[t2, ]
    xa <- dfv[df_sub$.from_fix, ".axis"]
    xb <- dfv[df_sub$.to_fix, ".axis"]
    ya <- dfv[df_sub$.from_fix, ".y_mod"]
    yb <- dfv[df_sub$.to_fix, ".y_mod"]
    df$x2[t2] <- csin((xa + abs(xa - xb) * bezier_jit_in) - 1, ya, n_axis)
    df$y2[t2] <- ccos((xa + abs(xa - xb) * bezier_jit_in) - 1, ya, n_axis)
    df$x3[t2] <- csin((xb - abs(xa - xb) * bezier_jit_in) - 1, yb, n_axis)
    df$y3[t2] <- ccos((xb - abs(xa - xb) * bezier_jit_in) - 1, yb, n_axis)
    
    df_x <- reshape2::melt(
        cbind(dfe, df[, c(".id", "x1", "x2", "x3", "x4")]), 
        measure.vars = c("x1", "x2", "x3", "x4"), 
        variable.name = ".x", value.name = ".coord_x"
    )
    df_y <- reshape2::melt(
        df[, c("y1", "y2", "y3", "y4")], 
        measure.vars = c("y1", "y2", "y3", "y4"), 
        variable.name = ".y", value.name = ".coord_y"
    )
    df <- cbind(df_x, df_y)
    df[order(df$.id), ]
}
fix_grid_df <- function(df, grid_cut) {
    df$.group <- paste(df$.label, (1:nrow(df) - df$.degree), sep = "_")
    df
}
