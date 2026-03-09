#' @keywords internal
inequality = function(a, b, direction = "leq") {
    op = switch(
        direction,
        "lt" = `<`,
        "leq" = `<=`,
        "gt" = `>`,
        "geq" = `>=`,
        "eq" = `==`,
        "neq" = `!=`,
        "all" = function(a, b) TRUE,
        stop("Invalid direction specified.")
    )
    op(a, b)
}

#' @keywords internal
all_pairs = function(x, direction = "leq", simplify = TRUE) {
    pairs = tidyr::expand_grid(x = x, y = x) |>
        dplyr::filter(inequality(.data$x, .data$y, direction = {{ direction }}))

    out = if (simplify) {
        pairs |>
            dplyr::rowwise() |>
            dplyr::mutate(xy = list(c(.data$x, .data$y))) |>
            dplyr::pull(xy)
    } else {
        pairs
    }

    out
}
