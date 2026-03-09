#' Supplied arguments declaration
#'
#' Use one-sided formula `~n` for no default, two-sided `n ~ 1000L` for default.
#'
#' @keywords internal
#' @export
extra_args = function(...) {
    dots = rlang::enexprs(...)

    extracts = purrr::map(
        dots,
        function(quo) {
            if (rlang::is_formula(quo)) {
                lhs = rlang::f_lhs(quo)
                rhs = rlang::f_rhs(quo)

                if (is.null(lhs)) {
                    list(
                        name = rlang::as_label(rhs),
                        default = NULL
                    )
                } else {
                    list(
                        name = rlang::as_label(lhs),
                        default = eval(rhs)
                    )
                }
            } else {
                cli::cli_abort(
                    "Each argument must be a formula. Use {.code ~n} or {.code n ~ 1000L}."
                )
            }
        }
    )

    out = list(args = extracts)
    class(out) = "extra_args"
    out
}
