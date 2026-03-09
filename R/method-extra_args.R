#' Declare extra arguments for a registered method
#'
#' `extra_args()` declares the additional arguments accepted by any method
#' registered via [reg_model_id()], such as [method_replicate()] or future
#' method types. Each argument is declared as a formula: one-sided `~n` for
#' no default, or two-sided `n ~ 1000L` for a default value.
#'
#' These declarations are used by the corresponding pipeline step —
#' [resample()] for resampling methods, and equivalents for other method types
#' — to validate supplied arguments and fill in defaults for missing ones.
#'
#' @param ... One or more formulas declaring method arguments. Use `~n` for
#'   an argument with no default, or `n ~ 1000L` for an argument with a
#'   default value.
#'
#' @return An `extra_args` object containing a list of argument declarations,
#'   each with a `name` and `default` slot.
#'
#' @seealso [method_replicate()], [reg_model_id()], [resample()]
#'
#' @examples
#' # No defaults
#' extra_args(~n, ~seed)
#'
#' # With defaults
#' extra_args(
#'     n ~ 1000L,
#'     seed ~ NULL
#' )
#'
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
