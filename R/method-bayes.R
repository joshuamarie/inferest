#' Bayesian method declaration
#'
#' @param method_name A string naming the method, e.g. `"permute"`, `"boot"`.
#' @param args An `extra_args` object declaring method-specific arguments.
#'
#' @export
method_bayes = function(method_name, args = NULL) {
    out = list(
        method_name = method_name,
        args = args
    )
    class(out) = "method_replicate"
    out
}
