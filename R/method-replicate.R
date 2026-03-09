#' Declare a resampling method for a registered implementation
#'
#' `method_replicate()` declares a named resampling method and its accepted
#' arguments for use in [reg_model_id()]. It is part of the developer-facing
#' extension API — end users do not call it directly.
#'
#' When a test registers an implementation with `method_replicate()`, that
#' implementation becomes available via [resample()] in the lazy pipeline.
#' The `method_name` must match the `.method` argument passed to [resample()].
#'
#' @section Developer API:
#' `method_replicate()` is used inside [reg_model_id()] to associate an impl
#' function with a named resampling method. Multiple resampling methods can be
#' registered for the same model type by passing multiple [reg_model_id()]
#' calls into [build_htest()].
#'
#' @param method_name A string naming the resampling method, e.g. `"permute"`
#'   or `"boot"`. Must match the `.method` argument passed to [resample()].
#' @param args An `extra_args` object from [extra_args()] declaring the
#'   method-specific arguments and their defaults. Defaults to `NULL` for
#'   methods with no extra arguments.
#'
#' @return A `method_replicate` object.
#'
#' @seealso [reg_model_id()], [extra_args()], [resample()], [build_htest()]
#'
#' @examples
#' # Permutation method with extra args
#' method_replicate(
#'     method_name = "permute",
#'     args = extra_args(
#'         n ~ 1000L,
#'         seed ~ NULL
#'     )
#' )
#'
#' # Inside reg_model_id()
#' reg_model_id(
#'     ttest_impl_rel_permute,
#'     "rel",
#'     method = method_replicate(
#'         method_name = "permute",
#'         args = extra_args(
#'             n ~ 1000L,
#'             seed ~ NULL
#'         )
#'     )
#' )
#'
#' @export
method_replicate = function(method_name, args = NULL) {
    out = list(
        method_name = method_name,
        args = args
    )
    class(out) = "method_replicate"
    out
}
