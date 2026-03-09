#' @export
TTEST = function(
        .model = NULL,
        .data = NULL,
        .name = "T-Test",
        .paired = TRUE,
        .mu = 0,
        .alt = "two.sided",
        .ci = 0.95,
        ...
) {
    args = list(
        .data = .data,
        .paired = .paired,
        .mu = .mu,
        .alt = .alt,
        .ci = .ci
    )

    build_htest(
        cls = "ttest",
        args = args,
        impl = list(
            reg_model_id(ttest_impl_two, "compare_by"),
            reg_model_id(ttest_impl_formula, "formula"),
            reg_model_id(ttest_impl_pairwise, "pairwise"),
            reg_model_id(ttest_impl_one, "one_sample"),
            reg_model_id(
                ttest_impl_rel_permute,
                "rel",
                method = method_replicate(
                    method_name = "permute",
                    args = extra_args(
                        n ~ 1000L,
                        seed ~ NULL
                    )
                )
            ),
            reg_model_id(
                ttest_impl_rel_boot,
                "rel",
                method = method_replicate(
                    method_name = "boot",
                    args = extra_args(
                        n ~ 1000L,
                        seed ~ NULL
                    )
                )
            )
        ),
        model_id = .model,
        .name = .name
    )
}

ttest_impl_two = function(model_obj, args) {
    grp = model_obj$data[[model_obj$vars$grp[[1]]]]
    resp = model_obj$data[[model_obj$vars$x[[1]]]]

    grp = as.character(grp)
    lvls = unique(grp)

    if (length(lvls) != 2L) {
        cli::cli_abort(c(
            "Two-sample t-test requires exactly 2 groups.",
            "i" = "Found {length(lvls)} group{?s} in {.val {model_obj$vars$x[[1]]}}."
        ))
    }

    res = stats::t.test(
        x = resp[grp == lvls[[1]]],
        y = resp[grp == lvls[[2]]],
        paired = isTRUE(args$.paired),
        mu = args$.mu  %||% 0,
        alternative = args$.alt %||% "two.sided",
        conf.level  = args$.ci  %||% 0.95
    )

    new_htest(res, impl_cls = "ttest_two")
}

ttest_impl_formula = function(model_obj, args) {
    res = stats::t.test(
        formula = model_obj$formula,
        data = model_obj$data,
        mu = args$.mu %||% 0,
        alternative = args$.alt %||% "two.sided",
        conf.level = args$.ci %||% 0.95
    )

    new_htest(res, impl_cls = "ttest_formula")
}

ttest_impl_pairwise = function(model_obj, args) {
    pairs = model_obj$pairs

    res = lapply(pairs, function(pair) {
        out = stats::t.test(
            x = model_obj$data[[pair[[1]]]],
            y = model_obj$data[[pair[[2]]]],
            paired = isTRUE(args$.paired),
            mu = args$.mu %||% 0,
            alternative = args$.alt %||% "two.sided",
            conf.level = args$.ci %||% 0.95
        )
        tidy_df = broom::tidy(out)
        tidy_df$pair = paste0(pair[[1]], " vs ", pair[[2]])
        tidy_df
    })

    new_htest(dplyr::bind_rows(res), impl_cls = "ttest_pairwise")
}

ttest_impl_one = function(model_obj, args) {
    res = stats::t.test(
        x = model_obj$data[[model_obj$vars[["var"]]]],
        mu = args$.mu,
        alternative = args$.alt,
        conf.level = args$.ci
    )

    new_htest(res, impl_cls = "ttest_one")
}

ttest_impl_rel_permute = function(model_obj, args) {
    n = args$replicate_spec$args$n
    seed = args$replicate_spec$args$seed

    if (!is.null(seed)) set.seed(seed)

    data = model_obj$data
    grp = model_obj$vars$x[[1]]
    resp = model_obj$vars$resp[[1]]
    lvls = unique(data[[grp]])

    if (length(lvls) != 2L) {
        cli::cli_abort(c(
            "Permutation t-test requires exactly 2 groups.",
            "i" = "Found {length(lvls)} group{?s} in {.val {grp}}."
        ))
    }

    obs = stats::t.test(
        x = data[[resp]][data[[grp]] == lvls[[1]]],
        y = data[[resp]][data[[grp]] == lvls[[2]]],
        paired = isTRUE(args$.paired)
    )$statistic

    null_dist = replicate(n, {
        perm = sample(data[[resp]])
        stats::t.test(
            x = perm[data[[grp]] == lvls[[1]]],
            y = perm[data[[grp]] == lvls[[2]]],
            paired = isTRUE(args$.paired)
        )$statistic
    })

    new_htest(
        list(
            observed = obs,
            null_dist = null_dist,
            p.value = mean(abs(null_dist) >= abs(obs)),
            n = n
        ),
        impl_cls = "ttest_permute"
    )
}

ttest_impl_rel_boot = function(model_obj, args) {
    n = args$replicate_spec$args$n
    seed = args$replicate_spec$args$seed

    if (!is.null(seed)) set.seed(seed)

    data = model_obj$data
    grp  = model_obj$vars$x[[1]]
    resp = model_obj$vars$resp[[1]]
    lvls = unique(data[[grp]])

    if (length(lvls) != 2L) {
        cli::cli_abort(c(
            "Bootstrap t-test requires exactly 2 groups.",
            "i" = "Found {length(lvls)} group{?s} in {.val {grp}}."
        ))
    }

    boot_dist = replicate(n, {
        idx = sample(nrow(data), replace = TRUE)
        boot = data[idx, ]
        stats::t.test(
            x = boot[[resp]][boot[[grp]] == lvls[[1]]],
            y = boot[[resp]][boot[[grp]] == lvls[[2]]],
            paired = isTRUE(args$.paired)
        )$statistic
    })

    new_htest(
        list(
            boot_dist = boot_dist,
            ci = quantile(
                boot_dist,
                c((1 - args$.ci) / 2, 1 - (1 - args$.ci) / 2)
            ),
            n = n
        ),
        impl_cls = "ttest_boot"
    )
}

#' @keywords internal
#' @export
print.ttest_two = function(x, ...) {
    res = x$data
    pander::pander(broom::tidy(res))
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_formula = function(x, ...) {
    res = x$data
    pander::pander(broom::tidy(res))
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_pairwise = function(x, ...) {
    print(x$data)
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_one = function(x, ...) {
    res = x$data
    pander::pander(broom::tidy(res))
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_permute = function(x, ...) {
    cli::cli_text("{.field Observed statistic}    : {round(x$data$observed, 4)}")
    cli::cli_text("{.field p-value (permutation)} : {round(x$data$p.value, 4)}")
    cli::cli_text("{.field Replicates}            : {x$data$n}")
    invisible(x)
}

#' @keywords internal
#' @export
print.ttest_boot = function(x, ...) {
    ci = round(x$data$ci, 4)
    cli::cli_text("{.field Bootstrap CI} : [{ci[[1]]}, {ci[[2]]}]")
    cli::cli_text("{.field Replicates}   : {x$data$n}")
    invisible(x)
}
