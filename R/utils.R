write_class = function(clss)
    c(clss, "infer_model")

add_class = function(x, clss)
    c(clss, class(x))

append_class = function(new_cls, current_cls)
    c(new_cls, current_cls)

`%||%` = function(x, y) if (!is.null(x)) x else y
