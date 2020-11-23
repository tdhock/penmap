penmap_to_list <- function(object){
  penalties <- object$df()
  models.list <- list()
  for(suffix in c("on", "after")){
    one.df.list <- list()
    for(prefix in c("loss", "size")){
      one.df.list[[prefix]] <- penalties[[paste0(prefix, "_", suffix)]]
    }
    all.rows <- do.call(data.frame, one.df.list)
    models.list[[suffix]] <- all.rows[0 <= all.rows$size, ]
  }
  models <- unique(do.call(rbind, models.list))
  penalties$next_pen <- c(penalties$penalty[-1], NA)
  cost <- function(loss, size, penalty){
    loss + ifelse(size==0, 0, size*penalty)
  }
  pen.segs <- with(penalties[0 <= penalties$size_after, ], data.frame(
    pen_start=penalty,
    pen_end=next_pen,
    loss=loss_after,
    size=size_after,
    cost_start=cost(loss_after, size_after, penalty),
    cost_end=cost(loss_after, size_after, next_pen)))
  is.break <- penalties$size_on == -1
  break.points <- if(any(is.break))with(penalties[is.break,], data.frame(
    penalty,
    loss=loss_after,
    size=size_after,
    type="break"))
  maybe.end <- penalties[!is.break & 0 <= penalties$size_on,]
  end.points <- if(nrow(maybe.end))with(maybe.end, data.frame(
    penalty,
    loss=loss_on,
    size=size_on,
    type="end"))
  maybe.help <- penalties[penalties$size_after == -3,]
  help.points <- if(nrow(maybe.help))with(maybe.help, data.frame(
    penalty=loss_after,
    loss=loss_on,
    size=size_on,
    type="helpful"))
  pen.points <- rbind(break.points, help.points, end.points)
  pen.points$cost <- with(pen.points, cost(loss, size, penalty))
  finite.cost <- with(pen.points, cost[is.finite(cost)])
  inf.cost <- if(length(finite.cost)){
    min(finite.cost)
  }else{
    0
  }
  pen.points[!is.finite(pen.points$cost), "cost"] <- inf.cost
  list(
    abline=models,
    point=pen.points,
    segment=pen.segs)
}

opt.color <- "red"

penmap_to_ggplot <- function(x, ...){
  param.list <- penmap_to_list(x)
  gg <- ggplot2::ggplot()+
    ggplot2::scale_size_manual(values=c(optimal=2))+
    ggplot2::xlab("penalty")+
    ggplot2::ylab("loss + penalty*model_size")+
    ggplot2::scale_color_manual(values=c(optimal=opt.color))+
    ggplot2::scale_fill_manual(values=c(
      "break"="black",
      end=opt.color,
      helpful="white"))
  if(nrow(param.list[["segment"]])){
    gg <- gg+ggplot2::geom_segment(ggplot2::aes(
      pen_start, cost_start,
      color=status, size=status,
      xend=pen_end, yend=cost_end),
      data=data.frame(param.list[["segment"]], status="optimal"))
  }
  if(nrow(param.list[["abline"]])){
    gg <- gg+
      ggplot2::geom_abline(ggplot2::aes(
        slope=size,
        intercept=loss),
        data=param.list[["abline"]])+
      ggplot2::geom_rect(ggplot2::aes(
        xmin=-Inf, xmax=0,
        ymin=-Inf, ymax=Inf),
        fill="white",
        color=NA)+
      directlabels::geom_dl(ggplot2::aes(
        0, loss, label=size),
        hjust=1,
        method="left.polygons",
        data=param.list[["abline"]])
  }
  if(nrow(param.list[["point"]])){
    gg <- gg+ggplot2::geom_point(ggplot2::aes(
      penalty, cost,
      fill=type),
      shape=21,
      color="black",
      data=param.list[["point"]])
  }
  gg
}

penmap_cat <- function(object){
  cat(sprintf(
    "with %d breakpoints, %d optimal models, %d helpful penalties.\n",
    object$num_breakpoints(),
    object$num_optimal(),
    object$num_helpful()))
}

Rcpp::loadModule("penmap_module", TRUE)

penmap_create <- function(solve, ...){
  e <- new.env()
  e$call <- match.call()
  e$data_args <- list(...)
  e$penmap <- new(penmap)
  e$results <- list()
  e$solve <- solve
  class(e) <- "penmap"
  e
}

penmap_result <- function(e, penalty){
  size <- e$penmap$minimize(penalty)
  if(size < 0){
    penmap_solve(e, penalty)
    size <- e$penmap$minimize(penalty)
  }
  e$results[[paste(size)]]
}

penmap_solve <- function(e, penalty.vec=e$penmap$helpful()){
  solve.args <- e$data_args
  result.list <- future.apply::future_lapply(penalty.vec, function(penalty){
    solve.args[["penalty"]] <- penalty
    do.call(e$solve, solve.args)
  })
  ##TODO error checking.
  for(pen.i in seq_along(penalty.vec)){
    penalty <- penalty.vec[[pen.i]]
    result <- result.list[[pen.i]]
    solved.size <- e$penmap$minimize(penalty)
    if(solved.size < 0){
      e$penmap$insert(penalty, result$penmap_loss, result$penmap_size)
      e$results[[paste(result$penmap_size)]] <- result
    }
  }
}

penmap_solve_plot <- function(e, penalty=e$penmap$helpful()){
  penmap_solve(e, penalty)
  plot(e)+
    ggplot2::geom_vline(ggplot2::aes(
      xintercept=penalty),
      color=opt.color,
      size=2,
      alpha=0.2,
      data=data.frame(penalty))
}

print.penmap <- function(x, ...){
  print(x$call)
  penmap_cat(x$penmap)
}

plot.penmap <- function(x, ...){
  penmap_to_ggplot(x$penmap)
}

