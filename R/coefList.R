coefList <- function(object){
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
