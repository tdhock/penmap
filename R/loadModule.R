Rcpp::loadModule("penmap_module", TRUE)

setMethod(
  "show",
  signature(object="Rcpp_penmap"),
  function(object){
    cat(sprintf(
      "Penalty map with %d breakpoints, %d optimal models, %d helpful penalties\n",
      object$num_breakpoints(),
      object$num_optimal(),
      object$num_helpful()))
  }
)

setMethod(
  "coef",
  signature(object="Rcpp_penmap"),
  coefList
)

setMethod(
  "plot",
  signature(x="Rcpp_penmap"),
  function(x, opt.color="red", ...){
    param.list <- coef(x)
    gg <- ggplot()+
      scale_size_manual(values=c(optimal=2))+
      xlab("penalty")+
      ylab("loss + penalty*model_size")+
      scale_color_manual(values=c(optimal=opt.color))+
      scale_fill_manual(values=c(
        "break"="black",
        end=opt.color,
        helpful="white"))
    if(nrow(param.list[["segment"]])){
      gg <- gg+geom_segment(aes(
        pen_start, cost_start,
        color=status, size=status,
        xend=pen_end, yend=cost_end),
        data=data.frame(param.list[["segment"]], status="optimal"))
    }
    if(nrow(param.list[["abline"]])){
      gg <- gg+geom_abline(aes(
        slope=size,
        intercept=loss),
        data=param.list[["abline"]])
    }
    if(nrow(param.list[["point"]])){
      gg <- gg+geom_point(aes(
        penalty, cost,
        fill=type),
        shape=21,
        color="black",
        data=param.list[["point"]])
    }
  }
)
