solver_fpop <- function(data, penalty){
  result <- fpop::Fpop(data, penalty)
  result$penmap_loss <- result$J.est
  result$penmap_size <- result$K-1
  result
}

solver_PeakSegOptimal <- function(data.df, penalty){
  result <- PeakSegOptimal::PeakSegFPOPchrom(data.df, penalty)
  result$penmap_loss <- with(result$loss, penalized.loss-peaks*penalty)
  result$penmap_size <- result$loss$peaks
  result
}

solver_LOPART <- function(x, labels, penalty){
  result <- LOPART::LOPART(x, labels, penalty)
  result$penmap_loss <- result$loss$total_loss
  result$penmap_size <- result$loss$changes_unlabeled
  result
}

solver_PeakSegDisk <- function(data.dir, penalty){
  result <- PeakSegDisk::PeakSegFPOP_dir(data.dir, penalty)
  result$penmap_loss <- result$loss$total.loss
  result$penmap_size <- result$loss$peaks
  result
}

solver_gfpop_std <- function(data.vec, penalty){
  result <- gfpop::gfpop(data.vec, gfpop::graph(type="std", penalty=penalty))
  result$penmap_loss <- result$globalCost
  result$penmap_size <- length(result$changepoints)-1L
  result
}

solver_PELT_mean <- function(data.vec, penalty){
  fit <- changepoint::cpt.mean(
    data.vec, penalty="Manual", pen.value=penalty, method="PELT")
  end <- fit@cpts
  start <- c(1, end[-length(end)]+1)
  mean.vec <- rep(fit@param.est[["mean"]], end-start+1)
  loss <- sum((data.vec-mean.vec)^2)
  list(
    cpt.mean=fit,
    penmap_loss=loss,
    penmap_size=changepoint::ncpts(fit))
}
