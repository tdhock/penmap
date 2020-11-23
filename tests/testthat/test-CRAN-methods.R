library(testthat)
p=new(penmap::penmap)

test_that("penmap_to_ggplot works", {
  g=penmap::penmap_to_ggplot(p)
  expect_is(g, "ggplot")
})

set.seed(1)
N <- 10
x <- c(rnorm(N, 0), rnorm(N, 3))
pm <- penmap::penmap_create(penmap::solver_fpop, x)
pm
str(pm)
future::plan("multicore")

penmap::penmap_solve(pm, 4)
pm
penmap::penmap_solve_plot(pm, 0)
pm
penmap::penmap_solve_plot(pm, 1e5)
pm
penmap::penmap_solve_plot(pm)
pm

signal <- c(
  rnorm(25, mean = 10),
  rnorm(25, mean = 7),
  rnorm(25, mean = 8),
  rnorm(25, mean = 5))
signal[86] <- 10
labels.df <- data.frame(
  start = c(20, 45, 80),
  end = c(30, 55, 90),
  changes = c(1, 1, 0))
pm <- penmap::penmap_create(penmap::solver_LOPART, signal, labels.df)

penmap::penmap_solve_plot(pm)
pm$penmap$df()
pm$penmap$helpful()
pen.list <- penmap::penmap_to_list(pm$penmap)

ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(
    size, loss),
    data=pen.list[["abline"]])

set.seed(1)
count.vec <- c(
  rpois(N, 0.4),
  rpois(N, 3.5),
  rpois(N, 0.5),
  rpois(N, 5.6),
  rpois(N, 0.6))
plot(count.vec)
chromEnd <- seq_along(count.vec)
count.df <- data.frame(
  chrom="chrX",
  chromStart=chromEnd-1L,
  chromEnd,
  count=count.vec)
pm <- penmap::penmap_create(penmap::solver_PeakSegOptimal, count.df)
penmap::penmap_solve_plot(pm, c(0, 0.3, 3, 30, 3e5))

penmap::penmap_solve_plot(pm)
pm$penmap$df()
pm$penmap$helpful()

cov.dir <- tempfile()
dir.create(cov.dir)
coverage.bedGraph <- file.path(cov.dir, "coverage.bedGraph")
write.table(
  count.df, coverage.bedGraph, sep="\t",
  row.names=FALSE, col.names=FALSE)

pm <- penmap::penmap_create(penmap::solver_PeakSegDisk, cov.dir)
penmap::penmap_solve_plot(pm, c(0, 0.3, 3, 30, 3e5))

penmap::penmap_solve_plot(pm)
pm$penmap$df()
pm$penmap$helpful()

pm <- penmap::penmap_create(penmap::solver_gfpop_std, x)
penmap::penmap_solve_plot(pm, c(0, 0.3, 3, 30, 3e5))

penmap::penmap_solve_plot(pm)
pm$penmap$df()
pm$penmap$helpful()

pm <- penmap::penmap_create(penmap::solver_PELT_mean, x)
penmap::penmap_solve_plot(pm, c(0, 0.3, 3, 30, 3e5))

penmap::penmap_solve_plot(pm)
pm$penmap$df()
pm$penmap$helpful()
