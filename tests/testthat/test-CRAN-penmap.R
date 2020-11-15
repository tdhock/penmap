library(testthat)
## helper functions for making expected values easier to understand.
r <- function(penalty, on, after){
  a <- function(d){
    suffix <- paste(match.call()[[2]])
    names(d) <- paste0(names(d), "_", suffix)
    d
  }
  data.frame(penalty, a(on), a(after))
}
L <- function(loss, size){
  data.frame(loss, size)
}
BOTH <- L(Inf, -2)
UNKNOWN <- L(Inf, -1)

m.inc <- new(penmap::penmap)
test_that("one insert one row 0.1", {
  m.inc$insert(0.1, 2.0, 3)
  (computed <- m.inc$df())
  expected <- r(0.1, L(2,3), UNKNOWN)
  expect_equal(computed, expected)
})
test_that("two inserts, same size, two rows", {
  m.inc$insert(0.2, 2.0, 3)
  (computed <- m.inc$df())
  expected <- rbind(
    r(0.1, L(2,3), L(2,3)),
    r(0.2, L(2,3), UNKNOWN))
  expect_equal(computed, expected)
})
test_that("three inserts, same size, two rows", {
  m.inc$insert(0.3, 2.0, 3)
  (computed <- m.inc$df())
  expected <- rbind(
    r(0.1, L(2,3), L(2,3)),
    r(0.3, L(2,3), UNKNOWN))
  expect_equal(computed, expected)
})

m.dec <- new(penmap::penmap)
test_that("one insert one row 0.3", {
  m.dec$insert(0.3, 2.0, 3)
  (computed <- m.dec$df())
  expected <- r(0.3, L(2,3), UNKNOWN)
  expect_equal(computed, expected)
})
test_that("two inserts, same size, two rows, dec", {
  m.dec$insert(0.2, 2.0, 3)
  (computed <- m.dec$df())
  expected <- rbind(
    r(0.2, L(2,3), L(2,3)),
    r(0.3, L(2,3), UNKNOWN))
  expect_equal(computed, expected)
})
test_that("three inserts, same size, two rows, dec", {
  m.dec$insert(0.1, 2.0, 3)
  (computed <- m.dec$df())
  expected <- rbind(
    r(0.1, L(2,3), L(2,3)),
    r(0.3, L(2,3), UNKNOWN))
  expect_equal(computed, expected)
})

m.points <- new(penmap::penmap)
test_that("one insert one row 0.1 30", {
  m.points$insert(0.1, 2.0, 30)
  (computed <- m.points$df())
  expected <- r(0.1, L(2.0,30), UNKNOWN)
  expect_equal(computed, expected)
})
test_that("two inserts, two rows, 30 20", {
  m.points$insert(0.2, 3.5, 20)
  (computed <- m.points$df())
  expected <- rbind(
    r(0.1, L(2,30), UNKNOWN),
    r(0.2, L(3.5,20), UNKNOWN))
  expect_equal(computed, expected)
})
test_that("three inserts, three rows, 30 20 10", {
  m.points$insert(0.3, 6.5, 10)
  (computed <- m.points$df())
  expected <- rbind(
    r(0.1, L(2,30), UNKNOWN),
    r(0.2, L(3.5,20), UNKNOWN),
    r(0.3, L(6.5,10), UNKNOWN))
  expect_equal(computed, expected)
})

test_that("error already known", {
  m.dup <- new(penmap::penmap)
  m.dup$insert(0.1, 2.0, 3)
  expect_error({
    m.dup$insert(0.1, 2.0, 3)
  }, class="std::domain_error")
  m.dup$insert(0.5, 2.0, 3)
  expect_error({
    m.dup$insert(0.1, 2.0, 3)
  }, class="std::domain_error")
  expect_error({
    m.dup$insert(0.2, 2.0, 3)
  }, class="std::domain_error")
  expect_error({
    m.dup$insert(0.5, 2.0, 3)
  }, class="std::domain_error")
})

test_that("breakpoint and model size ok", {
  m <- new(penmap::penmap)
  m$insert(1.0, 2.0, 3)
  (computed <- m$df())
  expected <- r(1.0, L(2.0,3), UNKNOWN)
  expect_equal(computed, expected)
  m$insert(2.0, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(1, L(2,3), L(2,3)),
    r(1.5, BOTH, L(3.5,2)),
    r(2, L(3.5,2), UNKNOWN))
  expect_equal(computed, expected)
})

test_that("insert three models ok with cross point size=2", {
  m=new(penmap::penmap)
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- r(2, L(3.5,2), UNKNOWN)
  expect_equal(computed, expected)
  ## at penalty=3 size_on=1 and 2 are optimal
  m$insert(3, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(2, L(3.5,2), L(3.5,2)),
    r(3, L(3.5,2), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(4, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(2, L(3.5,2), L(3.5,2)),
    r(3, L(3.5,2), UNKNOWN),
    r(4, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(3.1, 6.5, 1)
  (computed <- m$df())
  expected <- rbind(
    r(2, L(3.5,2), L(3.5,2)),
    r(3, BOTH, L(6.5,1)),
    r(3.5, BOTH, L(10,0)),
    r(4, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
})

test_that("insert three models ok with cross point size=1", {
  m=new(penmap::penmap)
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- r(2, L(3.5,2), UNKNOWN)
  expect_equal(computed, expected)
  ## at penalty=3 size_on=1 and 2 are optimal
  m$insert(3, 6.5, 1)
  (computed <- m$df())
  expected <- rbind(
    r(2, L(3.5,2), L(3.5,2)),
    r(3, L(6.5,1), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(4, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(2, L(3.5,2), L(3.5,2)),
    r(3, BOTH, L(6.5,1)),
    r(3.5, BOTH, L(10,0)),
    r(4, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
})

test_that("insert three models ok with cross point size=1 other side", {
  m=new(penmap::penmap)
  m$insert(3, 6.5, 1)
  expected <- r(3, L(6.5,1), UNKNOWN)
  (computed <- m$df())
  expect_equal(computed, expected)
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(2, L(3.5,2), L(3.5,2)),
    r(3, L(6.5,1), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(4, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(2, L(3.5,2), L(3.5,2)),
    r(3, BOTH, L(6.5,1)),
    r(3.5, BOTH, L(10,0)),
    r(4, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
})

test_that("insert three models ok with fill larger smaller", {
  m=new(penmap::penmap)
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- r(2, L(3.5,2), UNKNOWN)
  expect_equal(computed, expected)
  m$insert(4, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(2, L(3.5,2), UNKNOWN),
    r(4, L(10, 0), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(3.25, 6.5, 1)
  (computed <- m$df())
  expected <- rbind(
    r(2, L(3.5, 2), L(3.5, 2)),
    r(3, BOTH, L(6.5, 1)),
    r(3.5, BOTH, L(10,0)),
    r(4, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
})

test_that("breakpoints are combined", {
  m = new(penmap::penmap)
  m$insert(0, 2, 3)
  (computed <- m$df())
  expected <- r(0, L(2,3), UNKNOWN)
  expect_equal(computed, expected)
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,3), L(2,3)),
    r(1.5, BOTH, L(3.5, 2)),
    r(2, L(3.5, 2), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(5, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,3), L(2,3)),
    r(1.5, BOTH, L(3.5, 2)),
    r(2, L(3.5, 2), UNKNOWN),
    r(5, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(3.25, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,3), L(2,3)),
    r(1.5, BOTH, L(3.5, 2)),
    r(3.25, BOTH, L(10,0)),
    r(5, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
})

## test_that("inserted penalty = larger intersect ok Inf", {
##   m = new(penmap::penmap)
##   m$insert(0, 2, 3)
##   expect_equal(m$df(), data.frame(penalty=0, loss_on=2, size_on=3, loss_after=Inf, size_after=-1))
##   m$insert(Inf, 10, 0)
##   expect_equal(m$df(), data.frame(penalty=c(0, Inf), loss_on=c(2,10), size_on=c(3,0), loss_after=Inf, size_after=-1))
##   m$insert(3.5, 6.5, 1)
##   expect_equal(m$df(), data.frame(penalty=c(0, 3.5), loss_on=c(2,6.5), size_on=c(3,1), loss_after=c(Inf, 10), size_after=c(-1, 0)))
##   m$insert(2, 3.5, 2)
##   expect_equal(m$df(), data.frame(penalty=c(0, 1.5, 3, 3.5), loss_on=c(2, 3.5, 6.5, 10), size_on=c(3,2,1,0), loss_after=c(2,3.5,6.5,10), size_after=c(3,2,1,0)))
## })

## test_that("inserted penalty = larger intersect ok finite", {
##   m = new(penmap::penmap)
##   m$insert(0, 2, 103)
##   (computed <- m$df())
##   expect_equal(computed, data.frame(penalty=0, loss_on=2, size_on=103, loss_after=Inf, size_after=-1))
##   m$insert(8, 10, 100)
##   (computed <- m$df())
##   expect_equal(computed, data.frame(penalty=c(0, 8), loss_on=c(2,10), size_on=c(103,100), loss_after=Inf, size_after=-1))
##   m$insert(3.5, 6.5, 101)
##   (computed <- m$df())
##   expect_equal(computed, data.frame(penalty=c(0, 3.5), loss_on=c(2,6.5), size_on=c(103,101), loss_after=c(Inf, 10), size_after=c(-1, 100)))
##   m$insert(2, 3.5, 102)
##   (computed <- m$df())
##   expect_equal(computed, data.frame(penalty=c(0, 1.5, 3, 3.5), loss_on=c(2, 3.5, 6.5, 10), size_on=c(103,102,101,100), loss_after=c(2,3.5,6.5,10), size_after=c(103,102,101,100)))
## })

test_that("inserted penalty = larger intersect ok finite interval", {
  m = new(penmap::penmap)
  m$insert(0, 2, 103)
  (computed <- m$df())
  expected <- r(0, L(2,103), UNKNOWN)
  expect_equal(computed, expected)
  m$insert(8, 10, 100)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,103), UNKNOWN),
    r(8, L(10,100), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(9, 10, 100)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,103), UNKNOWN),
    r(8, L(10,100), L(10,100)),
    r(9, L(10,100), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(3.5, 6.5, 101)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,103), UNKNOWN),
    r(3.5, L(6.5,101), L(10,100)),
    r(9, L(10,100), UNKNOWN))
  expect_equal(computed, expected)
  m$insert(2, 3.5, 102)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,103), L(2,103)),
    r(1.5, BOTH, L(3.5, 102)),
    r(3, BOTH, L(6.5, 101)),
    r(3.5, L(6.5,101), L(10,100)),
    r(9, L(10,100), UNKNOWN))
  expect_equal(computed, expected)
})
