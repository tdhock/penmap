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
cross <- function(d){
  l1 <- d$loss_on[-nrow(d)]
  l2 <- d$loss_on[-1]
  s1 <- d$size_on[-nrow(d)]
  s2 <- d$size_on[-1]
  x <- (l1-l2)/(s2-s1)
  x[is.finite(x)]
}
HELPFUL <- function(loss){
  data.frame(loss, size=-3)
}
BOTH <- L(Inf, -1)
UNKNOWN <- L(Inf, -2)

test_that("new penmap has next penalties 0 Inf", {
  m <- new(penmap::penmap)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
})

test_that("one insert pen=0.1", {
  m <- new(penmap::penmap)
  m$insert(0.1, 2.0, 3)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.1, L(2,3), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
})

test_that("one insert pen=0", {
  m <- new(penmap::penmap)
  m$insert(0, 2.0, 3)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,3), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), Inf)
})

test_that("one insert pen=Inf size=3", {
  m <- new(penmap::penmap)
  expect_error({
    m$insert(Inf, 2.0, 3)
  }, "size should be zero with infinite penalty")
})

test_that("one insert pen=Inf", {
  m <- new(penmap::penmap)
  m$insert(Inf, 2.0, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(Inf, L(2,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), 0)
})

test_that("three inserts same size increasing", {
  m <- new(penmap::penmap)
  m$insert(0.1, 2.0, 3)
  m$insert(0.2, 2.0, 3)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.1, L(2,3), L(2,3)),
    r(0.2, L(2,3), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
  m$insert(0.3, 2.0, 3)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.1, L(2,3), L(2,3)),
    r(0.3, L(2,3), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
})

test_that("three inserts same size decreasing", {
  m <- new(penmap::penmap)
  m$insert(0.3, 2.0, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.3, L(2,0), L(2,0)),
    r(Inf, L(2,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), 0)
  m$insert(0.2, 2.0, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.2, L(2,0), L(2,0)),
    r(Inf, L(2,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), 0)
  m$insert(0.1, 2.0, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.1, L(2,0), L(2,0)),
    r(Inf, L(2,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), 0)
})

##penaltyLearning::modelSelection(data.frame(loss=c(2,3.5,6.5), complexity=c(30,20,10)))
test_that("insert sizes 30 20 10", {
  m <- new(penmap::penmap)
  m$insert(0.1, 2.0, 30)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.1, L(2,30), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
  m$insert(0.2, 3.5, 20)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.1, L(2,30), HELPFUL(0.15)),
    r(0.2, L(3.5,20), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  cross(expected)
  expect_equal(sort(m$helpful()), c(0, 0.15, Inf))
  m$insert(0.4, 6.5, 10)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.1, L(2,30), HELPFUL(0.15)),
    r(0.2, L(3.5,20), HELPFUL(0.3)),
    r(0.4, L(6.5,10), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  cross(expected)
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, 0.15, 0.3, Inf))
})

test_that("insert sizes 10 20 30", {
  m <- new(penmap::penmap)
  m$insert(0.4, 6.5, 10)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.4, L(6.5,10), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
  m$insert(0.2, 3.5, 20)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.2, L(3.5,20), HELPFUL(0.3)),
    r(0.4, L(6.5,10), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  cross(expected)
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, 0.3, Inf))
  m$insert(0.1, 2, 30)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(0.1, L(2,30), HELPFUL(0.15)),
    r(0.2, L(3.5,20), HELPFUL(0.3)),
    r(0.4, L(6.5,10), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  cross(expected)
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, 0.15, 0.3, Inf))
})

## TODO check before inserting bogus loss values.

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
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(1.0, L(2.0,3), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
  m$insert(2.0, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(1, L(2,3), L(2,3)),
    r(1.5, BOTH, L(3.5,2)),
    r(2, L(3.5,2), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
})

test_that("insert three models ok with cross point size=2", {
  m=new(penmap::penmap)
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- r(2, L(3.5,2), UNKNOWN)
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
  ## at penalty=3 size_on=1 and 2 are optimal
  m$insert(3, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), L(3.5,2)),
    r(3, L(3.5,2), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
  m$insert(4, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), L(3.5,2)),
    r(3, L(3.5,2), HELPFUL(3.25)),
    r(4, L(10,0), L(10,0)),
    r(Inf, L(10,0), UNKNOWN))
  cross(expected)
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, 3.25))
  m$insert(3.1, 6.5, 1)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), L(3.5,2)),
    r(3, BOTH, L(6.5,1)),
    r(3.5, BOTH, L(10,0)),
    r(Inf, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), 0)
})

penaltyLearning::modelSelection(data.frame(loss=c(6.5,3.5,1.5),complexity=c(1,2,3)))
test_that("insert size=2 alone at pen=3", {
  m=new(penmap::penmap)
  m$insert(3, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(3, L(3.5,2), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
  ## at penalty=3 size_on=1 and 2 are optimal
  m$insert(3.1, 6.5, 1)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(3, L(3.5,2), L(6.5,1)),
    r(3.1, L(6.5,1), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
  m$insert(1.9, 1.5, 3)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(1.9, L(1.5,3), L(1.5,3)),
    r(2, BOTH, L(3.5,2)),
    r(3, BOTH, L(6.5,1)),
    r(3.1, L(6.5,1), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0, Inf))
})

test_that("insert three models ok with cross point size=1", {
  m=new(penmap::penmap)
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0,Inf))
  ## at penalty=3 size_on=1 and 2 are optimal
  m$insert(3, 6.5, 1)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), L(3.5,2)),
    r(3, L(6.5,1), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0,Inf))
  m$insert(4, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), L(3.5,2)),
    r(3, BOTH, L(6.5,1)),
    r(3.5, BOTH, L(10,0)),
    r(Inf, L(10,0), UNKNOWN))
  cross(expected)
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), 0)
})

test_that("insert three models ok with cross point size=1 other side", {
  m=new(penmap::penmap)
  m$insert(3, 6.5, 1)
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(3, L(6.5,1), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  (computed <- m$df())
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0,Inf))
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), L(3.5,2)),
    r(3, L(6.5,1), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0,Inf))
  m$insert(4, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), L(3.5,2)),
    r(3, BOTH, L(6.5,1)),
    r(3.5, BOTH, L(10,0)),
    r(Inf, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), 0)
})

test_that("insert three models ok with fill larger smaller", {
  m=new(penmap::penmap)
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0,Inf))
  m$insert(4, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), HELPFUL(3.25)),
    r(4, L(10, 0), L(10,0)),
    r(Inf, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(0,3.25))
  m$insert(3.25, 6.5, 1)
  (computed <- m$df())
  expected <- rbind(
    r(0, UNKNOWN, HELPFUL(0)),
    r(2, L(3.5,2), L(3.5,2)),
    r(3, BOTH, L(6.5, 1)),
    r(3.5, BOTH, L(10,0)),
    r(Inf, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), 0)
})

penaltyLearning::modelSelection(data.frame(loss=c(10,3.5),complexity=c(0,2)))
test_that("breakpoints are combined", {
  m = new(penmap::penmap)
  m$insert(0, 2, 3)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,3), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), Inf)
  m$insert(2, 3.5, 2)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,3), L(2,3)),
    r(1.5, BOTH, L(3.5, 2)),
    r(2, L(3.5, 2), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), Inf)
  m$insert(5, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,3), L(2,3)),
    r(1.5, BOTH, L(3.5, 2)),
    r(2, L(3.5, 2), HELPFUL(3.25)),
    r(5, L(10,0), L(10,0)),
    r(Inf, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), 3.25)
  m$insert(3.25, 10, 0)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,3), L(2,3)),
    r(1.5, BOTH, L(3.5, 2)),
    r(3.25, BOTH, L(10,0)),
    r(Inf, L(10,0), UNKNOWN))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), numeric())
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

penaltyLearning::modelSelection(data.frame(loss=c(2,3.5,6.5,10),complexity=103:100))
test_that("inserted penalty = larger intersect ok finite interval", {
  m = new(penmap::penmap)
  m$insert(0, 2, 103)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,103), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), Inf)
  m$insert(8, 10, 100)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,103), HELPFUL(8/3)),
    r(8, L(10,100), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  cross(expected)
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(8/3,Inf))
  m$insert(9, 10, 100)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,103), HELPFUL(8/3)),
    r(8, L(10,100), L(10,100)),
    r(9, L(10,100), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))    
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(8/3,Inf))
  m$insert(3.5, 6.5, 101)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,103), HELPFUL(2.25)),
    r(3.5, L(6.5,101), L(10,100)),
    r(9, L(10,100), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))
  cross(expected)
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), c(2.25,Inf))
  m$insert(2, 3.5, 102)
  (computed <- m$df())
  expected <- rbind(
    r(0, L(2,103), L(2,103)),
    r(1.5, BOTH, L(3.5, 102)),
    r(3, BOTH, L(6.5, 101)),
    r(3.5, BOTH, L(10,100)),
    r(9, L(10,100), UNKNOWN),
    r(Inf, UNKNOWN, HELPFUL(Inf)))    
  expect_equal(computed, expected)
  expect_equal(sort(m$helpful()), Inf)
})
