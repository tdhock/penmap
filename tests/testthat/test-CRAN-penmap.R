library(testthat)
m.inc <- new(penmap::penmap)
test_that("one insert one row 0.1", {
  m.inc$insert(0.1, 2.0, 3)
  expect_equal(m.inc$df(), data.frame(penalty=0.1, loss=2, model_size=3, after=0))
})
test_that("two inserts, same size, two rows", {
  m.inc$insert(0.2, 2.0, 3)
  expect_equal(m.inc$df(), data.frame(penalty=c(0.1, 0.2), loss=2, model_size=3, after=c(1,0)))
})
test_that("three inserts, same size, two rows", {
  m.inc$insert(0.3, 2.0, 3)
  expect_equal(m.inc$df(), data.frame(penalty=c(0.1, 0.3), loss=2, model_size=3, after=c(1,0)))
})

m.dec <- new(penmap::penmap)
test_that("one insert one row 0.3", {
  m.dec$insert(0.3, 2.0, 3)
  expect_equal(m.dec$df(), data.frame(penalty=0.3, loss=2, model_size=3, after=0))
})
test_that("two inserts, same size, two rows, dec", {
  m.dec$insert(0.2, 2.0, 3)
  expect_equal(m.dec$df(), data.frame(penalty=c(0.2, 0.3), loss=2, model_size=3, after=c(1,0)))
})
test_that("three inserts, same size, two rows, dec", {
  m.dec$insert(0.1, 2.0, 3)
  expect_equal(m.dec$df(), data.frame(penalty=c(0.1, 0.3), loss=2, model_size=3, after=c(1,0)))
})

m.points <- new(penmap::penmap)
test_that("one insert one row 0.1 30", {
  m.points$insert(0.1, 2.0, 30)
  expect_equal(m.points$df(), data.frame(penalty=0.1, loss=2.0, model_size=30, after=0))
})
test_that("two inserts, two rows, 30 20", {
  m.points$insert(0.2, 3.5, 20)
  expect_equal(m.points$df(), data.frame(penalty=c(0.1, 0.2), loss=c(2.0, 3.5), model_size=c(30, 20), after=c(0, 0)))
})
test_that("three inserts, three rows, 30 20 10", {
  m.points$insert(0.3, 6.5, 10)
  expect_equal(m.points$df(), data.frame(penalty=c(0.1, 0.2, 0.3), loss=c(2.0, 3.5, 6.5), model_size=c(30, 20, 10), after=c(0, 0, 0)))
})

test_that("error already known", {
  m.dup <- new(penmap::penmap)
  m.dup$insert(0.1, 2.0, 3)
  expect_error({
    m.dup$insert(0.1, 2.0, 3)
  }, class="std::range_error")
  m.dup$insert(0.5, 2.0, 3)
  expect_error({
    m.dup$insert(0.1, 2.0, 3)
  }, class="std::range_error")
  expect_error({
    m.dup$insert(0.2, 2.0, 3)
  }, class="std::range_error")
  expect_error({
    m.dup$insert(0.5, 2.0, 3)
  }, class="std::range_error")
})

test_that("breakpoint and model size ok", {
  m <- new(penmap::penmap)
  m$insert(1.0, 2.0, 3)
  expect_equal(m$df(), data.frame(penalty=1.0, loss=2.0, model_size=3, after=0))
  m$insert(2.0, 3.5, 2)
  (res <- m$df())
  expect_equal(res, data.frame(penalty=c(1.0, 1.5, 2.0), loss=c(2.0, 3.5, 3.5), model_size=c(3, 2, 2), after=c(1,1,0)))
  ##penaltyLearning::modelSelection(data.frame(complexity=c(3,2), loss=c(2, 3.5)))
})

##   penalty loss model_size after
## 1     2.0  3.5          2     1
## 2     3.0  6.5          1     1
## 3     3.5 10.0          0     1
## 4     4.0 10.0          0     0
test_that("insert three models ok with cross point size=2", {
  m=new(penmap::penmap)
  m$insert(2, 3.5, 2)
  expect_equal(m$df(), data.frame(penalty=2, loss=3.5, model_size=2, after=0))
  ## at penalty=3 model_size=1 and 2 are optimal
  m$insert(3, 3.5, 2)
  expect_equal(m$df(), data.frame(penalty=c(2,3), loss=c(3.5, 3.5), model_size=2, after=c(1,0)))
  m$insert(4, 10, 0)
  expect_equal(m$df(), data.frame(penalty=c(2,3,4), loss=c(3.5,3.5,10), model_size=c(2,2,0), after=c(1,0,0)))
  m$insert(3.1, 6.5, 1)
  expect_equal(m$df(), data.frame(penalty=c(2,3,3.5,4), loss=c(3.5,6.5,10,10), model_size=c(2,1,0,0), after=c(1,1,1,0)))
})

test_that("insert three models ok with cross point size=1", {
  m=new(penmap::penmap)
  m$insert(2, 3.5, 2)
  expect_equal(m$df(), data.frame(penalty=2, loss=3.5, model_size=2, after=0))
  ## at penalty=3 model_size=1 and 2 are optimal
  m$insert(3, 6.5, 1)
  expect_equal(m$df(), data.frame(penalty=c(2,3), loss=c(3.5, 6.5), model_size=c(2,1), after=c(1,0)))
  m$insert(4, 10, 0)
  expect_equal(m$df(), data.frame(penalty=c(2,3,3.5,4), loss=c(3.5,6.5,10,10), model_size=c(2,1,0,0), after=c(1,1,1,0)))
})

test_that("insert three models ok with cross point size=1 other side", {
  m=new(penmap::penmap)
  m$insert(3, 6.5, 1)
  expect_equal(m$df(), data.frame(penalty=3, loss=6.5, model_size=1, after=0))
  m$insert(2, 3.5, 2)
  expect_equal(m$df(), data.frame(penalty=c(2,3), loss=c(3.5, 6.5), model_size=c(2,1), after=c(1,0)))
  m$insert(4, 10, 0)
  expect_equal(m$df(), data.frame(penalty=c(2,3,3.5,4), loss=c(3.5,6.5,10,10), model_size=c(2,1,0,0), after=c(1,1,1,0)))
})

##    min.lambda max.lambda loss complexity
## 1:        0.0        3.0  3.5          2
## 2:        3.0        3.5  6.5          1
## 3:        3.5        Inf 10.0          0
test_that("insert three models ok with fill larger smaller", {
  m=new(penmap::penmap)
  m$insert(2, 3.5, 2)
  expect_equal(m$df(), data.frame(penalty=2, loss=3.5, model_size=2, after=0))
  m$insert(4, 10, 0)
  m$df()
  expect_equal(m$df(), data.frame(penalty=c(2, 4), loss=c(3.5, 10), model_size=c(2, 0), after=0))
  m$insert(3.25, 6.5, 1)
  expect_equal(m$df(), data.frame(penalty=c(2, 3, 3.5, 4), loss=c(3.5, 6.5, 10, 10), model_size=c(2, 1, 0, 0), after=c(1,1,1,0)))
})

test_that("breakpoints are combined", {
  m = new(penmap::penmap)
  m$insert(0, 2, 3)
  expect_equal(m$df(), data.frame(penalty=0, loss=2, model_size=3, after=0))
  m$insert(2, 3.5, 2)
  expect_equal(m$df(), data.frame(penalty=c(0,1.5,2), loss=c(2,3.5,3.5), model_size=c(3,2,2), after=c(1,1,0)))
  m$insert(5, 10, 0)
  expect_equal(m$df(), data.frame(penalty=c(0,1.5,2,5), loss=c(2,3.5,3.5,10), model_size=c(3,2,2,0), after=c(1,1,0,0)))
  m$insert(3.25, 10, 0)
  expect_equal(m$df(), data.frame(penalty=c(0,1.5,3.25,5), loss=c(2,3.5,10,10), model_size=c(3,2,0,0), after=c(1,1,1,0)))
})

m <- new(penmap::penmap)
m$insert(1.0, 2.0, 3)
m$df()
m$insert(2.0, 3.5, 2)
m$df()
m$insert(3.5, 6.5, 1)
m$df()
m$insert(4.0, 10.0, 0)
m$df()
##penaltyLearning::modelSelection(data.frame(complexity=3:0, loss=c(2, 3.5, 6.5, 10)))
