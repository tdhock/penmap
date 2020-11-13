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
