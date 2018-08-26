context("first test")
library(courseraRPackageProj)


test_that("first test", {
  expect_that(make_filename(2013), is_a("character"))
}
)
