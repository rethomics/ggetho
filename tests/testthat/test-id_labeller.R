context("id labeller")

test_that("pipes are replaced by \n", {
  expect_equal(ggetho:::id_wrap("a|b|c"), "a\nb\nc")
})


