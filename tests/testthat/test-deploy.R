test_that('deploy to temp dir', {
  expect_true(deploy(directory = tempdir()))
})

test_that('deploy to temp dir with examples', {
  expect_true(deploy(directory = tempdir(), with_examples = TRUE))
})
