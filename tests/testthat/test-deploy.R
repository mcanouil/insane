test_that('deploy to temp dir', {
  dir.create(file.path(tempdir(), "test_insane"), showWarnings = FALSE)
  expect_true(deploy(directory = file.path(tempdir(), "test_insane")))
  expect_false(deploy(directory = file.path(tempdir(), "test_insane")))
  expect_false(deploy(directory = file.path(tempdir(), "test_insane"), demo = TRUE))
  expect_true(deploy(directory = file.path(tempdir(), "test_insane"), demo = TRUE, overwrite = TRUE))
})
