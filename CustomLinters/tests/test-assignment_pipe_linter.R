library(lintr)
library(testthat)
library(rex)
context("assignment_pipe_linter")
source("CustomLinters/CustomLinters/assignment_pipe_linter.R")  


test_that("returns the correct lintting", {
  expect_lint("Blah", NULL, assignment_pipe_linter)
  expect_lint("Blah %<>% xyv",
    rex("Assignment pipes should not be used."),
      assignment_pipe_linter)
})



