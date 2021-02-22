context("assignment_pipe_linter")
source(here("CustomLinters/assignment_pipe_linter.R"))  
library(lintr)
library(testthat)
library(rex)

test_that("returns the correct lintting", {
  expect_lint("Blah", NULL, assignment_pipe_linter)
  expect_lint("Blah %<>% xyv",
    rex("Assignment pipes should not be used."),
      assignment_pipe_linter)
})



