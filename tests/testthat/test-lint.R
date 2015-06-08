if(require("lintr", quietly = TRUE)) {
  skip_on_cran()
  context("Code is high quality and lint free")
  test_that("Code Lint", {
    my_linters <- list(
      absolute_paths_linter=absolute_paths_linter,
      assignment_linter=assignment_linter,
      closed_curly_linter=closed_curly_linter,
      commas_linter=commas_linter,
      #commented_code_linter=commented_code_linter,
      infix_spaces_linter=infix_spaces_linter,
      line_length_linter=line_length_linter,
      no_tab_linter=no_tab_linter,
      object_usage_linter=object_usage_linter,
      snake_case_linter=snake_case_linter,
      multiple_dots_linter=multiple_dots_linter,
      object_length_linter=object_length_linter,
      open_curly_linter=open_curly_linter,
      single_quotes_linter=single_quotes_linter,
      spaces_inside_linter=spaces_inside_linter,
      #spaces_left_parentheses_linter=spaces_left_parentheses_linter,
      trailing_blank_lines_linter=trailing_blank_lines_linter,
      trailing_whitespace_linter=trailing_whitespace_linter
    )
    lintr::expect_lint_free(linters=my_linters)
  })
}
