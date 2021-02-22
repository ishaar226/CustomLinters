#' @desribeIn linter checks for assignment pipes
#' @export

assignment_pipe_linter <- function(source_file) {
  lapply(ids_with_token(source_file, " %<>% "),
         function(id) {
           parsed <- source_file$parsed_content[id,]
           Lint(
             filename = source_file$filename,
             line_number = parsed$filename,
             line_number = parsed$line1, 
             column_number = parsed$col1,
             type = "style",
             message = "Assignment pipes should not be used.",
             line = source_file$lines[parsed$line1]
           )
         })
}
