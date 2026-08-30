# # Example data
# dat <- data.frame(
#   variable = LETTERS[1:6],
#
#   group1__mpg = c(
#     21.0, 21.0, 22.8,
#     21.4, 18.7, 18.1
#   ),
#
#   group1__cyl = c(
#     6, 6, 4,
#     6, 8, 6
#   ),
#
#   group1__disp = c(
#     160, 160, 108,
#     258, 360, 225
#   ),
#
#   group2__hp = c(
#     110, 110, 93,
#     110, 175, 105
#   ),
#
#   group2__drat = c(
#     3.90, 3.90, 3.85,
#     3.08, 3.15, 2.76
#   ),
#
#   variable2 = LETTERS[1:6],
#
#   check.names = FALSE
# )
#
#
# # Notes
# notes <- list(
#
#   # Body: multiple rows + multiple columns
#   list(
#     row = c(1, 3, 5),
#     col = c(
#       "group1__mpg",
#       "group1__cyl"
#     ),
#     mark = "a",
#     text = "P < 0.05"
#   ),
#
#   # Body: column index
#   list(
#     row = c(2, 4),
#     col = c(2, 3),
#     mark = "b",
#     text = "P < 0.01"
#   ),
#
#   # Header: first-level header
#   list(
#     part = "header",
#     row = 1,
#     col = 2,
#     mark = "c",
#     text = "Adjusted model"
#   ),
#
#   # Header: second-level header
#   list(
#     part = "header",
#     row = 2,
#     col = 5,
#     mark = "d",
#     text = "Per 1-SD increase"
#   ),
#
#   # Header: column index
#   list(
#     part = "header",
#     row = 2,
#     col = c(2, 3),
#     mark = "e",
#     text = "Missing data: 43 for body mass index, 4 for estimated glomerular filtration rate, 4 for high-density lipoprotein, 5 for total cholesterol, 49 for triglycerides, 175 for high-sensitivity C-reactive protein."
#   )
# )
#
#
# # Create table
# ft <- format_flextable(
#   dat,
#   notes = notes
# )
#
# ft
#
# doc <- officer::read_docx()
#
# doc <- flextable::body_add_flextable(
#   doc,
#   value = ft
# )
#
# print(
#   doc,
#   target = "table.docx"
# )
