
# rmarkdown::render(input = "Belknap_Monti_project3_ST558.Rmd",
#                   output_file = "README.md",
#                   output_format = "github_document",
#                   output_options = list(
#                     name_value_pairs = "value",
#                     or_something = TRUE,
#                     html_preview = FALSE,
#                     toc = TRUE,
#                     df_print = "tibble"
#                   ),
#                   runtime = "static",
#                   clean = TRUE,
#                   params = NULL,
#                   knit_meta = NULL,
#                   envir = parent.frame(),
#                   run_pandoc = TRUE,
#                   quiet = FALSE,
#                   encoding = "UTF-8"
# )

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "lifestyle.md", params = list(channel = "lifestyle"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "entertainment.md", params = list(channel = "entertainment"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "bus.md", params = list(channel = "bus"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "socmed.md", params = list(channel = "socmed"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "tech.md", params = list(channel = "tech"))

rmarkdown::render("Belknap_Monti_project3_ST558.Rmd", output_file = "world.md", params = list(channel = "world"))
