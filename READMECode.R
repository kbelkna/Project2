# Code to create README file for github repo

rmarkdown::render("Project2.Rmd",
                  output_format = "github_document",
                  output_file = "README.md",
                  output_options = list(
                                    toc = TRUE,
                                    toc_depth = 2
                                    )
)
