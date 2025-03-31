
# remove previous binary build files
unlink(c("src-i386", "src-x64"), recursive = TRUE)
unlink(c("Meta","inst/doc", "inst/Meta"), recursive = TRUE)

# run CHECK
Sys.setenv(CI="TRUE") #skip long-running tests
devtools::check(vignettes = FALSE)
Sys.setenv(CI="")

# update files for build process
devtools::document()
devtools::document()
devtools::install(upgrade="never", quick=TRUE)

# test-build the vignettes
#devtools::build_vignettes(quiet=FALSE, install=FALSE)
#unlink("doc", recursive = TRUE)

# pre-knit verification vignette, because it takes too long to do on the fly
withr::with_dir("vignettes", knitr::knit("orig/lemna-verification.Rmd", output = "lemna-verification.Rmd"))

# copy HTML versions of the vignettes for the Github pages
rmarkdown::render("vignettes/lemna-introduction.Rmd", output_format="html_document", output_dir="docs")
rmarkdown::render("vignettes/lemna-verification.Rmd", output_format="html_document", output_dir="docs")


# build package files
unlink(list.files("src", pattern="*.o", full.names = TRUE))
devtools::build() # for CRAN
#devtools::build(vignettes = FALSE)
devtools::build(binary=TRUE, vignettes = FALSE) # for local testing
