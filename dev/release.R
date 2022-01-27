
# remove previous binary build files
unlink(c("src-i386", "src-x64"), recursive = TRUE)
unlink(c("Meta","inst/doc", "inst/Meta"), recursive = TRUE)

# run CHECK
Sys.setenv(CI="TRUE")
devtools::check(vignettes = FALSE)
Sys.setenv(CI="")

# update files for build process
devtools::document()
devtools::document()
devtools::install(upgrade="never", quick=TRUE)
devtools::build_vignettes(quiet=FALSE, install=FALSE)
if(!dir.exists("docs")) dir.create("docs")
file.copy(list.files("doc", pattern="*.html", full.names=TRUE), "docs")
unlink("doc", recursive = TRUE)

# build package files
unlink(list.files("src", pattern="*.o", full.names = TRUE))
devtools::build() # for CRAN
#devtools::build(vignettes = FALSE)
devtools::build(binary=TRUE, vignettes = FALSE) # for local testing
