
# remove previous binary build files
unlink(c("src-i386", "src-x64"), recursive = TRUE)
unlink(c("doc","Meta","inst/doc", "inst/Meta"), recursive = TRUE)

# run CHECK
Sys.setenv(CI="TRUE")
devtools::check(vignettes = FALSE)
Sys.setenv(CI="")

# update files for build process
devtools::document()
devtools::document()
devtools::install(upgrade="never")
devtools::build_vignettes(quiet=FALSE, install=FALSE)

# build package files
unlink(list.files("src/*.o"))
devtools::build()
#devtools::build(vignettes = FALSE)
devtools::build(binary=TRUE, vignettes = FALSE)
