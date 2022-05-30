# DAAG
Development version of DAAG R package

The package can be installed, from a local computer, by typing

```
install.packages("remotes")
remotes::install_gitlab('daagur/DAAG', build = TRUE, build_opts = c("--no-resave-data", "--no-manual"), dependencies=FALSE)
```

Omit `build_opts = c("--no-resave-data", "--no-manual")` if you do
not wish the vignettes to be built, thus reducing the time taken
for installation.  The default for `build_opts` is 
`build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes")`.
Checks for dependencies seem best handled separately from installation
of the package.

Alternatively, with `devtools` >= 2.0.0`, specify `devtools` in place
of `remotes`.  (The arguments and defaults for `devtools::install_gitlab`
changed with version 2.0.0 of `devtools`.)

```
# Assumes devtools >= 2.0.0
devtools::install_gitlab('daagur/DAAG', build = TRUE, build_opts = c("--no-resave-data", "--no-manual"), dependencies=FALSE)
```
