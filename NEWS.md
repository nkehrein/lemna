# lemna 1.0.2, 2025-03-31

* Default value of parameter `beta` modified due to typo in previous report
  versions, now conforms with Klein et al. (2025), report version 1.2 (to be published);
  new value: `beta=0.25`, old value: `beta=0.025`.
* Default value for *deSolve*'s parameter `hmax` set to `hmax=0.01`.
* Effect calculation is no longer bound to the interval ]oo, 1], instead the full
  range of rational numbers is possible.
* Internal concentration `C_int` can no longer become negative in R equations.
* Verification vignette is pre-knitted to reduce built time of the package. See
  directory `vignettes/orig/` for the original RMarkdown file.

# lemna 1.0.1, 2023-04-22

* Minor change to `lemna.c` to avoid compiler warnings on CRAN.

# lemna 1.0.0, 2022-05-10

* Documentation adapted to reference report version 1.1.
* Added a warning message in case removed parameter `BM_threshold` is used.

# lemna 0.9.2

* Minor adaption of the biomass ODE according to the draft report version 1.1,
  handling of low biomass densities was simplified to use only one parameter,
  `BM_min`.

# lemna 0.9.1

* Documentation improved and typos fixed.
* `lemna_desolve()` added which allows direct access to the ODE solver.

# lemna 0.9.0

* Initial release

