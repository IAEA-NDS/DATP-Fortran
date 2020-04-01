## About DATP

The DATP code preprocesses experimental data for the evaluation
of neutron cross sections. It reduces/compresses the information of
experimental data sets given on disjoint sets of energies to a common
energy grid. The reduced data sets can then be used in the generalized
least squares code GMAP. Both codes, DATP and GMAP were developed by
Wolfgang P. Poenitz. Notably, they have been employed for the evaluation
of [neutron cross section standards]. Among other available documents,
there is a [short user guide] for DATP and GMAP and a more detailed
[report] on the technicalities of the codes.

[neutron cross section standards]: https://www-nds.iaea.org/standards/
[short user guide]: https://www-nds.iaea.org/standards/Codes/GMA-User-Guide.pdf
[report]: https://www-nds.iaea.org/standards/Reports/ANL-NDM-139.pdf

## About this repository

The purpose of this repository is to track changes of DATP effected by
the owner of this repository. So far the effected modifications are:

- An uninitialized variable made the proper working of the code
  dependent on a compiler flags to initialize it to zero.
  Not all compilers do this by default, e.g., GNU Fortran.
- A strict comparison of variables of type real made branching
  dependent on the technical details of arithmetic computations,
  e.g., different order of calculation steps that should mathematically
  lead to the same result.
- The preallocated size of some arrays has been increased to
  allow for a larger number of measurement points per experiment.
- The manual input of the value for variable ULI (threshold for outlier
  detection) has been replaced by a default value.
