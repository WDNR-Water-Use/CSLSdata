# CSLSdata

CSLSdata is an R package containing key field measurements collected by the
Wisconsin Department of Natural Resources as part of the Central Sands Lakes
Study (CSLS), processed for use in analysis. See `inst/CSLSdata_1.0.pdf` for a
detailed description of each dataset.

To regenerate cleaned data from the scripts used to process the raw data, run
`data-raw/runall_cslsdata.R`. To explore how specific datasets are processed,
drill down from this wrapper script. Additional pointers to key scripts should
also be found in the help/metadata associated with each dataset.

## Installation

A large number of data files in this package are hosted via [Git Large File
Storage (LFS)](https://git-lfs.github.com/). You may need to install LFS in
order for some files to download properly via git.

To install or explore this R package:

  1. Use `devtools` to install the package. This will allow you to
  load the cleaned Rda data files and explore vignettes.  
  ```
  devtools::install_github("WDNR-Water-Use/CSLSdata", build_vignettes=T)
  ```
  
  2. Fork from github, clone or download ZIP, then open the cslsdata.prj file in
  R. This will allow you to also explore the raw data files and cleaning scripts
  included in "data-raw".
  
  3. csv equivalents of the Rda data files are saved under `inst/csv/`. These are
  updated every time the Rda data files are, and may be more accessible for
  non-R users or those more comfortable in other programming languages. The only
  Rda files without equivalents in this directory are the lake rasters, which
  can be found in .tif form in `data-raw/`.

## Notes

The code in `data-raw/import_water_levels.R` is used to fetch all entries from
the ArcGIS service layer noted in the URL. At somepoint, this URL started only
fetching the first 1000 entries of the full layer. Until I figure out how to
change this, I have commented out the relevant lines in
`data-raw/runall_cslsdata.R` and rely instead on what was last fetched, which
contains data through ~May 2020 (for lake levels) or ~October 2019 (for
groundwater levels). For the most complete and recent data, go directly to the
GIS service layer.
