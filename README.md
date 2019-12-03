# MITUSCalibPlots Package

This package facilitates the generation of the comparison to recent 
data plots that are available in tabby2. 

By having the code available here in a standalone package, we allow the
developers of it to work on it without having to get into the details of the
MITUS / Tabby2 packages if they aren't already familiar. 

# Setup

Install the package dependencies, install using devtools. 

    git clone git@github.com:ppml/mitus_calib_plots.git
    cd mitus_calib_plots/
    R

    # in R:
    > devtools::install("./", dependencies=TRUE)
