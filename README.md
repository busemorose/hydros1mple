# hydros1mple

## Installation

The instruction for the installation and the download of R can be found on the [CRAN website](https://cran.r-project.org/).

Once R is installed, hydros1mple can be installed from [GitHub](https://github.com/busemorose/hydros1mple).

``` r
if (!require("devtools")) install.packages("devtools") # install devtools package if needed
devtools::install_github("busemorose/hydros1mple") # install hydros1mple package
```

## Launch

``` r
library(hydros1mple)
```

## Functions

- `import_km()`: import KarstMod data input files
- `PET()`: calculate potential evapotranspiration with different methods
- `plot1v()`: generate plot of different variables (discharge, precipitation, temperature, potential evapotranspiration)
- `plot2qp()`: generate plot of discharge-precipitation (double y-axis)
- `mgl_to_meq()`: convert mg/L to meq/L
- `meq_to_percent()`: convert meq/L to % (according to total anion/cation)
- `transform_piper_data()`: transform data in mg/L to coordinates on the Piper diagram
- `blank_ggplot_piper()`: empty Piper diagram
- `snow_routine()`: snow routine with temperature, precipitation and snow parameters
- `run_snow_routine()`: run the snow routine with subcatchments
- `score()`: calculate different performance criteria between sim and obs
- `sample_color()`: get *n* random colors with or without including grey shades
- `km_properties()`: extract information from KarstMod *.properties* files
- `cdf()`: cumulative distribution function of a variable
- `insight()`: helping function for analysing sim/obs discharge from hydrological models
- `score_year()`: generate plot to evaluate a model by hydrological year on different subregimes
