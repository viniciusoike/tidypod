# tidypod

# Overview

This is a repository for the {tidypod} package. This package offers helper functions to read the microdata from São Paulo's Origin Destination Survey. It also offers an easy alternative to quickly import socioeconomic and travel data aggregated at the OD zones level. Currently, the package supports only the most recent 2017 OD survey.

## Installation

Make sure to have either `remotes` or `devtools` installed.

```{r}
#| eval: false

# Install using remotes
remotes::install_github("viniciusoike/tidypod")

# To force the package to update to the most recent version
remotes::install_github("viniciusoike/tidypod", force = TRUE)
```

## Usage

This package currently has two main functions `import_pod_tables` and `import_pod_travel_tables` that download summary POD tables into R. By default the output will be either a single tibble or a named list of tibbles. Setting `geo = TRUE` will change the output to a spatial data.frame, i.e., a `sf` object.

```{r}
# eval: false

# To get a single table
import_pod_tables(tables = "income")

# To get all tables
import_pod_tables(tables = "all")

# Import with geometry
import_pod_tables(tables = "income", geo = TRUE)
```

This package also exports some useful shapefiles as `sf` objects

```{r}
sp_dstr <- subset(districts, code_muni == 36)
plot(sp_dstr, main = "São Paulo Districts")
```

Finally, this package also has a helper function to read POD microdata

```{r}
#| eval: false

# Read microadata using haven
pod <- haven::read_sav("OD_2017_v1.sav")

# Identify microdata at the household level
podhh <- design_pod(pod, id = "household")
```

## Further Developments

-   More tables including microdata.

-   Make `design_pod` support travel microdata.

-   Backwards compatibility with previous OD surveys.
