
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://api.travis-ci.org/raff-k/VLSM.svg?branch=master)](https://travis-ci.org/raff-k/VLSM)

VLSM: Vector-based Landscape Metrics
====================================

Most common landscape metrics are calculated on a raster-basis. However, sometimes landscape information come along in vector-formats. For the calculation of landscape metrics, one must perform a vector-to-raster-transformation often resulting in a lost of information. In VLSM we implemented the following landscape metrics based on vector data:

-   **Effective Mesh Size** based on [Moser et al. 2007](https://doi.org/10.1007/s10980-006-9023-0)
-   **Urban Sprawl** based on [Ackermann et al. 2013](https://www.schulthess.com/buchshop/detail/ISBN-9783784340326/Ackermann-Werne-Schweiger-Manue-Sukopp-Ulrich-fuer-Naturschutz-BfN-Bundesam-Editor/Indikatoren-zur-biologischen-Vielfalt?bpmbutton211549=1&bpmtoken=)
-   **Integration Index** based on [Meinel & Winkler 2002](https://www2.ioer.de/recherche/pdf/2002_meinel_earsel.pdf)
-   **Shape Indices** based on [Forman & Godron 1986](https://link.springer.com/journal/10980)
-   **Edge Length** based on [Walz 2013](http://rosdok.uni-rostock.de/file/rosdok_disshab_0000000980/rosdok_derivate_0000005089/Habilitationsschrift_Walz_2013.pdf)
-   **Shannon's Diversity Index** for landscapes based on [McGarigal et al. 1995](https://www.fs.usda.gov/treesearch/pubs/3064)

How to use the functions in `R`, please refer to the [vignette-R](https://github.com/raff-k/VLSM/blob/master/vignettes/vignette_R.pdf). You may also want use the functions in the open-source desktop GIS `QGIS`. For this, please refer to the [vignette-QGIS](https://github.com/raff-k/VLSM/blob/master/vignettes/vignette_QGIS.pdf).

Installation
------------

You can install the latest version of VLSM from [github](https://github.com/raff-k/VLSM) with:

``` r
devtools::install_github("raff-k/VLSM")
```
