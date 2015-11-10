reporttool
==========

Installation:

    ```r
    devtools::install_github("itsdalmo/reporttool")
    ```

Requirements:

-   Packages ahead of CRAN:

    ``` r
    devtools::install_github("hadley/dplyr")
    devtools::install_github("hadley/haven")
    devtools::install_github("hadley/readxl")
    devtools::install_github("hadley/tidyr")
    ```

-   Pandoc (if you don't have Rstudio, you'll need to install Pandoc): [<http://pandoc.org/installing.html>](http://pandoc.org/installing.html)

Optional:

-   For PDF output, Latex (xelatex):
    -   See link for instructions: [<http://pandoc.org/installing.html>](http://pandoc.org/installing.html)
    -   Windows: Use the Net installer for MikTeX and do a full installation.
-   For PPT outputs, you'll need to install Java and ReporteRs:
    -   [www.oracle.com](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)

    ``` r
    install.packages("ReporteRs")
    ```
