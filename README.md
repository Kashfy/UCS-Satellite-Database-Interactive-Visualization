# UCS Satellite Database Interactive Visualization

**[▶ Open the live app](https://kashfy.github.io/UCS-Satellite-Database-Interactive-Visualization/)**

> The first load takes a little while — R itself is downloaded and started inside your browser. It is cached after that, so subsequent visits are fast. No server required.

## Features
- Interactive data visualization of satellite information.
- User-friendly interface for analyzing satellite data.
- Filter by launch date range, country of operator, user type, class of orbit, and purpose.
- A world choropleth of satellites per country, plus a per-year launch trend line for the top countries.

## Technologies Used
- **R**: [R Language](https://www.r-project.org/)
- **Shiny**: [Shiny Web Framework](https://shiny.rstudio.com/)
- **ggplot2**: [ggplot2 for Data Visualization](https://ggplot2.tidyverse.org/)
- **dplyr**: [dplyr for Data Manipulation](https://dplyr.tidyverse.org/)
- **maps**: [maps for World Map Data](https://cran.r-project.org/package=maps)
- **shinylive**: [shinylive for Serverless Deployment](https://posit-dev.github.io/r-shinylive/)

## Running It Locally
Clone the repository and install the required packages:

```bash
git clone https://github.com/Kashfy/UCS-Satellite-Database-Interactive-Visualization.git
cd UCS-Satellite-Database-Interactive-Visualization
```

In R:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "maps"))
shiny::runApp("UCS Satellite Database Interactive Visualization")
```

## Deployment
The live site is a [shinylive](https://posit-dev.github.io/r-shinylive/) build: the Shiny app is compiled to WebAssembly and R runs entirely client-side, which is what allows a normally server-bound Shiny app to be hosted on GitHub Pages. The build and deploy are handled automatically by [`.github/workflows/deploy-pages.yml`](.github/workflows/deploy-pages.yml).

To produce the static site yourself:

```r
install.packages("shinylive")
shinylive::export("UCS Satellite Database Interactive Visualization", "_site")
# The User Guide link points at a real file on the server, because the app's own
# files are only reachable through shinylive's service worker and the browser's
# download manager bypasses it.
file.copy("UCS Satellite Database Interactive Visualization/www/User_Guide.docx",
          "_site/User_Guide.docx", overwrite = TRUE)
httpuv::runStaticServer("_site")
```

## Data
[UCS Satellite Database](https://www.kaggle.com/datasets/sujaykapadnis/every-known-satellite-orbiting-earth), current as of 1 January 2023. Launches span 15 November 1973 to 28 December 2022.

## Author/Contributions
This project was done by [Kashfy](https://github.com/Kashfy).
