# UCS Satellite Database Interactive Visualization

An R Shiny app for exploring the Union of Concerned Scientists (UCS) Satellite Database — 6,718 satellites in orbit as of January 1, 2023.

## Features
- **World heatmap** — satellites shaded by country of operator/owner.
- **Time series** — satellites launched per year, one line per country (top 8 by volume, or up to 10 you pick).
- **Filters** — launch date range, country, user type (Civil / Commercial / Government / Military), class of orbit (LEO, MEO, GEO, Elliptical), and purpose.
- **Downloadable user guide** available from inside the app.

## Data
- Source: [UCS Satellite Database via Kaggle](https://www.kaggle.com/datasets/sujaykapadnis/every-known-satellite-orbiting-earth)
- File: `UCS Satellite Database Interactive Visualization/UCS-Satellite-Database-1-1-2023.csv`
- Launch dates span **November 15, 1974** to **December 28, 2022**.

## Technologies Used
- **R**: [R Language](https://www.r-project.org/)
- **Shiny**: [Shiny Web Framework](https://shiny.posit.co/)
- **ggplot2**: [ggplot2 for Data Visualization](https://ggplot2.tidyverse.org/)
- **dplyr**: [dplyr for Data Manipulation](https://dplyr.tidyverse.org/)
- **maps**: [maps for World Map Data](https://cran.r-project.org/package=maps)

## Getting Started

Clone the repository:

```bash
git clone https://github.com/Kashfy/UCS-Satellite-Database-Interactive-Visualization.git
```

Install the required packages from an R console:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "maps"))
```

## Running the App

The app reads the CSV with a relative path, so it must run with the project subdirectory as the working directory:

```bash
cd "UCS-Satellite-Database-Interactive-Visualization/UCS Satellite Database Interactive Visualization"
```

```bash
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

In RStudio, open `KASHFYGAZI_PROJECT3.Rproj`, then open `app.R` and click **Run App**.

## Author/Contributions
This project was done by [Kashfy](https://github.com/Kashfy).
