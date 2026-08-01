# UCS Satellite Database Interactive Visualization

An R Shiny app for exploring the Union of Concerned Scientists (UCS) Satellite Database — 6,718 satellites in orbit as of January 1, 2023.

## Features
- **World heatmap** — satellites shaded by country of operator/owner.
- **Time series** — satellites launched per year, one line per country (top 8 by volume, or up to 10 you pick).
- **Filters** — launch date range, country, user type, class of orbit, and purpose. See [Controls](#controls).
- **Downloadable user guide** available from inside the app.

## Controls

All five filters live in the left sidebar and combine with AND logic — a satellite has to satisfy every active filter to appear in either chart.

### Start Date / End Date

Two date pickers in `MM/DD/YYYY` format, filtering on **Date of Launch**. They default to the full span of the dataset and cannot be dragged outside it.

This is the date each satellite went up, not a measure of whether it is still operational. Every satellite in the file was in orbit as of January 1, 2023, so narrowing the range answers "what was launched during this window," not "what was flying during this window."

### Countries

**Select All Countries** is checked by default. Unchecking it reveals a multi-select listing the 76 countries that actually appear in the data, ordered by satellite count.

Country means **Country of Operator/Owner** — who runs the satellite, not who built or launched it. When the source file lists several operators, only the first is used, so `USA/Japan` is counted as USA.

Two caveats worth knowing:

- The label says *max 10 for the line plot*. That limit is real but applies only to the time series, which silently plots the first 10 you selected. The heatmap always uses everything you picked.
- `Multinational` and `NR` (not registered) can't be matched to a country on the map, so they're excluded from the heatmap. They still count in the time series.

### User Type

A radio button — one choice at a time — offering **All**, **Civil**, **Commercial**, **Government**, and **Military**.

| Value | Meaning |
|---|---|
| Civil | Academic, scientific, and non-profit operators |
| Commercial | Private-sector and for-profit operators |
| Government | Non-military state agencies |
| Military | Defence and armed-forces operators |

The match is exact, which matters more than it sounds. Roughly 300 satellites carry a shared designation like `Government/Commercial` or `Military/Civil`, and those are **not** included when you select either component on its own — only **All** shows them.

### Class of Orbit

Checkboxes, all ticked by default. **At least one must stay ticked**; clear them all and both charts are replaced by a red prompt.

| Class | Altitude | Period | What it's for | Count |
|---|---|---|---|---|
| **LEO** — Low Earth Orbit | ~160–2,000 km | ~90 min | The busy shell. Imaging, remote sensing, the ISS, and large broadband constellations like Starlink. Close enough for high-resolution imagery and low latency, but each satellite is only overhead for minutes at a time, so coverage needs many of them. | 5,937 |
| **GEO** — Geostationary | 35,786 km | 24 h | Orbits directly above the equator at exactly the speed Earth rotates, so it appears to hang motionless over one spot. Ground dishes can point at it and never move again — hence traditional TV, weather, and comsats. | 580 |
| **MEO** — Medium Earth Orbit | ~2,000–35,786 km | 2–12 h | Between the two. Wide footprint with a manageable constellation size, which is why navigation systems live here: GPS, Galileo, GLONASS, BeiDou. | 142 |
| **Elliptical** | Varies | Varies | Deliberately non-circular, so the satellite lingers near apogee and races through perigee. Molniya-type orbits use this to cover high latitudes that GEO serves poorly, since a satellite over the equator sits near the horizon from the far north. | 59 |

### Purpose

**Select All Purposes** is checked by default; unchecking it reveals a multi-select of the 30 purpose values in the data. Communications (4,812) and Earth Observation (1,146) dominate, with Technology Development, Navigation, and Space Science making up most of the rest.

As with User Type, matching is exact. Combined entries such as `Earth Observation/Technology Development` are their own separate options and won't appear under either single purpose.

## Reading the Charts

**World heatmap** — countries shade light blue to dark blue by satellite count. **Gray means no data**: either the country has no satellites matching your filters, or it's an entry the map can't place. The scale rebuilds itself on every filter change, so shades are only comparable within a single view.

**Time series** — satellites launched per year, one line per country. Which countries appear depends on the country filter: with **Select All** on it shows the top 8 by total volume within your other filters, and with specific countries chosen it shows up to the first 10 of them. Gaps in a line mean zero launches that year, not missing data.

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
