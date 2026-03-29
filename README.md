# UCS Satellite Database Interactive Visualization

An interactive web application for exploring and analyzing satellite launch data across countries and time periods.

## 📋 Project Overview

This project is an interactive visualization tool built for my Introduction to Data Science class. It provides an intuitive interface to explore the Union of Concerned Scientists (UCS) satellite database, allowing users to discover patterns in satellite launches by country, orbit class, and time periods.

## ✨ Features

- **Multi-country filtering** - View satellite launches by specific countries or entities
- **Orbit classification filters** - Analyze satellites by orbit type and class
- **Time period selection** - Explore launch trends across different time ranges
- **Interactive visualizations** - Dynamically updated charts and plots
- **User-friendly interface** - Intuitive controls for data exploration

## 🛠️ Technologies & Tools

- **Language:** R
- **Key Packages:**
  - [Shiny](https://shiny.rstudio.com/) - Interactive web application framework
  - [dplyr](https://dplyr.tidyverse.org/) - Data manipulation and wrangling
  - [ggplot2](https://ggplot2.tidyverse.org/) - Data visualization
  - [maps](https://cran.r-project.org/web/packages/maps/) - Geographic mapping

## 📊 Data Source

This project utilizes the Union of Concerned Scientists (UCS) satellite database, providing comprehensive information about satellites launched into orbit.

## 🚀 Getting Started

### Prerequisites
- R (version 3.5+)
- Required packages: shiny, dplyr, ggplot2, maps

### Installation

```r
install.packages(c("shiny", "dplyr", "ggplot2", "maps"))
```

### Running the Application

```r
shiny::runApp("path/to/app")
```

## 📁 Project Structure

```
UCS-Satellite-Database-Interactive-Visualization/
├── README.md
├── app.R                    # Main Shiny application
└── data/                    # Data files
```

## 📚 Usage

1. Launch the application using the command above
2. Select countries/entities from the filter menu
3. Choose orbit classes of interest
4. Adjust time period range to explore specific launch windows
5. Interact with the visualizations to discover insights

## 📝 License

[Add your license here if applicable]

## 👤 Author

Kashfy

## 🤝 Contributions

Contributions are welcome! Feel free to open issues or submit pull requests.

---

**Note:** This was a class project for Introduction to Data Science.