# Melbourne Bike Trails: Weather Effects on Bicycle Traffic 🚲☔️🌡️

A comprehensive analysis of how weather conditions affect bicycle traffic and trail usage in Melbourne, with interactive maps and analytics.

## 📊 Overview

This repository contains data, analysis, and visualizations examining how Melbourne's bicycle traffic is affected by seasonal and shorter-term weather conditions. The project investigates how inclement weather leads to changes in cycling patterns, route choices, and transport mode switches.

## 🌦️ Background

In an article reported by Jonathan Corcoran and colleagues in *The Conversation* and later highlighted by *ABC News*, research shows that bad weather can significantly impact planned journeys, leading to rescheduling, rerouting, or cancellation. This results in increased traffic congestion, more accidents, travel delays, mental stress, environmental pollution, and general travel dissatisfaction.

People who travel by bike or on foot are most likely to change their travel plans or switch to alternative transport modes rather than arrive at their destination drenched and uncomfortable. Some cities are trying to combat this issue by introducing innovations such as heated bicycle lanes and sheltered walkways.

This project was developed to investigate how bicycle traffic in Melbourne is affected by both seasonal weather patterns and shorter-term weather conditions.

## 📁 Repository Structure

-   `data/`: Contains raw and processed datasets 📈
    -   `combCondLoc.csv`
    -   `dataBike_Day.csv`
    -   `dataBike_Loc1`
    -   `dataBike_Loc2`
    -   `dataBike_Loc3`
    -   `Loc.csv`
-   `markdown/`: Text files 📓
    -   `code.rmd`: File containing code
    -   `readme.rmd`: Readme file for GitHub site
-   `app.R`: Shiny application file 🤖


## 🚀 Getting Started

### Prerequisites

To run this project, you need to have the following installed:

-   R (version 4.0+ recommended)
-   RStudio (optional but recommended)
-   Shiny package

### Required R Packages

Ensure you have the required R packages installed by running:

``` r
install.packages(c("shiny", "ggplot2", "leaflet", "dplyr", "readr", "lubridate"))
```

## 📚 Running the Shiny App

1.  Clone the repository to your local machine:

    ``` sh
    git clone https://github.com/capgalea/melb_bike_trails.git
    cd melb_bike_trails
    ```

2.  Open RStudio and set the working directory to the project folder.

3.  Run the Shiny app by executing the following in R:

    ``` r
    library(shiny)
    runApp("app.R")
    ```

4.  The app will launch in your default web browser, where you can explore interactive maps and analytics on Melbourne’s bike trails and weather effects.

## 🔧 Contributing

Contributions are welcome! If you have suggestions, bug fixes, or new features, feel free to open an issue or submit a pull request.

## 🌍 License

This project is licensed under the MIT License. See the LICENSE file for details.

## 📅 References

1.  Corcoran, J. et al., *The Conversation*
2.  *ABC News* article on weather effects on travel

------------------------------------------------------------------------

Happy cycling! 🚲
