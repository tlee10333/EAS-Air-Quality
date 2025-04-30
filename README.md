# EAS-Air-Quality
### Trinity Lee
In this directory, R functions exist that were developed and used to generate the graphs used for the EAS Final Project for Air Quality Analysis. 
You can easily clone this repository and use the functions in `main_helpers.r`. 

## Setup
To get started, clone this repository:

```bash
git clone git@github.com:tlee10333/EAS-Air-Quality.git
cd EAS-Air-Quality
```

To import the functions and other variables, just run this in R:

``` R
source("main_helpers.r")

```
You can see an example to run these functions in `main.Rmd`. Please note that you will need to either load or modify `./graphableData.RData` in  `main.Rmd` to have a dataframe to work with when running these. **A `.RData` is not included in this repository.**

All function documentation exist in `main_helpers.r`, but below is a summary of all the functions available and their purpose.


## Functions 
- averageAndIndividualTimeVariations - Function to generate TimeVariation Plots based on a group (individual sensors and then an overall group plot)
- timeVariationByDay - Generate TimeVariation Plots for a single sensor (Entire Week, Weekday only, and Weekend Only)
- filterBySensorData - Function which filters data based on a date range
- plotAnnulusMap - Function to plot AnnulusMap (html widget)
- plotPolarMap - Function to plot PolarMap (html widget)
- computeFullSummary - Ouputs dataframe used for Bar charts and Heatmaps by calculating mean, median, std. deviation, and 25th, 75th, and 95th percentile
- barCharts - Generates bar charts & saves the PNGs
- percentile_summary -  Internal function for heatmap
- percentileHeatmap - Generates percentile heatmaps (mean, median, 25th, 75th, and 95th percentile)
- generateNetworkMap - Generates colored dots of the network map (html widget)
- dutyCycleSensor -  Generates a Duty Cycle graph for a single sensor
- dutyCycleMultipleSensors -  Generates a Duty Cycle graph for multiple sensors
- individualMaps - Generates Polar and Annulus Plots On a map and automatically takes a screenshot of the html widget and saves it as a png. 
- pieCharts - Generates Pie Charts showing a sensor's low, medium, and high distribution in a pie chart based on manually set thresholds
- calendarPlotPNGs -  Generates Calendar Plot PNGs


Not all of these functions will generate and save PNGs, and it's documented in  `main_helpers.r` if it does generate PNGs by indicating if it returns anything. 
