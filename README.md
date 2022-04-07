# Plot Tracks Colored By Attribute

MoveApps

Github repository: github.com/movestore/PlotTracksColoredByAttribute

## Description
Each segment of the plotted track(s) can be colored by the values of any of the available attributes associated to the locations. Single or multiple individuals can be displayed simultaneously.

## Documentation
This App is embedded in an shiny UI, enabling the user to interactively select the attribute by which the segments of the track should be colored, the individuals to be display, the color gradient and if the tracks should be displayed all in one panel, or in multiple panels (one per individual). 

The created plot can than be saved locally via the `Save plot` button. By default the size of the saved plot depends on the screen of the users device. Optionally the plot can be saved with a width and height set by the user. 


### Input data
moveStack in Movebank format

### Output data
moveStack in Movebank format

### Artefacts
none

The `Save Plot` button enables a local download of the created plot.

### Parameters
`Select attribute`: one attribute from the drop down list can be selected. All available attributes associated to the locations of the study are displayed.

`Choose display mode`: the tracks can be either displayed on a `Single panel` or on a `Multipanel`, were each individual is displayed on one panel. Currently the plots in the multipanel display have distorted aspect ratio, i.e. it is not 1/1.

`Select colour`: the `low`, `mid` and `high` colors can be chosen to create a customized color scale for continuous attributes. Currently the colors for the display of categorical attributes cannot be customized, default colors will be displayed.

`Select individuals`: one or multiple individuals can be selected.

`Save Plot`: locally downloads the current plot.

`Width(mm)` & `Height(mm)`: optional. Save plot with personalized width and hight in mm.

`Store current configuration`: save the current configuration for future use in the same workflow.

### Null or error handling
**Data**: For use in further Apps the input data set is returned unmodified. Empty input will give an error.
