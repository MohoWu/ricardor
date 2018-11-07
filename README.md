# ricardor

**ricardor** contains a suite of functions that are useful for various projects 
at Ricardo. These functions can range from importing data, data wrangling, 
plotting and modelling.


## Installation

You can install the development version of **ricardor** from Github with:

``` r
devtools::install_github("mohowu/ricardor")
```

## Examples

### Plotting functions

As HTML documents are becoming more and more popular, one advantage of HTML ouput over
traditional PDF or Word is that you can embed interative plots. `ricardor` contains 
some high level plotting functions leveraging on some most popular interactive plotting
packages but have the boilerplates set up for you. These functions are prefixed 
with `plot_` for easy searching.

* `plot_leaflet`: Plot a leaflet map with `sp` object, with customisable popup text 
legend options.
* `plot_dygraph`: Plot a interactive timeseries with `dygraph`. 
* `plot_calendar`: A ggplot version of calendar plot. It's easy to convert this 
to an interactive plot with `ploty::ggplotly`

### Modelling

Some functions that can be useful during modelling are prefixed with `var_`.

* `var_shift`: Shift variable(s) forward or backward by n observations to investigate
if there's a delay effect between the response variable and predictor variable.
* `var_select`: A stepwise variable selection algorithm aiming to maximise the 
*R*^2^ of a multiple linear regression model.

### Spatial 

Some functions dealing with spatial objects are prefixed with `sp_`.

`sp_transform`: Transform the projection system. To get code for the projection 
system, use `projection_*`.
`sp_from_data_frame`: Convert data frame to spatial object.

### String functions

Functions dealing with character strings are prefixed with `str_`.

`str_trim`: Trim leading or tailing white space in a string.
`str_underscore`: Replace space with "_" in a string.
`str_to_link`: Convert a string to a HTML link.



