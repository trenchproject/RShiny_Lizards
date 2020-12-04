
guide1 <- Cicerone$
  new()$
  step(
    el = "viz-wrapper",
    title = "Visualization",
    description = HTML("Welcome to the guided tour. We will be going over the distribution map and how to use it.")
  )$
  step(
    el = "map-wrapper",
    title = "Distribution map",
    description = HTML("The current map shows the thermal safety margin of <i>Anolis carolinensis</i> in the sun at normal condition. 
                       Notice that the greyed area outside of the map says January and 01 PM. This is because the facets are set to be <b>Month</b> and <b>Hour</b>. We'll go over them later.
                       <br>The legend on the right informs how the colors correspond to TSM. 
                       You can see that <i>A. carolinensis</i> would be in danger in parts of Florida.
                       <br>Try clicking on any part of the map that is colored and hit next."),
    position = "left"
  )$
  step(
    el = "info",
    title = "Temperature data",
    description = HTML("Here shows the specific temperature of that clicked location. If the values say NaN, go back and click somewhere else where it's colored.")
  )$
  step(
    el = "density-wrapper",
    title = "Density plot",
    description = HTML("This plot lets you see how much the species distribution range is in danger at a quick glance. The x axis is TSM and the y axis is relative frequency.
                       The more the colored area to the left, the greater the distribution range is under risk."),
    position = "left"
  )$
  step(
    el = "facet-wrapper",
    title = "Setting facets",
    description = HTML("Multiple plots can be shown at once using these selects. For example, try setting the vertical facets to <b>Shade</b> while keeping the horizontal facets at <b>Month</b>.
                       ")
  )$
  step(
    el = "time-wrapper",
    title = "Setting the month and hour",
    description = HTML("Keep <b>January</b> on and select <b>April</b>. The hour can be kept at 1 pm. Hit next."),
    position = "right"
  )$
  step(
    el = "var-wrapper",
    title = "Scenario and shade",
    description = HTML("Scenario allows for a simulation of 1.5 &deg;C and 2 &deg;C increase in air temperature.
                       Shade determines the lizards exposure to the sun. Check all three under <b>Shade</b> for now and hit next.")
  )$
  step(
    el = "run",
    title = "Run",
    description = HTML("After changing any variables, hit this button to map the changes. Hit Run!")
  )$
  step(
    el = "map-wrapper",
    title = "New map",
    description = HTML("The process can take a bit to run so be patient.
                       <br>After the data process is complete, we can see multiple maps sorted by months and shades, which we specified as facets.
                       In April, it looks like the whole distribution is too hot for the lizards when exposed but thermoregulaion would prevent them from overheating."),
    position = "left"
  )$
  step(
    el = "density-wrapper",
    title = "New density plot",
    description = HTML("The density plots have changed as well. As you can see, even in April, lizards in any part of their distribution are able to thermoregulate to secure a safe temperature.")
  )$
  step(
    el = "scale-wrapper",
    title = "Discrete vs. continuous scale",
    description = HTML("This switch toggles between discrete and continuous scales on the map. Discrete scale is useful to view areas that are under risk at a quick glance. 
                       Continuous scale is useful to see the gradient of TSM throughout the distribution. Click on the bar once and hit next.")
  )$
  step(
    el = "map-wrapper",
    title = "Continuously-scaled map",
    description = HTML("The map is colored differently now. Warmer color corresponds to smaller TSM.")
  )$
  step(
    el = "map-switch",
    title = "Show world map",
    description = HTML("Turn this switch on to show a world map.")
  )$
  step(
    el = "mymap",
    title = "World map",
    description = HTML("The blue rectangle shows where the species distribution is in the world. If you didn't know by now, <i>A. carolinensis</b> inhabits southeastern United States.")
  )$
  step(
    el = "species-wrapper",
    title = "Species",
    description = HTML("Last but not least, this is where you can select other species to map its distribution and view its TSMs.
                       <br>That's it for the tour!")
  )



guide2 <- Cicerone$
  new()$
  step(
    el = "plot2",
    title = "Hourly distribution plot",
    description = HTML("This plot shows the change in TSM of <b>Anolis Carolinensis</b> throughout the day. Each hour has three overlapping plots, each color representing a warming scenario.
                       The shape of the curves is determined by the distribution of TSM within their range. 
                       For example, if you take a look at the curves at 11pm, most of the colored area are weighed on the right, and a small proportion of it extends to the left. 
                       This means that most of the lizards' range has TSM close to 40 &deg;C while a tiny proportion of the area that is the warmest has a TSM of 25-30 &deg;C."),
    position = "left"
  )$
  step(
    el = "plotvar-wrapper",
    title = "Variables",
    description = HTML("Selecting <b>Scenario</b> for facets will compare plots based on warming offset instead of by exposure to sun. Other months can be plotted here as well.")
  )