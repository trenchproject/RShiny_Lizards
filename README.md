# RShiny_Lizards

Rshiny_Lizards is an interactive shiny app that allows any user to visualize the distributions and the thermal safety margins of 15 species of lizards. Each lizard is presented with a picture and a description on their thermal strategy. Lizard optimal temperatures and critical thermal maximum are obtained from [Bennett et al., 2018](https://www.nature.com/articles/sdata201822), and their range maps are from [IUCN](https://www.iucnredlist.org/resources/spatial-data-download). 


## Prerequisites for opening in Rstudio
Git and Rstudio ([Instructions](https://resources.github.com/whitepapers/github-and-rstudio/))  
Installation of the following R packages:
shiny, ggplot2, ggridges, magrittr, plotly, shinyBS, data.table, leaflet, shinyjs, shinysky, shinythemes, shinyWidgets, maps, shinycssloaders

```
pkgs <- c('shiny', 'ggplot2','ggridges','magrittr','plotly','shinyBS', 'data.table', 'leaflet', 'shinyjs', 'shinythemes', 'shinyWidgets', 'maps', 'shinycssloaders')
lapply(pkgs, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)
devtools::install_github("AnalytixWare/ShinySky")
```

## Using RShiny_Lizards
* Opening in Rstudio:  
Click on "Code" on the top right to copy the link to this repository.  
Click ```File```, ```New Project```, ```Version Control```, ```Git```  
Paste the repository URL and click ```Create Project```.

* Alternatively, go to [this link](https://map.trenchproject.com/RShiny_Lizards/).

## Contributing to RShiny_Lizards
<!--- If your README is long or you have some specific process or steps you want contributors to follow, consider creating a separate CONTRIBUTING.md file--->
To contribute to Rshiny_Lizards, follow these steps:

1. Fork this repository.
2. Create a branch: `git checkout -b <branch_name>`.
3. Make your changes and commit them: `git commit -m '<commit_message>'`
4. Push to the original branch: `git push origin <project_name>/<location>`
5. Create the pull request.

Alternatively see the GitHub documentation on [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).
