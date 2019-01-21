# StrainHub
<h4 align = "right">Adriano de Bernardi Schneider, Ph.D.<br> John Williams<br> Mike Cioce<br> Colby T. Ford, Ph.D.<br>Daniel Janies, Ph.D.</h3>

## About
Strainhub is designed as a web-based software to generate disease transmission networks and associated metrics from a combination of a phylogenetic tree and a metadata associated file. The software maps the metadata onto the tree and performs a parsimony ancestry reconstruction step to create links between the associated metadata and enable the construction of the network.
For more information, click [here](ABOUT.md).

## Shiny Application
To run, download the repository and open the `shiny/app.R` file and run the following script.
```r
library(shiny)
runApp('strainhub')
```
