# StrainHub
<h4 align = "right">Adriano de Bernardi Schneider, Ph.D.<br> Colby T. Ford, Ph.D.<br>John Williams<br> Mike Cioce<br>Daniel Janies, Ph.D.</h3>

## About
Strainhub is designed as a web-based software to generate disease transmission networks and associated metrics from a combination of a phylogenetic tree and a metadata associated file. The software maps the metadata onto the tree and performs a parsimony ancestry reconstruction step to create links between the associated metadata and enable the construction of the network.

![StrainHub CHIKV Network](https://github.com/supramap/transmission_graphs/raw/master/chikv_StrainHub_network.png "Sample Chikungunya Virus Network")


## Use StrainHub Online
You'll need 2 files to get started
1) A phylogenetic tree formatted in Newick tree format generated through your preferred phylogenetic search method (e.g. BEAST, TNT, RAxML, IQTree).

2) A metadata associated file formatted as a comma separated value (CSV) file that includes headers, has the Accession number as the first column and the metadata associated values (e.g. host, country, risk group)

For more information, click [here](ABOUT.md).

[Try Out StrainHub Online](https://colbyford.shinyapps.io/strainhub/)

## Run StrainHub Locally
To run, download the repository and open the `strainhub/app.R` file and run the following script.
```r
library(shiny)
runApp()
```
