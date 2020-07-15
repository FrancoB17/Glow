# GloWPa Model

This version of the GloWPa model is prepared by Nynke on 23/4/2020. It is provided to Lisanne Nauta and Gifty Abiwu on that day.

This version has not been thoroughly checked.

## Installation

Clone the git repository in your current folder and checkout mapping-tool branch:

`git clone https://git.wur.nl/glowpa/glowpa-model.git`  
`git checkout mapping-tool`

The mapping tool branch comes with the following submodules:
* pathogenflows R package (https://git.wur.nl/glowpa/pathogenflows.git)

To install the submodules type the following command:

`git submodule update`

The git repository of the submodule is now cloned in the submodule directory. Now open the GloWPa.Rproj file R studio.

Install the folowing R packages from the R console:

`devtools::install_local('./libs/pathogenflows')`

##  Updating code in submodules
In case anything needs to be changed in the submodule code make sure you make changes in the repository of the submodule. So:

1. Go to your submodule git project.
2. Change code.
3. Commit code.
4. Push code.
5. Go to GloWPa git 
6. Update submodule: `git submodule update`
7. Now the submodule reference code has been updated and needs to be committed to the GloWPa repository: `git commit -a -m "Updated submodule"`

## Running the model

### With local data
TODO
### Using API and remote data

1. Install plumber: `install.packages(plumber)`
2. Type: `api <- plump("api.R")` and then `api$run()`
3. Check the swagger specification for the endpoints.
4. Do POST call to ${baseurl}/scenario and provide data

