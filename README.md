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

`curl -X POST -d 'human_data=iso%2Csubarea%2Chdi%2Cpopulation%2Cfraction_urban_pop%2Cfraction_pop_under5%2CsheddingRate%2Cshedding_duration%2Cincidence_urban_under5%2Cincidence_urban_5plus%2Cincidence_rural_under5%2Cincidence_rural_5plus%2CflushSewer_urb%2CflushSeptic_urb%2CflushPit_urb%2CflushOpen_urb%2CflushUnknown_urb%2CpitSlab_urb%2CpitNoSlab_urb%2CcompostingTwinSlab_urb%2CcompostingTwinNoSlab_urb%2CcompostingToilet_urb%2CbucketLatrine_urb%2CcontainerBased_urb%2ChangingToilet_urb%2CopenDefecation_urb%2Cother_urb%2CcoverBury_urb%2CsewageTreated_urb%2CfecalSludgeTreated_urb%2CisWatertight_urb%2ChasLeach_urb%2CemptyFrequency_urb%2CpitAdditive_urb%2Curine_urb%2ConsiteDumpedland_urb%2CflushSewer_rur%2CflushSeptic_rur%2CflushPit_rur%2CflushOpen_rur%2CflushUnknown_rur%2CpitSlab_rur%2CpitNoSlab_rur%2CcompostingTwinSlab_rur%2CcompostingTwinNoSlab_rur%2CcompostingToilet_rur%2CbucketLatrine_rur%2CcontainerBased_rur%2ChangingToilet_rur%2CopenDefecation_rur%2Cother_rur%2CcoverBury_rur%2CsewageTreated_rur%2CfecalSludgeTreated_rur%2CisWatertight_rur%2ChasLeach_rur%2CemptyFrequency_rur%2CpitAdditive_rur%2Curine_rur%2ConsiteDumpedland_rur%2CFractionPrimarytreatment%2CFractionSecondarytreatment%2CFractionTertiarytreatment%2CFractionQuaternarytreatment%2CFractionPonds%0A1%2CCentral%2C0.493%2C63206%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.263426061%2C0.202395075%2C0.057237316%2C0.012221734%2C0.007753772%2C0.41097414%2C0.02447696%2C0%2C0%2C0.006835904%2C0%2C0%2C0%2C0.011578119%2C0.003100918%2C0%2C1%2C0.436780403%2C0%2C0.733457483%2C3%2CNone%2CFresh%20Urine%2C0.1%2C0.263426061%2C0.202395075%2C0.057237316%2C0.012221734%2C0.007753772%2C0.41097414%2C0.02447696%2C0%2C0%2C0.006835904%2C0%2C0%2C0%2C0.011578119%2C0.003100918%2C0%2C1%2C0.436780403%2C0%2C0.733457483%2C3%2CNone%2CFresh%20Urine%2C0%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0A2%2CKawempe%2C0.493%2C333024%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.036860622%2C0.142274476%2C0.057601288%2C0.001679583%2C0.00524847%2C0.736130931%2C0.014416174%2C0%2C0%2C0.00033069%2C0%2C0%2C0.0001061%2C0.00459866%2C0.000745624%2C0%2C1%2C0.211244894%2C0%2C0.705524168%2C3%2CNone%2CFresh%20Urine%2C0.1%2C0.036860622%2C0.142274476%2C0.057601288%2C0.001679583%2C0.00524847%2C0.736130931%2C0.014416174%2C0%2C0%2C0.00033069%2C0%2C0%2C0.0001061%2C0.00459866%2C0.000745624%2C0%2C1%2C0.211244894%2C0%2C0.705524168%2C3%2CNone%2CFresh%20Urine%2C0%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0A3%2CMakindye%2C0.493%2C385309%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.028570368%2C0.259280693%2C0.082540034%2C0.003821692%2C0.017477095%2C0.574689697%2C0.024289707%2C0%2C0%2C0.001400786%2C0%2C0%2C3.45E-05%2C0.006082428%2C0.001834099%2C0%2C1%2C0.236410709%2C0%2C0.740031033%2C3%2CNone%2CFresh%20Urine%2C0.1%2C0.028570368%2C0.259280693%2C0.082540034%2C0.003821692%2C0.017477095%2C0.574689697%2C0.024289707%2C0%2C0%2C0.001400786%2C0%2C0%2C3.45E-05%2C0.006082428%2C0.001834099%2C0%2C1%2C0.236410709%2C0%2C0.740031033%2C3%2CNone%2CFresh%20Urine%2C0%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0A4%2CNakawa%2C0.493%2C317023%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.098606697%2C0.282278627%2C0.065945258%2C0.002724668%2C0.016024352%2C0.49795729%2C0.027461236%2C0%2C0%2C0.000559863%2C0%2C0%2C0.000678506%2C0.006932757%2C0.000831023%2C0%2C1%2C0.190629622%2C0%2C0.788037462%2C3%2CNone%2CFresh%20Urine%2C0.1%2C0.098606697%2C0.282278627%2C0.065945258%2C0.002724668%2C0.016024352%2C0.49795729%2C0.027461236%2C0%2C0%2C0.000559863%2C0%2C0%2C0.000678506%2C0.006932757%2C0.000831023%2C0%2C1%2C0.190629622%2C0%2C0.788037462%2C3%2CNone%2CFresh%20Urine%2C0%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0A5%2CRubaga%2C0.493%2C383216%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.004654201%2C0.164638786%2C0.089541423%2C0.001799026%2C0.010791608%2C0.688829209%2C0.031009589%2C0%2C0%2C0.000566465%2C0%2C0%2C2.01E-05%2C0.003735057%2C0.004392258%2C0%2C1%2C0.243857358%2C0%2C0.98927109%2C3%2CNone%2CFresh%20Urine%2C0.1%2C0.004654201%2C0.164638786%2C0.089541423%2C0.001799026%2C0.010791608%2C0.688829209%2C0.031009589%2C0%2C0%2C0.000566465%2C0%2C0%2C2.01E-05%2C0.003735057%2C0.004392258%2C0%2C1%2C0.243857358%2C0%2C0.98927109%2C3%2CNone%2CFresh%20Urine%2C0%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0A&isoraster=http://data.waterpathogens.org/dataset/c4a395f1-5e48-4109-b02f-951b8be89275/resource/ce447a61-b998-45df-a142-bb18273ce652/download/isoraster_kla_div.tif&popurban=http://data.waterpathogens.org/dataset/c4a395f1-5e48-4109-b02f-951b8be89275/resource/02355eae-65b5-45b8-b52b-4f6d06328351/download/popurban_kla.tif&poprural=http://data.waterpathogens.org/dataset/c4a395f1-5e48-4109-b02f-951b8be89275/resource/1f3803bb-14a1-4ede-a9a9-27c035be0700/download/poprural_kla.tif&wwtp=http://data.waterpathogens.org/dataset/c4a395f1-5e48-4109-b02f-951b8be89275/resource/5844b1dc-fd2c-4f85-9677-25f54f1fccd9/download/wwtp_input_data_kla.csv'  "http://127.0.0.1:9262/scenario" -H  "accept: application/json"`

