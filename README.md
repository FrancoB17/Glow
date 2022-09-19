# GloWPa Model

This version of the GloWPa model is prepared by Nynke on 23/4/2020. It is provided to Lisanne Nauta and Gifty Abiwu on that day.

This version has not been thoroughly checked.


![alt text](Model script schema 2017 v5.pdf "Schema")

## Installation

Clone the git repository in your current folder and checkout mapping-tool branch:

```console
git clone https://git.wur.nl/glowpa/glowpa-model.git
git checkout mapping-tool
```

dependencies for model:
```R
install.packages("raster","sf","sp","rgdal","logger","dplyr","tictoc","parallel","khroma")
```
additional dependencies for api:
```R
install.packages("jsonlite","plumber","rnaturalearth","rnaturalearthdata")
```

The mapping tool branch comes with the following submodules:
* pathogenflows R package (https://git.wur.nl/glowpa/pathogenflows.git forked from https://github.com/mverbyla/pathogenflows)

To install the submodules type the following commands:

```bash
git submodule init
git submodule update
```
The git repository of the submodule is now cloned in the submodule directory. Now open the GloWPa.Rproj file R studio.

Install the folowing R packages from the R console: `devtools::install_local('./libs/pathogenflows')`

##  Updating code in submodules
In case anything needs to be changed in the submodule code make sure you make changes in the repository of the submodule. So:


1. Go to your submodule git project
2. Change code

    2.a  If you want to merge updates from origial project_:
    1. Check if the original remote is added: `git remote -v`
    2. If upstream is missing: `git remote add upstream https://github.com/mverbyla/pathogenflows`
    3. Fetch changes: `git fetch upstream`
    4. Create branch `git checkout -b merge-upstream`
    5. Merge code from upstream with local code: `git merge upstream/master`
    6. Solve conflicts if needed.
    7. Commit code in your new branch: `git commit -a -m "Updates from original projects"`
    8. Go to your master branch: `git checkout master`
    9. Merge branch 'merge-upstream' into master: `git merge merge-upstream`
    10. Depending on your settings you might to commit the merge seperately.
    11. Push changes to your origin: `git push origin master`.

    2.b  If you want to make changes on your own:
    1. Commit code.
    2. Push code.
3. Go to GloWPa git 
4. Go to libs/pathogenflows
5. Run `git pull`.
6. Update the R library locally. `devtools::install_local("libs/pathogenflows/", force = T)` 
7. Test if everything works
8. Now the submodule reference code has been updated and needs to be committed to the GloWPa repository: `git commit -a -m "Updated submodule"`
9. Push to origin. `git push`

## Running the model
Set enviroment variables in local_env.R
example:
```R
model_dir <- "/home/nauta/Data/Software/GloWPa/" # not used in api mode
model_input_dir <- "/home/nauta/Data/Projects/2020/K2P/02_Experiment/02_Runs/Run0/Input/kla_online/" #not used in api mode
model_output_dir <- "/home/nauta/Data/Projects/2020/K2P/02_Experiment/02_Runs/Run0/Output/" # not used in api mode

ENV$gadm_file = "/home/nauta/Data/Data/GADM/gadm36_levels.gpkg"
ENV$csv_sep = ";"
```

### Locally

### Using API and remote data

1. Install plumber: `install.packages(plumber)`
2. Type: `api <- plumb("api.R")` and then `api$run()`
3. Check the swagger specification for the endpoints.
4. Do POST call to ${baseurl}/scenario and provide data

```shell
curl -X POST -d 'human_data=iso%2Cgid%2Csubarea%2Chdi%2Cpopulation%2Cfraction_urban_pop%2Cfraction_pop_under5%2CsheddingRate%2Cshedding_duration%2Cincidence_urban_under5%2Cincidence_urban_5plus%2Cincidence_rural_under5%2Cincidence_rural_5plus%2CflushSewer_urb%2CflushSeptic_urb%2CflushPit_urb%2CflushOpen_urb%2CflushUnknown_urb%2CpitSlab_urb%2CpitNoSlab_urb%2CcompostingToilet_urb%2CbucketLatrine_urb%2CcontainerBased_urb%2ChangingToilet_urb%2CopenDefecation_urb%2Cother_urb%2CcoverBury_urb%2CsewageTreated_urb%2CfecalSludgeTreated_urb%2CisWatertight_urb%2ChasLeach_urb%2CemptyFrequency_urb%2CpitAdditive_urb%2Curine_urb%2CtwinPits_urb%2ConsiteDumpedland_urb%2CflushSewer_rur%2CflushSeptic_rur%2CflushPit_rur%2CflushOpen_rur%2CflushUnknown_rur%2CpitSlab_rur%2CpitNoSlab_rur%2CcompostingToilet_rur%2CbucketLatrine_rur%2CcontainerBased_rur%2ChangingToilet_rur%2CopenDefecation_rur%2Cother_rur%2CcoverBury_rur%2CsewageTreated_rur%2CfecalSludgeTreated_rur%2CisWatertight_rur%2ChasLeach_rur%2CemptyFrequency_rur%2CpitAdditive_rur%2Curine_rur%2CtwinPits_rur%2ConsiteDumpedland_rur%2CLRV_virus%2CLRV_protozoa%2CFractionPrimarytreatment%2CFractionSecondarytreatment%2CFractionTertiarytreatment%2CFractionQuaternarytreatment%2CFractionPonds%0D%0A1%2CUGA.16.1.1_1%2CCentral+Division%2C0.493%2C63206%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.263426061%2C0.202395075%2C0.057237316%2C0.012221734%2C0.007753772%2C0.41097414%2C0.02447696%2C0.006835904%2C0%2C0%2C0%2C0.011578119%2C0.003100918%2C0%2C1%2C0.436780403%2C0%2C0.733457483%2C3%2C0.5%2C0.006835904%2C0.1%2C0.1%2C0.263426061%2C0.202395075%2C0.057237316%2C0.012221734%2C0.007753772%2C0.41097414%2C0.02447696%2C0.006835904%2C0%2C0%2C0%2C0.011578119%2C0.003100918%2C0%2C1%2C0.436780403%2C0%2C0.733457483%2C3%2C0%2C0.006835904%2C0%2C0%2C1%2C2%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0D%0A2%2CUGA.16.1.2_1%2CKawempe+Division%2C0.493%2C333024%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.036860622%2C0.142274476%2C0.057601288%2C0.001679583%2C0.00524847%2C0.736130931%2C0.014416174%2C0.00033069%2C0%2C0%2C0.0001061%2C0.00459866%2C0.000745624%2C0%2C1%2C0.211244894%2C0%2C0.705524168%2C3%2C0.1%2C0.00033069%2C0.1%2C0.1%2C0.036860622%2C0.142274476%2C0.057601288%2C0.001679583%2C0.00524847%2C0.736130931%2C0.014416174%2C0.00033069%2C0%2C0%2C0.0001061%2C0.00459866%2C0.000745624%2C0%2C1%2C0.211244894%2C0%2C0.705524168%2C3%2C0%2C0.00033069%2C0%2C0%2C1%2C2%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0D%0A3%2CUGA.16.1.3_1%2CMakindye%2C0.493%2C385309%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.028570368%2C0.259280693%2C0.082540034%2C0.003821692%2C0.017477095%2C0.574689697%2C0.024289707%2C0.001400786%2C0%2C0%2C3.45E-05%2C0.006082428%2C0.001834099%2C0%2C1%2C0.236410709%2C0%2C0.740031033%2C3%2C0%2C0.001400786%2C0.1%2C0.1%2C0.028570368%2C0.259280693%2C0.082540034%2C0.003821692%2C0.017477095%2C0.574689697%2C0.024289707%2C0.001400786%2C0%2C0%2C3.45E-05%2C0.006082428%2C0.001834099%2C0%2C1%2C0.236410709%2C0%2C0.740031033%2C3%2C0%2C0.001400786%2C0%2C0%2C1%2C2%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0D%0A4%2CUGA.16.1.4_1%2CNakawa+Division%2C0.493%2C317023%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.098606697%2C0.282278627%2C0.065945258%2C0.002724668%2C0.016024352%2C0.49795729%2C0.027461236%2C0.000559863%2C0%2C0%2C0.000678506%2C0.006932757%2C0.000831023%2C0%2C1%2C0.190629622%2C0%2C0.788037462%2C3%2C0.05%2C0.000559863%2C0.1%2C0.1%2C0.098606697%2C0.282278627%2C0.065945258%2C0.002724668%2C0.016024352%2C0.49795729%2C0.027461236%2C0.000559863%2C0%2C0%2C0.000678506%2C0.006932757%2C0.000831023%2C0%2C1%2C0.190629622%2C0%2C0.788037462%2C3%2C0%2C0.000559863%2C0%2C0%2C1%2C2%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0D%0A5%2CUGA.16.1.5_1%2CRubaga+Division%2C0.493%2C383216%2C1%2C0.177278887%2C1.00E%2B10%2C7%2C0.24%2C0.08%2C0.24%2C0.08%2C0.004654201%2C0.164638786%2C0.089541423%2C0.001799026%2C0.010791608%2C0.688829209%2C0.031009589%2C0.000566465%2C0%2C0%2C2.01E-05%2C0.003735057%2C0.004392258%2C0%2C1%2C0.243857358%2C0%2C0.98927109%2C3%2C0.9%2C0.000566465%2C0.1%2C0.1%2C0.004654201%2C0.164638786%2C0.089541423%2C0.001799026%2C0.010791608%2C0.688829209%2C0.031009589%2C0.000566465%2C0%2C0%2C2.01E-05%2C0.003735057%2C0.004392258%2C0%2C1%2C0.243857358%2C0%2C0.98927109%2C3%2C0%2C0.000566465%2C0%2C0%2C1%2C2%2C0.067307692%2C0.067307692%2C0%2C0%2C0.865384615%0D%0A&isoraster=http://data.waterpathogens.org/dataset/c4a395f1-5e48-4109-b02f-951b8be89275/resource/ce447a61-b998-45df-a142-bb18273ce652/download/isoraster_kla_div.tif&popurban=http://data.waterpathogens.org/dataset/c4a395f1-5e48-4109-b02f-951b8be89275/resource/02355eae-65b5-45b8-b52b-4f6d06328351/download/popurban_kla.tif&poprural=http://data.waterpathogens.org/dataset/c4a395f1-5e48-4109-b02f-951b8be89275/resource/1f3803bb-14a1-4ede-a9a9-27c035be0700/download/poprural_kla.tif&wwtp=http://data.waterpathogens.org/dataset/c4a395f1-5e48-4109-b02f-951b8be89275/resource/5844b1dc-fd2c-4f85-9677-25f54f1fccd9/download/wwtp_input_data_kla.csv&level=3&wkt_extent=POLYGON%20%28%2829.5715%20-1.48214%2C%2035.00027%20-1.48214%2C%2035.00027%204.234466%2C%2029.5715%204.234466%2C%2029.5715%20-1.48214%29%29&wwtp_available=3'  "http://127.0.0.1:6672/scenario" -H  "accept: application/json"

```


