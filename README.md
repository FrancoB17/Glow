# GloWPa Model

This version of the GloWPa model is prepared by Nynke on 23/4/2020. It is provided to Lisanne Nauta and Gifty Abiwu on that day.

This version has not been thoroughly checked.

## Installation

Clone the git repository in your current folder and checkout mapping-tool branch:

`git clone https://git.wur.nl/glowpa/glowpa-model.git`  
`git checkout mapping-tool`

The mapping tool branch comes with the following submodules:
* pathogenflows R package (https://git.wur.nl/glowpa/pathogenflows.git forked from https://github.com/mverbyla/pathogenflows)

To install the submodules type the following command:

`git submodule update`

The git repository of the submodule is now cloned in the submodule directory. Now open the GloWPa.Rproj file R studio.

Install the folowing R packages from the R console:

`devtools::install_local('./libs/pathogenflows')`

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
    11. Push changes to your origin: `git push origin master`

    2.b  If you want to make changes on your own:
    1. Commit code.
    2. Push code.
3. Go to GloWPa git 
4. Update submodule: `git submodule update`
5. Now the submodule reference code has been updated and needs to be committed to the GloWPa repository: `git commit -a -m "Updated submodule"`

