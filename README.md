# lexical-analysis-obo-foundry
Supplementary data for the readability and the structural accuracy analysis of the OBO Foundry ontologies. The ontologies used in this analysis are available at [https://doi.org/10.5281/zenodo.4701571](https://doi.org/10.5281/zenodo.4701571).

# Description of the files and the folders

- The **data** folder initially contains the several files with links to download both the member and the candidate OBO Foundry ontologies.

- The **scripts** folder contains the following scripts:
    - **get_ontologies.sh**: Bash script to download the latest version of the OBO Foundry ontologies into the *data/ontologies* folder.
    - **metrics.jar**: Command line based java application to obtain the readability and the structural accuracy metrics for a set of ontologies.
    - **getMetricsMembers.sh**: Bash script to obtain the metrics for the OBO Foundry member ontologies. This script read the folder */data/ontologies/member_ontologies* and put the results in */results/members_results*.
    - **getMetricsMembers.sh**: Bash script to obtain the metrics for the OBO Foundry candidate ontologies. This script read the folder */data/ontologies/candidate_ontologies* and put the results in */results/candidates_results*.
    - **data_analysis.R**: R script intended to be open in an interactive rstudio session. It contains the R commands used to perform the statistical and clusering analysis, including commented lines that help to follow the script.

- The **results** folder contains subfolders with the results provided by each step, namely:
    - **candidates_results**: Folder including TSV files for each candidate ontology, including its metric values. A file called *allMetrics.tsv* is also included, in which the information about all the candidate ontologies is merged.
    - **members_results**: Folder including TSV files for each member ontology, including its metric values. A file called *allMetrics.tsv* is also included, in which the information about all the member ontologies is merged.
    - **detailed_files**: Folder including detailed files for each (ontology, metric) pair, including extra information at entity level.
    - **dendrograms**: Folder including the dendrograms obtained by the clustering analysis. In the case of the ontology clustering according the readability or the structural accuracy metrics, extra figures evaluating each cluster are provided.

# How to reproduce the results

## Requirements
- UNIX based system (tested on Ubuntu 14.04.6 LTS)
- Git
- Java (tested on openjdk 11.0.11)
- R (tested on version 3.6.3)
- RStudio (tested on version 1.1.453)
- R libraries:
    - ggplot2 (tested on version 3.3.4)
    - tools (tested on version 3.6.3)
    - ggpubr (tested on version 0.4.0)
    - stringr (tested on version 1.4.0)
    - dplyr (tested on version 1.0.7)
    - tidyr (tested on version 1.0.2)
    - pvclust (tested on version 2.2-0)
    - caret (tested on version 6.0-88)
    - philentropy (tested on version 0.5.0)
    - parallel (tested on version 3.6.3)
    - doParallel (tested on version 1.0.15)
    - outforest (tested on version 0.1.1)
    - corrplot (tested on version 0.84)
    - RColorBrewer (tested on version 1.1-2)
    - rstudioapi (tested on version 0.11)


## Get the repository
The first step to reproduce our results is to download this repository into your computer with the command:
`git clone https://github.com/fanavarro/lexical-analysis-obo-foundry.git`

Once the repo is downloaded, enter into the folder:

`cd lexical-analysis-obo-foundry`

## Get the ontologies
The first step to reproduce our results is to download the ontologies that we analysed. This could be done in two ways. On the one hand, we provide a bash script that downloads the latest version of the ontologies from the OBO Foundry. On the other hand, we provide a Zenodo persistent link that contains the concrete versions used in our analysis.

### Download the ontologies with the provided script
We provide a bash script that we used for downloading the ontologies. Note that this script will download the latest version of the ontologies so changes could have been done on them. If you want to download the ontologies used in the article, please go to [Download the ontologies from Zenodo](#download-the-ontologies-from-zenodo). This script reads the download links for each ontology from the files [candidates_download_links.txt](https://github.com/fanavarro/lexical-analysis-obo-foundry/blob/main/data/ontologies/candidates_download_links.txt) and [members_download_links.txt](https://github.com/fanavarro/lexical-analysis-obo-foundry/blob/main/data/ontologies/members_download_links.txt). The downloaded ontologies are saved by default in the folders *lexical-analysis-obo-foundry/data/ontologies/candidate_ontologies* and *lexical-analysis-obo-foundry/data/ontologies/member_ontologies*.

To run the script:

```
# Enter in scripts folder
cd scripts

# Execute the script
bash get_ontologies.sh
```

Please, check that the ontologies are in the folders *lexical-analysis-obo-foundry/data/ontologies/candidate_ontologies* and *lexical-analysis-obo-foundry/data/ontologies/member_ontologies* after running the script in order to verify that everything worked well.


### Download the ontologies from Zenodo
In order to assess reproducibility, we uploaded the concrete versions of the ontologies we used for the study in the Zenodo platform at [https://doi.org/10.5281/zenodo.4701571](https://doi.org/10.5281/zenodo.4701571). Download the provided zip files for the candidate ontologies ([candidate_ontologies.zip](https://zenodo.org/record/4701572/files/candidate_ontologies.zip?download=1)) as well as the member ontologies ([member_ontologies.zip](https://zenodo.org/record/4701572/files/member_ontologies.zip?download=1)) and extract them in the folder *lexical-analysis-obo-foundry/data/ontologies/* so that you have the following folders:

- *lexical-analysis-obo-foundry/data/ontologies/candidate_ontologies*
- *lexical-analysis-obo-foundry/data/ontologies/member_ontologies*


## Calculate the metrics
The next step is to calculate the metrics for the ontologies previously downloaded. To achieve this, we provide the java application *metrics.jar*, located at the [scripts folder](./scripts). This application has the following parameters:

- -i,--input arg      The input. It can be a single ontology provided in OWL format, or a folder containing a set of ontologies in OWL format.
- -o,--output arg     The output. It will be a TSV file including the metrics for the ontology in the input.
- -v,--detailed-files If present, generate a report for each metric-ontology pair, consisting in detailed information at ontology class level.

Furthermore, we provide two bash scripts that encapsulates the use of *metrics.jar* to calculate the metrics for candidate and member ontologies:

- [getMetricsCandidates.sh](./scripts/getMetricsCandidates.sh): Calculate the metrics for the candidate ontologies, which are taken from the folder *lexical-analysis-obo-foundry/data/ontologies/candidate_ontologies*. The results are saved into the folder *lexical-analysis-obo-foundry/results/candidates_results*. This will create one TSV file per ontology including all the metrics for that ontology, together with a log file. Furthermore, a file called *allMetrics.tsv* will be also created, including all the metrics for all the ontologies. Moreover, detailed_files will be created into the detailed_files folder.

- [getMetricsMembers.sh](./scripts/getMetricsMembers.sh): Calculate the metrics for the member ontologies, which are taken from the folder *lexical-analysis-obo-foundry/data/ontologies/member_ontologies*. The results are saved into the folder *lexical-analysis-obo-foundry/results/members_results*. This will create one TSV file per ontology including all the metrics for that ontology, together with a log file. Furthermore, a file called *allMetrics.tsv* will be also created, including all the metrics for all the ontologies. Moreover, detailed_files will be created into the detailed_files folder.

Run the following commands to execute these scripts:
```
# Go to the scripts folder (assuming that you are on the root folder of this repository)
cd scripts

# Get metrics for the member ontologies
bash getMetricsMembers.sh

# Get metrics for the candidate ontologies
bash getMetricsCandidates.sh

# Move detailed files to the results folder
mv detailed_files ../results/
```

Once the above commands are executed, your results folder will contain a three folders:

- *candidates_results*: logs and TSV files with the metrics obtained for each candidate ontology. The file *allMetrics.tsv* contains the information about all the candidate ontologies and their metrics.
- *members_results*: logs and TSV files with the metrics obtained for each member ontology. The file *allMetrics.tsv* contains the information about all the member ontologies and their metrics.
- *detailed_files*: Set of TSV files including a more detailed information about each ontology-metric pair.

## Perform analysis
The statistical analysis was performed by using the rstudio software. In order to reproduce it, open rstudio and load the file [data_analysis.R](./scripts/data_analysis.R). Then, you can execute it line by line in an interactive way. Furthermore, this file includes comments to clarify each section.
