# lexical-analysis-obo-foundry
Supplementary data for the readability and the structural accuracy analysis of the OBO Foundry ontologies. The ontologies used in this analysis are available at [https://doi.org/10.5281/zenodo.4701571](https://doi.org/10.5281/zenodo.4701571).

# Description of the files and the folders

- The **members.txt** file contains the links to download the member ontologies.

- The **candidates.txt** file contains the links to download the candidate ontologies.

- The **get_ontologies.sh** file is a bash script used for downloading both member and candidate ontologies.

- The **metrics.jar** file is a command line tool writen in java that calculate the readability and the structural accuracy metrics for a given input.


- The **getMetricsMembers.sh** file is a bash script used for computing the readability and the structural accuracy metrics of the member ontologies.

- The **getMetricsCandidates.sh** file is a bash script used for computing the readability and the structural accuracy metrics of the candidate ontologies.

- The **members_results** folder contains the results of the metrics for the member ontologies. It contains a TSV file per ontology with the value of each metric for the corresponding ontology, in addition to the file allMetrics.tsv, which merges the metric values of all ontologies in a single file.

- The **candidates_results** folder contains the results of the metrics for the candidate ontologies. It contains a TSV file per ontology with the value of each metric for the corresponding ontology, in addition to the file allMetrics.tsv, which merges the metric values of all ontologies in a single file.

- The **detailed_files** folder contains a TSV file per ontology and metric, including details of each entity considered in the ontology for the metric.

- The **dendrograms** folder contains all the dendrograms obtained for clustering the metrics, the ontologies according to the readability, and the ontologies according to the structural accuracy metrics.

