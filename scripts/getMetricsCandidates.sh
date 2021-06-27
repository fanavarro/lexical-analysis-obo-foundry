#!/bin/bash
ontology_folder="../data/ontologies/candidate_ontologies"
output_folder="../results/candidates_results"
metrics_file="../results/candidates_results/allMetrics.tsv"

if [ ! -d "$output_folder" ] 
then
    mkdir $output_folder
fi

first_file=true
for file in $(ls $ontology_folder)
do
	echo Starting $file
	timeout 5h bash -c "java -Xmx8g -jar metrics.jar -i $ontology_folder/$file -o $output_folder/$file.tsv -t 1 -l INFO -v &> $output_folder/$file.log"
	if $first_file ; then
		cat $output_folder/$file.tsv > $metrics_file
		first_file=false
	else
		cat $output_folder/$file.tsv | tail -n +2 >> $metrics_file
	fi
	echo $file analysis finished
done
