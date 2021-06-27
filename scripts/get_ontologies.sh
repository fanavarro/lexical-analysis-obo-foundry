#!/bin/bash

download_ontologies() {
	links_file=$1
	download_folder=$2
	if [ ! -d "$download_folder" ] 
	then
		mkdir $download_folder
	fi


	for ontology_url in $(cat $links_file)
	do
		echo $ontology_url
		file=`basename $ontology_url`
		wget --output-document=$file $ontology_url
		mv $file $download_folder
	done
}

member_folder="../data/ontologies/member_ontologies"
member_links="../data/ontologies/members_download_links.txt"

not_member_folder="../data/ontologies/candidate_ontologies"
not_member_links="../data/ontologies/candidates_download_links.txt"

download_ontologies $member_links $member_folder
download_ontologies $not_member_links $not_member_folder

