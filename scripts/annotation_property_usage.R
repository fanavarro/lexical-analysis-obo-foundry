library(dplyr)
library(tidyr)
library(rstudioapi)
library(ggplot2)
library(egg)
library(stringr)

# Get data paths
rootPath = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
usagePath = file.path(rootPath, 'results', 'annotation_property_usage.tsv')

# Read source files and compose the dataset to be analysed
usage = read.csv2(usagePath, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F, check.names = FALSE) %>% select(-ontology)

# Use prefix instead complete IRI
usage$`annotation property` = str_replace(usage$`annotation property`, 'http://purl.obolibrary.org/obo/IAO_', 'IAO:')
usage$`annotation property` = str_replace(usage$`annotation property`, 'http://www.w3.org/2004/02/skos/core#', 'SKOS:')
usage$`annotation property` = str_replace(usage$`annotation property`, 'http://www.w3.org/2000/01/rdf-schema#', 'RDFS:')
usage$`annotation property` = str_replace(usage$`annotation property`, 'http://purl.org/dc/elements/1.1/', 'DC:')
usage$`annotation property` = str_replace(usage$`annotation property`, 'http://purl.obolibrary.org/obo/OBI_', 'OBI:')
usage$`annotation property` = str_replace(usage$`annotation property`, 'http://www.geneontology.org/formats/oboInOwl#', 'oboInOwl:')


# Plots
names_data = filter(usage, `content type` == 'Name') %>% select(`annotation property`, `annotations in classes`)
names_plot = ggplot(names_data, aes(x=reorder(`annotation property`, `annotations in classes`), y=`annotations in classes`) ) +
  geom_segment( aes(x=reorder(`annotation property`, `annotations in classes`) ,xend=`annotation property`, y=0, yend=`annotations in classes`), color="#74c69d") +
  geom_point(size=3, color="#69b3a2") +
  geom_text(x=names_data$`annotation property`, y=names_data$`annotations in classes`, label = names_data$`annotations in classes`, vjust=-1, size=3.2, colour="#495057") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,6000000), labels = scales::comma_format(big.mark = "")) +
  xlab("Annotation property for names") +
  ylab("Number of annotations\nin classes") +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +
  labs(title='A')

descriptions_data = filter(usage, `content type` == 'Description') %>% select(`annotation property`, `annotations in classes`)
descriptions_plot = ggplot(descriptions_data, aes(x=reorder(`annotation property`, `annotations in classes`), y=`annotations in classes`) ) +
  geom_segment( aes(x=reorder(`annotation property`, `annotations in classes`) ,xend=`annotation property`, y=0, yend=`annotations in classes`), color="#74c69d") +
  geom_point(size=3, color="#69b3a2") +
  geom_text(x=descriptions_data$`annotation property`, y=descriptions_data$`annotations in classes`, label = descriptions_data$`annotations in classes`, vjust=-1, size=3.2, colour="#495057") +
  theme_minimal() +
  scale_y_continuous(limits=c(0,1300000), labels = scales::comma_format(big.mark = "")) +
  xlab("Annotation property for descriptions") +
  ylab("Number of annotations\nin classes") +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +
  labs(title='B')

synonyms_data = filter(usage, `content type` == 'Synonym') %>% select(`annotation property`, `annotations in classes`)
synonyms_plot = ggplot(synonyms_data, aes(x=reorder(`annotation property`, `annotations in classes`), y=`annotations in classes`) ) +
  geom_segment( aes(x=reorder(`annotation property`, `annotations in classes`) ,xend=`annotation property`, y=0, yend=`annotations in classes`), color="#74c69d") +
  geom_point(size=3, color="#69b3a2") +
  geom_text(x=synonyms_data$`annotation property`, y=synonyms_data$`annotations in classes`, label = synonyms_data$`annotations in classes`, vjust=-1, size=3.2, colour="#495057") +
  theme_minimal() +
  xlab("Annotation property for synonyms") +
  ylab("Number of annotations\nin classes") +
  ylim(0,2200000) +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +
  labs(title='C')


ggarrange(names_plot, descriptions_plot, synonyms_plot)
