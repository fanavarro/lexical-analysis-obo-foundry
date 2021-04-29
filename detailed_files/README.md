# Description of the files
This folder contains a TSV file per ontology/metric pair with detailed information. The name of these files follows the pattern "{ontology_acronym}.owl_{metric_name}.tsv". The structure of the files depend on the nature of the metric.

## Structural accuracy metrics
The structural accuracy metrics comprises the *systematic naming* and the *lexically suggest, logically define* metrics. The files concerning these metrics have the following columns:

* **Metric**: Metric name.
* **Class**: The LR class that is measured.
* **Class depth**: The depth in which the LR class is located in the ontology.
* **LR**: The label of the LR class, which is a lexical regularity.
* **Positive Cases**: Number of positive cases. If metric is "Lexically suggest logically define", the value of this column is the number of classes exhibiting *LR* in their labels that are semantically related to *Class*. If metric is "Systematic naming", positive cases are the number of subclasses of *Class* that exhibit *LR*.
* **Positive cases average depth**: The average depth in which the positive cases are located in the ontology.
* **Positive cases average distance to LR class**: The average hierarchical distance from positive cases to *Class*.
* **Negative Cases**: Number of negative cases. If metric is "Lexically suggest logically define", the value of this column is the number of classes exhibiting *LR* in their labels that are not semantically related to *Class*. If metric is "Systematic naming", positive cases are the number of subclasses of *Class* that do not exhibit *LR*.
* **Negative cases average depth**: The average depth in which the negative cases are located in the ontology.
* **Negative cases average distance to LR class**: The average hierarchical distance from negative cases to *Class*.
* **Metric Value**: The individual metric value for the LR class according the formula $\frac{Positive Cases}{Positive Cases + NegativeCases}$.


## Readability metrics
The readability metrics are (names|synonyms|descriptions) per (class|object property|data property|annotation property). The detailed files related with these metrics are structured as follows:

* **Metric**: Metric name.
* **Entity**: The individual entity that is measured by the metric. Depending on the metric, this column will be named "Class", "Object property", "Data Property", or "Annotation Property".
* **Metric Value**: The individual metric value for the entity.
* **Hierarchy**: The SNOMED CT hierarchy in which the entity is located. This value is only present if the entity is a class.
