library(stringr)
library(rstudioapi)

### FUNCTIONS ###

### MAIN SCRIPT ###
# Get data paths
rootPath = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
candidateResultsPath = file.path(rootPath, 'results', 'candidates_results', 'allMetrics.tsv')
memberResultsPath = file.path(rootPath, 'results', 'members_results', 'allMetrics.tsv')
detailedFilesPath = file.path(rootPath, 'results', 'detailed_files')

# Read source files and compose the dataset to be analysed
candidates = read.csv2(candidateResultsPath, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F)
members = read.csv2(memberResultsPath, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F)
candidates$Member = F
members$Member = T
all = rbind(members, candidates)
all$Value = as.numeric(all$Value)
all$File = as.character(all$File)

### CORRELATION ANALYSIS ###

metricsToShow = c("Names per class", 
                  "Synonyms per class", 
                  "Descriptions per class", 
                  "Systematic naming",
                  "Lexically suggest logically define")



# Test whether the number of transitive sub-classes of an LR classis correlated 
# with the systematic naming metric value

cor.test(x=exampleSystematicNaming$transitiveChildren, y=exampleSystematicNaming$Metric.Value, method = "spearman", use="complete.obs")
column_ontology = c()
column_number_of_lr_classes = c()
column_rho = c()
column_p_value = c()

systematic_naming_total = NULL # This stores all LR classes from obo foundry

systematic_naming_file_list = list.files(path = detailedFilesPath, pattern = '.*_Systematic_naming.tsv')
for (systematic_naming_name in systematic_naming_file_list){
  ontology = str_split(systematic_naming_name, '\\.')[[1]][1]
  systematic_naming_file_path = file.path(detailedFilesPath, systematic_naming_name)
  systematic_naming_data = read.csv2(systematic_naming_file_path, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F)
  if (nrow(na.omit(systematic_naming_data)) > 2) {
    # lm model
    systematic_naming_data$Metric.Value = as.numeric(systematic_naming_data$Metric.Value)
    systematic_naming_data$transitiveChildren = systematic_naming_data$Positive.Cases + systematic_naming_data$Negative.Cases
    cat (paste(systematic_naming_name,  '\n'))
    model = lm(systematic_naming_data$Metric.Value ~ systematic_naming_data$transitiveChildren)
    plot(systematic_naming_data$transitiveChildren, systematic_naming_data$Metric.Value, ylab = "Systematic Naming value", xlab = "Transitive subclasses", main = ontology)
    abline(model)
    
    # Spearman correlation
    cor_test = cor.test(x=systematic_naming_data$transitiveChildren, y=systematic_naming_data$Metric.Value, method = "spearman", use="complete.obs")
    column_ontology = c(column_ontology, ontology)
    column_number_of_lr_classes = c(column_number_of_lr_classes, length(systematic_naming_data$Class))
    column_rho = c(column_rho, cor_test$estimate[["rho"]])
    column_p_value = c(column_p_value, cor_test$p.value)
    
    # Create systematic naming total table
    if (is.null(systematic_naming_total)){
      systematic_naming_total = systematic_naming_data
    } else {
      systematic_naming_total = rbind(systematic_naming_total, systematic_naming_data)
    }
  }
}
correlation_data = data.frame(column_ontology, column_number_of_lr_classes, column_rho, column_p_value)
colnames(correlation_data) = c('Ontology', 'LR classes', 'Spearman correlation', 'p-value')
sum(correlation_data$`LR classes`) == nrow(systematic_naming_total)

# For each ontology, the correlation between the number of transitive subclasses
# of an LR class and its systematic naming value
View(correlation_data)

# Correlation by using all LR classes in the repository
model = lm(systematic_naming_total$Metric.Value ~ systematic_naming_total$transitiveChildren)
plot(systematic_naming_total$transitiveChildren, systematic_naming_total$Metric.Value, ylab = "Systematic Naming value", xlab = "Transitive subclasses")
abline(model)

cor.test(x=systematic_naming_total$transitiveChildren, y=systematic_naming_total$Metric.Value, method = "spearman", use="complete.obs")
