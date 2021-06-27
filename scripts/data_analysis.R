library(ggplot2)
library(tools)
library(ggpubr)
library(stringr)
library(dplyr)
library(tidyr)
library(pvclust)
library(caret)
library (philentropy)
library(parallel)
library(doParallel)
library(outForest)
library(corrplot)
library(RColorBrewer)
library(rstudioapi)

### FUNCTIONS ###

# Creates a figure from 'obj' in bmp and eps format with the size specified
# by 'w' (width) and 'h' (height). The figure is saved in the specified path.
exportImage <- function(path, w, h, obj){
  setEPS()
  bmp(paste(path, ".bmp", sep = ""), width = w, height = h)
  plot(obj)
  dev.off()
  
  setEPS()
  postscript(paste(path, ".eps", sep = ""), width = w, height = h)
  plot(obj)
  dev.off()
}

# Creates a figure from the dendrogram in 'obj' in bmp and eps format with the size specified
# by 'w' (width) and 'h' (height). The dendrogram is saved in the specified path.
# Clusters supported by a significance higher than 0.95 are marked.
exportDendrogram <- function(path, w, h, obj){
  setEPS()
  bmp(paste(path, ".bmp", sep = ""), width = w, height = h)
  plot(obj)
  pvrect(obj, alpha = 0.95)
  dev.off()
  
  setEPS()
  postscript(paste(path, ".eps", sep = ""), width = w, height = h)
  plot(obj)
  pvrect(obj, alpha = 0.95)
  dev.off()
}


# Calculates the cosine distance
cosine_wrap <- function(x) {
  res = philentropy::distance(as.data.frame(as.matrix(t(x))), method = "cosine", use.row.names = TRUE)
  res = 1 - res
  res = as.dist(res)
  attr(res, "method") <- "cosine"
  return(res)
}
attr(cosine_wrap, 'name') <- 'cosine'


# Preprocess the data. This function receives the data from the study in long format
# together with the metrics to take into account.
# 1. Remove near zero variables.
# 2. Scale the data.
# 3. Remove outliers
# 4. Remove near zero variables.
# 5. Return the original data together with the processed data.
preprocessData <- function(longData, metricsToUse){
  wideData = spread(filter(longData, Metric %in% metricsToUse), Metric, Value)
  row.names(wideData) = tools::file_path_sans_ext(wideData$File)
  x = select(wideData, -File, -Member)
  x = na.omit(x)
  print(paste("Removing near zero var columns:", colnames(x)[nearZeroVar(x)]))
  x = select(x, -nearZeroVar(x))
  originalData = data.frame(x)
  x = scale(x)
  x = as.data.frame(as.matrix(x))
  # Outliers removal
  col_names = colnames(x)
  colnames(x) = gsub(" ", "_", colnames(x))
  outliers = outForest(x, replace = "NA", seed = 12345)
  print(paste("Removing outliers:\n", toString(outliers(outliers))))
  #outliers(outliers)
  x = Data(outliers)
  colnames(x) = col_names
  x = drop_na(x)
  print(paste("Removing near zero var columns after removing outliers:", colnames(x)[nearZeroVar(x)]))
  x = select(x, -nearZeroVar(x))
  return (list("originalData" = originalData, "preprocessedData" = x))
}


# This function receives the original data in long format, a list of metrics to take into account,
# a list of distances measures, a list of clustering methods, a base folder to put the generated figures,
# and a boolean 'clusterMetrics' indicating that the metrics should be clustered instead of the ontologies.
# 1. Preprocess the data.
# 2. Generate a clustering for each (distanceMethod, clusteringMethod).
# 3. Save the generated dendrogram in 'baseFolder' with a name indicating the used method.
# 4. If clusterMetrics is false, evaluate the clustering by generating boxplots comparing the formed groups
#    according to each evaluated metric.
# 5. Save the evaluation plot in the 'baseFolder'.
generateAndEvaluateClusters <- function(longData, metricsToUse, distancesToEval, clustMethodsToEval, baseFolder, clusterMetrics=F){
  nboot=1000
  parallel=TRUE
  seed = 563
  if (!dir.exists(file.path(baseFolder))){
    dir.create(file.path(baseFolder))
  }
  
  preprocess_info = preprocessData(longData, metricsToUse)
  x = preprocess_info$preprocessedData
  originalData = preprocess_info$originalData
  
  if(clusterMetrics){
    x = as.data.frame(as.matrix(t(x)))
  }
  
  for (distMethod in distancesToEval){
    if (is.function(distMethod)){
      distName = attr(distMethod, 'name')
    } else {
      distName = distMethod
    }
    
    for (hclustMethod in clustMethodsToEval){
      info = paste(distName,hclustMethod, sep = "-")
      clustering = pvclust(as.data.frame(as.matrix(t(x))), method.dist=distMethod, method.hclust=hclustMethod, nboot=nboot, parallel=parallel, iseed = seed)
      
      #dendrogramPlot = paste(getwd(), "/", baseFolder, "/", info, sep = "")
      #evalPlot = paste(getwd(), "/", baseFolder, "/", info, '-eval', sep = "")
      dendrogramPlot = paste(baseFolder, "/", info, sep = "")
      evalPlot = paste(baseFolder, "/", info, '-eval', sep = "")
      exportDendrogram(dendrogramPlot, w=1400, h=600, clustering)
      if(!clusterMetrics){
        aux = pvpick(clustering, alpha=0.95)
        groups = rep(seq_along(aux$clusters), times = sapply(aux$clusters, length))
        names(groups) = unlist(aux$clusters)
        if (length(groups) != 0){
          compareClusters(groups, originalData, evalPlot)  
        }
      }
    }
  }
}

# Receives the original data in long format and compares candidate and member ontologies
# from OBO Foundry according to each metric. Wilcoxon test is also used to show the significance.
compareCandidatesAndMembers <- function(all){
  x = spread(all, Metric, Value)
  pvalues = c()
  metrics = c()
  memberMean = c()
  candidateMean = c()
  for (metric in unique(all$Metric)) {
    metrics = c(metrics, metric)
    
    x_member = x %>% select(File, Member, matches(metric)) %>% filter(Member==T) 
    x_member = x_member[[metric]]
    memberMean = c(memberMean, mean(x_member, na.rm=T))
    
    x_candidate = x %>% select(File, Member, matches(metric)) %>% filter(Member==F)
    x_candidate = x_candidate[[metric]]
    candidateMean = c(candidateMean, mean(x_candidate, na.rm=T))
    
    p = wilcox.test(x_member, x_candidate)$p.value
    pvalues = c(pvalues, p)
  }
  comparison = as.data.frame(cbind(metrics, memberMean, candidateMean, pvalues))
  comparison = data.frame(metrics=metrics, memberMean=memberMean, candidateMean=candidateMean, pvalues=pvalues)
  colnames(comparison) = c('Metric', 'Mean of members', 'Mean of candidates', 'p-value')
  
  return (comparison)
}

# Receives the groups formed by a clustering algorithm, the dataframe with the original data
# and compare the groups according to the variables. Then, a figure including boxplots depicting
# the comparisons between each group for each metric is generated in 'plotFileOutDir'.
compareClusters <- function (groups, df, plotFileOutDir) {
  all_metrics = c("Names per class", 
                  "Synonyms per class", 
                  "Descriptions per class", 
                  "Names per object property", 
                  "Synonyms per object property", 
                  "Descriptions per object property", 
                  "Names per data property", 
                  "Synonyms per data property", 
                  "Descriptions per data property", 
                  "Names per annotation property", 
                  "Synonyms per annotation property", 
                  "Descriptions per annotation property",
                  "Systematic naming",
                  "Lexically suggest logically define")
  x = data.frame(df)
  x$cluster = NA
  for (row_name in rownames(x)){
    if(row_name %in% names(groups)){
      x[row_name, "cluster"] = groups[[row_name]]
    }
  }
  x = na.omit(x)
  # Get the comparison table per cluster and metric.
  col_names = str_trim(gsub("\\.", " ", colnames(x)), side = "both")
  colnames(x) = col_names
  summary = x %>% group_by(cluster)  %>% summarise_all(mean) %>% select(any_of(all_metrics))
  
  # Draw figures comparing clusters
  metrics = colnames(x)[colnames(x) %in% metricsToShow]
  x = gather(x, key = 'Metric', value = 'Value',  metrics)
  x$cluster = as.factor(x$cluster)
  
  comparisons = list()
  for (i in 1:(max(groups)-1)){
    for (j in (i+1):max(groups)){
      comparisons[[length(comparisons) + 1]] = c(as.character(i), as.character(j))
    }
  }
  setEPS()
  figure = ggplot(data = x, aes(x=cluster, y=Value)) + facet_wrap(~x$Metric) + geom_boxplot() + stat_compare_means(comparisons = comparisons)
  exportImage(plotFileOutDir, w=800, h=1200, figure)
  return (t(summary))
}


### MAIN SCRIPT ###
# Get data paths
rootPath = dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
candidateResultsPath = file.path(rootPath, 'results', 'candidates_results', 'allMetrics.tsv')
memberResultsPath = file.path(rootPath, 'results', 'members_results', 'allMetrics.tsv')

# Read source files and compose the dataset to be analysed
candidates = read.csv2(candidateResultsPath, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F)
members = read.csv2(memberResultsPath, header = T, sep = "\t", na.strings = "NaN", stringsAsFactors = F)
candidates$Member = F
members$Member = T
all = rbind(members, candidates)
all$Value = as.numeric(all$Value)
all$File = as.character(all$File)

### DESCRIPTIVE ANALYSIS ###
# Comparison between member and candidate ontologies according to the considered metrics (readability and structural accuracy)
metricsToShow = c("Names per class", 
                  "Synonyms per class", 
                  "Descriptions per class", 
                  "Names per object property", 
                  "Synonyms per object property", 
                  "Descriptions per object property", 
                  "Names per data property", 
                  "Synonyms per data property", 
                  "Descriptions per data property", 
                  "Names per annotation property", 
                  "Synonyms per annotation property", 
                  "Descriptions per annotation property",
                  "Systematic naming",
                  "Lexically suggest logically define")
View(compareCandidatesAndMembers(filter(all, Metric %in% metricsToShow)))

# Summary of the readability metrics regarding classes
summary(spread(all, Metric, Value) %>% select(File, Member, contains('per class')))
# Complete data of the readability metrics regarding classes
View(spread(all, Metric, Value) %>% select(File, Member, contains('per class')))

# Summary of the readability metrics regarding object properties
summary(spread(all, Metric, Value) %>% select(File, Member, contains('per object property')))
# Complete data of the readability metrics regarding object properties
View(spread(all, Metric, Value) %>% select(File, Member, contains('per object property')))

# Summary of the readability metrics regarding data properties
summary(spread(all, Metric, Value) %>% select(File, Member, contains('per data property')))
# Complete data of the readability metrics regarding data properties
View(spread(all, Metric, Value) %>% select(File, Member, contains('per data property')))

# Summary of the readability metrics regarding annotation properties
summary(spread(all, Metric, Value) %>% select(File, Member, contains('per annotation property')))
# Complete data of the readability metrics regarding annotation properties
View(spread(all, Metric, Value) %>% select(File, Member, contains('per annotation property')))

# Summary of the systematic naming metric
summary(spread(all, Metric, Value) %>% select(File, Member, contains('Systematic naming')))
# Complete data of the systematic naming metric
View(spread(all, Metric, Value) %>% select(File, Member, contains('Systematic naming')))

# Summary of the LSLD metric
summary(spread(all, Metric, Value) %>% select(File, Member, contains('Lexically suggest logically define')))
# Complete data of the LSLD metric
View(spread(all, Metric, Value) %>% select(File, Member, contains('Lexically suggest logically define')))


# Comparison between ontology entities according to the number of names they have
metricsToShow = c("Names per class", 
                  "Names per object property", 
                  "Names per data property", 
                  "Names per annotation property")
comparisons = list(c("Names per class", "Names per object property"),
                   c("Names per class", "Names per data property"),
                   c("Names per class", "Names per annotation property"),
                   c("Names per object property", "Names per data property"),
                   c("Names per object property", "Names per annotation property"),
                   c("Names per data property", "Names per annotation property"))
x = filter(all, Metric %in% metricsToShow)
x$Metric = factor(x$Metric, levels=metricsToShow)
ggplot(data = x, aes(x=Metric, y=Value)) + geom_boxplot() + stat_compare_means(comparisons = comparisons) + theme(axis.text.x = element_text(angle = 45, hjust=1))

# Comparison between ontology entities according to the number of synonyms they have
metricsToShow = c("Synonyms per class",
                  "Synonyms per object property",
                  "Synonyms per data property",
                  "Synonyms per annotation property")
comparisons = list(c("Synonyms per class", "Synonyms per object property"),
                   c("Synonyms per class", "Synonyms per data property"),
                   c("Synonyms per class", "Synonyms per annotation property"),
                   c("Synonyms per object property", "Synonyms per data property"),
                   c("Synonyms per object property", "Synonyms per annotation property"),
                   c("Synonyms per data property", "Synonyms per annotation property"))
x = filter(all, Metric %in% metricsToShow)
x$Metric = factor(x$Metric, levels=metricsToShow)
ggplot(data = x, aes(x=Metric, y=Value)) + geom_boxplot() + stat_compare_means(comparisons = comparisons) + theme(axis.text.x = element_text(angle = 45, hjust=1))

# Comparison between ontology entities according to the number of descriptions they have
metricsToShow = c("Descriptions per class",
                  "Descriptions per object property",
                  "Descriptions per data property",
                  "Descriptions per annotation property")
comparisons = list(c("Descriptions per class", "Descriptions per object property"),
                   c("Descriptions per class", "Descriptions per data property"),
                   c("Descriptions per class", "Descriptions per annotation property"),
                   c("Descriptions per object property", "Descriptions per data property"),
                   c("Descriptions per object property", "Descriptions per annotation property"),
                   c("Descriptions per data property", "Descriptions per annotation property"))
x = filter(all, Metric %in% metricsToShow)
x$Metric = factor(x$Metric, levels=metricsToShow)
ggplot(data = x, aes(x=Metric, y=Value)) + geom_boxplot() + stat_compare_means(comparisons = comparisons) + theme(axis.text.x = element_text(angle = 45, hjust=1))

# Comparison between the structural accuracy metrics (systematic naming and lexically suggest, logically define)
metricsToShow = c("Systematic naming",
                  "Lexically suggest logically define")
x = filter(all, Metric %in% metricsToShow)
x$Metric = factor(x$Metric, levels=metricsToShow)
ggplot(data = x, aes(x=Metric, y=Value)) + geom_boxplot() 



### CLUSTERING ANALYSIS ###

# Ontology clustering according to the readability metrics
metricsToShow = c("Names per class", 
                  "Synonyms per class", 
                  "Descriptions per class", 
                  "Names per object property", 
                  "Synonyms per object property", 
                  "Descriptions per object property", 
                  "Names per annotation property", 
                  "Synonyms per annotation property", 
                  "Descriptions per annotation property")
distances = c("euclidean", "correlation", "maximum", "manhattan", "canberra", "minkowski", cosine_wrap)
clustMethods = c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid")
base_dir = file.path(rootPath, 'results', 'dendrograms', 'readability_clustering')
generateAndEvaluateClusters(all, metricsToShow, distances, clustMethods, base_dir)  
# See generated figures at results/dendrograms/readability_clustering


# Ontology clustering according to the structural accuracy metrics
metricsToShow = c("Systematic naming", 
                  "Lexically suggest logically define")
distances = c("euclidean", "correlation", "maximum", "manhattan", "canberra", "minkowski", cosine_wrap)
clustMethods = c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid")
base_dir = file.path(rootPath, 'results', 'dendrograms', 'structural_accuracy_clustering')
generateAndEvaluateClusters(all, metricsToShow, distances, clustMethods, base_dir)  
# See generated figures at results/dendrograms/structural_accuracy_clustering





# Metric clustering
metricsToShow = c("Names per class", 
                  "Synonyms per class", 
                  "Descriptions per class", 
                  "Names per object property", 
                  "Synonyms per object property", 
                  "Descriptions per object property", 
                  "Names per annotation property", 
                  "Synonyms per annotation property", 
                  "Descriptions per annotation property",
                  "Systematic naming",
                  "Lexically suggest logically define")

distances = c("euclidean", "correlation", "maximum", "manhattan", "canberra", "minkowski", cosine_wrap)
clustMethods = c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid")
base_dir = file.path(rootPath, 'results', 'dendrograms', 'metrics_clustering')
generateAndEvaluateClusters(all, metricsToShow, distances, clustMethods, base_dir, clusterMetrics = T)  
# See generated figures at results/dendrograms/metrics_clustering

# Metrics correlation plot
prep_data_info = preprocessData(all, metricsToShow)
x=prep_data_info$preprocessedData
x = as.data.frame(as.matrix(t(x)))

# Addrect used for marking four clusters.
corrplot(cor(t(x)), is.cor=T, tl.col='black', type="full", order="hclust", hclust.method="ward.D",
         col=brewer.pal(n=8, name="RdYlGn"), addrect=4)





