#library(devtools)
#install_github("neobernad/evaluomeR", ref="feature_request")
#install_github("neobernad/evaluomeR")

library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstudioapi)
library(evaluomeR)
library(tools)

printRidgelinePlot <- function(data, metric) {
  ggplot(na.omit(data[[metric]]), aes(x = .data[[metric]], y = 1, fill = as.character(cluster))) +
    geom_density_ridges(jittered_points = TRUE, alpha = 0.4) +
    theme_ridges() + 
    theme(legend.position = "none")
}

printDensityPlotWithBars <- function(data, metric) {
  aux = na.omit(data[[metric]])
  aux$cluster = as.character(aux$cluster)
  ggplot(aux, aes(x = .data[[metric]], y = 1, fill = cluster,  point_color = cluster)) +
    geom_density_ridges(jittered_points = TRUE,
                         stat = "density_ridges",
                         alpha = 0.4, 
                         point_shape = "|", 
                         scale = .95, 
                         rel_min_height = .01, 
                         point_size = 5,
                         size = 0.2,
                        panel_scaling=F,
                         position = position_points_jitter(height = 0)) +
    theme_ridges() +
    ylab('Density')
}

printDensityPlotWithPoints <- function(data, metric) {
  aux = na.omit(data[[metric]])
  aux$cluster = as.character(aux$cluster)
  ggplot(aux, aes(x = .data[[metric]], y = 1, fill = cluster, point_color = cluster)) +
    geom_density_ridges(jittered_points = TRUE, size = 0.2, alpha = 0.4, stat = "density_ridges", panel_scaling=F) +
    theme_ridges() +
    ylab('Density')
}

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

all_metrics = c("Names per class", 
                "Synonyms per class", 
                "Descriptions per class", 
                "Systematic naming",
                "Lexically suggest logically define")

all = filter(all, Metric %in% all_metrics)
all = spread(all, Metric, Value)
all$File = tools::file_path_sans_ext(all$File)
all = rename(all, File = 'Ontology')
all = select(all, -Member)

# Evaluome params
k.range = c(2,11)
bs = 20
seed = 100

# Get statistics on optimal k value
stabilityData <- stabilityRange(data=all, k.range=k.range, 
                                bs=bs, getImages = FALSE, seed=seed)
qualityData <- qualityRange(data=all, k.range=k.range,
                            getImages = FALSE, seed=seed)

kOptTable <- getOptimalKValue(stabilityData, qualityData)
View(kOptTable)

# Get optimal clusters
x = annotateClustersByMetric(all, k.range=k.range, bs=bs, seed=seed)
printDensityPlotWithPoints(x, 'Names per class')
nrow(x[['Names per class']] %>% filter(cluster==1))
nrow(x[['Names per class']] %>% filter(cluster==2))

printDensityPlotWithPoints(x, 'Synonyms per class')
nrow(x[['Synonyms per class']] %>% filter(cluster==1))
nrow(x[['Synonyms per class']] %>% filter(cluster==2))

printDensityPlotWithPoints(x, 'Descriptions per class')
nrow(x[['Descriptions per class']] %>% filter(cluster==1))
nrow(x[['Descriptions per class']] %>% filter(cluster==2))

printDensityPlotWithPoints(x, 'Systematic naming')
nrow(x[['Systematic naming']] %>% filter(cluster==1))
nrow(x[['Systematic naming']] %>% filter(cluster==2))

printDensityPlotWithPoints(x, 'Lexically suggest logically define')
nrow(x[['Lexically suggest logically define']] %>% filter(cluster==1))
nrow(x[['Lexically suggest logically define']] %>% filter(cluster==2))

# Get metric range for each cluster
x = getMetricRangeByCluster(all, k.range=k.range, bs=bs, seed=seed)
View(x)





ggplot(na.omit(x[['Synonyms per class']]), aes(x = `Synonyms per class`, y = 1, fill = as.character(cluster))) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.25,
                      position = position_points_jitter(height = 0)) +
  theme_ridges() + 
  theme(legend.position = "none")

ggplot(na.omit(x[['Synonyms per class']]), aes(`Synonyms per class`, fill = cluster, colour = cluster)) +
  geom_density(alpha = 0.1) 


points <- tibble(x = na.omit(x[['Synonyms per class']]$`Synonyms per class`), y = 0)
ggplot(na.omit(x[['Synonyms per class']]), aes(`Synonyms per class`, y=cluster, fill = as.character(cluster), colour = as.character(cluster))) +
  geom_density(alpha = 0.1) +
  geom_jitter()
