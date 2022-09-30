#library(devtools)
#install_github("fanavarro/evaluomeR")
#install_github("neobernad/evaluomeR", ref="feature_request")
#install_github("neobernad/evaluomeR")

library(ggridges)
library(ggplot2)
library(egg)
library(dplyr)
library(tidyr)
library(rstudioapi)
library(evaluomeR)
library(tools)
library(factoextra)
library(corrplot)
library(RColorBrewer)

getStabilityInterpretation <- function(x) {
  if (x < 0.60){
    return("Unstable")
  }
  if (x <= 0.75){
    return("Doubtful")
  }
  if (x <= 0.85){
    return("Stable")
  }
  if (x <= 1){
    return("Highly stable")
  }
}

getQualityInterpretation <- function(x) {
  if (x < 0.25){
    return("No structure")
  }
  if (x <= 0.50){
    return("Weak structure")
  }
  if (x <= 0.70){
    return("Reasonable structure")
  }
  if (x <= 1){
    return("Strong structure")
  }
}

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
all = all %>% dplyr::rename(Ontology = File)
all = select(all, -Member)

# Evaluome params
k.range = c(2,11)
bs = 20
seed = 100

# Get statistics on optimal k value
stability_data <- stabilityRange(data=all, k.range=k.range, 
                                bs=bs, getImages = FALSE, seed=seed)
quality_data <- qualityRange(data=all, k.range=k.range,
                            getImages = FALSE, seed=seed)

kOptTable <- getOptimalKValue(stability_data, quality_data)
View(kOptTable)

# Get optimal clusters
x = annotateOptimalClustersByMetric(all, k.range=k.range, bs=bs, seed=seed)
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
x = getMetricOptimalRangeByCluster(all, k.range=k.range, bs=bs, seed=seed)
View(x)


# Get cluster info for k = [2, 5]
k.range = c(2,5)
x = annotateClustersByMetric(all, k.range=k.range, bs=bs, seed=seed)
stability_data = as.data.frame(assay(x[['stability_data']]))
quality_data = x[['quality_data']]
metric_ranges = getMetricRangeByCluster(all, k.range=k.range, bs=bs, seed=seed)
for (metric_name in all_metrics){
  metric_plots = list()
  for (k in k.range[1]:k.range[2]){
    stability_row_name = paste('Mean_stability_k_', k, sep='')
    stability_k = as.numeric(stability_data %>% filter(Metric == metric_name) %>% pull(!!sym(stability_row_name)))
    quality_col_name = paste('k_', k, sep = '')
    quality_k = as.data.frame(assay(quality_data[[quality_col_name]])) %>% filter(Metric == metric_name) %>% pull(Avg_Silhouette_Width) %>% as.numeric()
    annotationText = paste('Stability = ', format(round(stability_k, 3), nsmall = 3), '-', getStabilityInterpretation(stability_k), '\nQuality = ', format(round(quality_k, 3), nsmall = 3), '-', getQualityInterpretation(quality_k))
    aux = na.omit(x[[metric_name]][[as.character(k)]])
    aux$cluster = as.character(aux$cluster)
    # points to draw vertical lines
    cutpoints = c(metric_ranges[[as.character(k)]] %>% filter(metric==metric_name) %>% pull(min_value), metric_ranges[[as.character(k)]] %>% filter(metric==metric_name) %>% pull(max_value))
    #cat(paste('metric =', metric,'k =',k,'cutpoints =', cutpoints, "\n"))
    plot = ggplot(aux, aes(x = .data[[metric_name]], y = 1, fill = cluster, point_color = cluster)) +
      geom_density_ridges(jittered_points = TRUE, size = 0.2, alpha = 0.4, stat = "density_ridges", panel_scaling=F) +
      theme_ridges() + 
      annotate(geom = 'text', label = annotationText, x = -Inf, y = Inf, hjust = 0, vjust = 1) +
      ylab('Density') +
      geom_vline(xintercept = cutpoints, linetype="dotted") +
      scale_x_continuous(labels=cutpoints, breaks=cutpoints, guide = guide_axis(check.overlap = T, angle = 90))
    metric_plots[[as.character(k)]] = plot
  }
  ggarrange(plots = metric_plots)
}


# Get range information
x = getMetricRangeByCluster(all, k.range=k.range, bs=bs, seed=seed)
View(x[['3']])

library("gridExtra")
pdf("/home/fabad/imprimir/k2.pdf")
grid.table(x[['2']], rows = NULL)
dev.off()

pdf("/home/fabad/imprimir/k3.pdf")
grid.table(x[['3']], rows = NULL)
dev.off()

pdf("/home/fabad/imprimir/k4.pdf")
grid.table(x[['4']], rows = NULL)
dev.off()

pdf("/home/fabad/imprimir/k5.pdf")
grid.table(x[['5']], rows = NULL)
dev.off()


# Cluster ontologies by using lsld and systematic naming as features
x = all %>% select(Ontology, `Lexically suggest logically define`, `Systematic naming`) %>% na.omit()
rownames(x) = x$Ontology
x$Ontology = NULL
set.seed(seed)
k2 = kmeans(x, 2)
set.seed(seed)
k3 = kmeans(x, 3)
set.seed(seed)
k4 = kmeans(x, 4)
set.seed(seed)
k5 = kmeans(x, 5)

fviz_cluster(k2, data = x, stand=FALSE)
fviz_cluster(k3, data = x, stand=FALSE, repel = T)
fviz_cluster(k4, data = x, stand=FALSE)
fviz_cluster(k5, data = x, stand=FALSE)

# Optimal number of clusters according to the silhouette
fviz_nbclust(x, kmeans, method = "silhouette")

# Stability
a = clusterbootWrapper(data=x[c("Lexically suggest logically define", "Systematic naming")], B=bs,
                       bootmethod="boot",
                       cbi="kmeans",
                       krange=2, seed=seed)
a$bootmean
mean(a$bootmean)



# Metric correlation
cor.test(x=all$`Systematic naming`, y=all$`Lexically suggest logically define`, method = "spearman", use="complete.obs")

corrplot(cor(x), is.cor=T, tl.col='black', type="full", order="hclust", hclust.method="ward.D",
         col=brewer.pal(n=8, name="RdYlGn"))


