library(ggplot2)
library(tools)
library(stringr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(rstudioapi)
library(gridExtra)
library(ggpubr)

### FUNCTIONS ###

# Receives the original data in long format and a list of metric names, and plots
# the metric distribution by using density plots.
plot_metric_distribution <- function(all, metric_names){
  par_opt = par(mfrow=c(3,2))
  data = spread(all, Metric, Value)
  for (metric_name in metric_names){
    metric_values = data %>% pull(metric_name) %>% na.exclude()
    shapiro_p_value = format(shapiro.test(metric_values)$p.value, digits=4)
    plot(density(metric_values), main = paste(metric_name, 'distribution'))
    mtext(paste("Shapiro test p-value", shapiro_p_value), side=3, cex=0.7)
  }
  par(par_opt)
}

# Receives the original data in long format and a list of metric names, and plots
# the metric distribution by using violin plots.
plot_metric_violins <- function(all, metric_names){
  data = spread(all, Metric, Value)
  plots = list()
  for (metric_name in metric_names){
    x = filter(all, Metric == metric_name)
    metric_values = x$Value %>% na.exclude()
    shapiro_p_value = format(shapiro.test(metric_values)$p.value, digits=4)
    ymax=max(metric_values) + (0.1 * max(metric_values))
    plot = ggplot(data = x, aes(x=Metric, y=Value)) + geom_violin() + geom_boxplot(width=0.2) + ylim(0, ymax) + labs(x = "") + annotate(geom = 'text', label = paste("Shapiro test p-value =", shapiro_p_value), size=3, x = -Inf, y = Inf, hjust = 0, vjust = 1)
    plots[[metric_name]] = plot
  }
  return(do.call("grid.arrange", c(plots, ncol = 3)))
}

# Receives the original data in long format and compares candidate and member ontologies
# from OBO Foundry according to each metric. Wilcoxon test is also used to show the significance.
compareCandidatesAndMembers <- function(all, metricsToCompare){
  x = spread(all, Metric, Value)
  pvalues = c()
  metrics = c()
  memberMean = c()
  candidateMean = c()
  for (metric in metricsToCompare) {
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

### DESCRIPTIVE ANALYSIS ###
metricsToShow = c("Names per class", 
                  "Synonyms per class", 
                  "Descriptions per class", 
                  "Systematic naming",
                  "Lexically suggest logically define")

# Distribution of each metric
plot_metric_distribution(all, metricsToShow)

# Summary of the metrics
summary(filter(all, Metric %in% metricsToShow) %>% spread(Metric, Value))
# Standard deviations
apply((filter(all, Metric %in% metricsToShow) %>% spread(Metric, Value) %>% select(-File, -Member)), 2, sd, na.rm=TRUE)
# Complete data
View(filter(all, Metric %in% metricsToShow) %>% spread(Metric, Value))

# Violin plots
plot_metric_violins(all, metricsToShow)

# Boxplots
x = filter(all, Metric %in% metricsToShow)
x$Metric = factor(x$Metric, levels=metricsToShow)
ggplot(data = x, aes(x=Metric, y=Value)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1))

# Comparison between canididate and member ontologies
compareCandidatesAndMembers(all, metricsToShow)

# Comparison between names per class and descriptions per class
metricsToShow = c("Names per class",
                  "Descriptions per class")
comparisons = list(c("Names per class", "Descriptions per class"))
x = filter(all, Metric %in% metricsToShow)
x$Metric = factor(x$Metric, levels=metricsToShow)
ggplot(data = x, aes(x=Metric, y=Value)) + geom_boxplot() + stat_compare_means(comparisons = comparisons)
