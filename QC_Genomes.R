library(ggplot2)
library(tidyverse)
library(reshape2)
library(dplyr)
library(tibble)

checkm <- as.data.frame(read.csv(file = 'data/Checkm_qa.tsv',sep = '\t', header = TRUE))
checkm <- checkm[-nrow(checkm),]
checkm[checkm$GC < 45,]

checkm_df <- checkm %>% 
  mutate(Genome_Source = if_else(startsWith(as.character(Bin.Id), "GCF"), "Public", "UPCOAST-V"))

p <- ggplot(data= checkm_df) +
  geom_boxplot(aes(x=Genome_Source, y= X..predicted.genes, fill = Genome_Source)) +
  geom_point(aes(x=Genome_Source, y= X..predicted.genes, fill = Genome_Source), shape = 21, alpha = 0.5, size = 3 )+
  labs(title = "Predicted genes across Public and UPCOAST-V genomes",
       x = "",
       y = "Number of Predicted Genes")+
  ylim(4000, 6000)

p + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size = 12, colour = 'black'),
          axis.text.y = element_text(hjust = 0.5, size = 10, colour = 'black'), 
          axis.line = element_line(colour = "black"),
          panel.background = element_blank())

p1 <- ggplot(data =checkm_df) +
  geom_boxplot(aes(x=Genome_Source, y= GC, fill = Genome_Source)) +
  geom_point(aes(x=Genome_Source, y= GC, fill = Genome_Source), shape = 21, alpha = 0.5, size = 3 )+
  labs(title = "GC% across Public and UPCOAST-V genomes",
       x = "",
       y = "GC %")+
  ylim(43, 46)

p1 + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size = 12, colour = 'black'),
          axis.text.y = element_text(hjust = 0.5, size = 10, colour = 'black'), 
          axis.line = element_line(colour = "black"),
          panel.background = element_blank())

checkm_df

p1 <- ggplot(data =checkm_df) +
  geom_boxplot(aes(x=Genome_Source, y= Contamination, fill = Genome_Source)) +
  geom_point(aes(x=Genome_Source, y= Contamination, fill = Genome_Source), shape = 21, alpha = 0.5, size = 3 )+
  labs(title = "Contamination in Genome",
       x = "",
       y = "Contamination %")
p1 + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size = 12, colour = 'black'),
           axis.text.y = element_text(hjust = 0.5, size = 10, colour = 'black'), 
           axis.line = element_line(colour = "black"),
           panel.background = element_blank())


