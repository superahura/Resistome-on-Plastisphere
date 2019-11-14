library(ggplot2)
library(vegan)
library(labdsv)
library(mgcv)
library(nlme)
library(MASS)
library(RColorBrewer)
otu<-read.csv(file.choose(),row.names=1)
gene<-read.csv(file.choose(),row.names=1)
group<-read.csv(file.choose(),row.names=1)

otu.dis<-dsvdis(t(otu),'bray/curtis')
gene.dis<-dsvdis(t(gene),'bray/curtis')

pro<-protest(pco(otu.dis),pco(gene.dis),permutation=9999)
eigen <- sqrt(pro$svd$d)#
percent_var <- signif(eigen/sum(eigen), 4)*100

otu_pro <- data.frame(pro$X)
gene_pro <- data.frame(pro$Yrot)
otu_pro$sampleid <- rownames(otu_pro)
otu_pro$type <- "OTU"
gene_pro$sampleid <- rownames(gene_pro)
gene_pro$type <- "ARGs"

colnames(gene_pro) <- colnames(otu_pro)

pval <- signif(pro$signif, 1)

p_data <- rbind(otu_pro, gene_pro)
head(p_data)
p_data<-data.frame(p_data, group)
p_pro<- ggplot(p_data)+
  geom_point(aes(x = Dim1, y = Dim2,
                 color = Source,shape=type,size=factor(Time))) +
  labs(x = paste0("PC 1 (",percent_var[1],"%)"),
      y = paste0("PC 2 (",percent_var[2],"%)"))+
  scale_color_manual(values = c("#B2DF8A","#FB9A99")) +
  theme_bw(base_size = 18) +
  geom_line(aes(x= Dim1, y=Dim2, group=sampleid), 
            col = "#4D4D4D", alpha = 0.8) +
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(color = 'black'))
p_pro
 
mantel(otu.dis,gene.dis,method="spear",permutation=9999)


