library("ggplot2")
library("reshape2")
x1<-read.csv(file.choose(),head =1)
head(x1)
x2<-melt(x1,id.vars = "sample",variable.name = "Phylum")
x2$sample<-factor(x2$sample,levels = c('W0','W2','P2','W14','P14','W30','P30'))
head(x2)
p<-ggplot(x2,aes(x=sample,y=value,fill=Phylum))+
  geom_bar(position='fill',stat = "identity")+
  labs(y="Relative abundance")+
  scale_y_continuous(breaks = seq(0,1,0.25),
                     expand = c(0,0),
                     labels = scales::percent)+
  scale_fill_brewer(palette = "Paired")+
  theme_bw(base_size = 18)+
  theme(axis.text = element_text(colour = "black"),
        axis.title.x = element_blank(),
        legend.position = 'right',
        legend.background = element_blank())
p
#7.5*5