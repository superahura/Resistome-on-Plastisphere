library(vegan)
library(ggplot2)
species<-read.csv(file.choose(), row.names=1)
#DCA Axis Lengths DCA1 > 4 CCA,3-4 CCA or RDA, <3 RDA
#
species.dca<-decorana(species)
summary(species.dca)

#write.csv(smry$sites[1,2],file='C:/Users/yk/Documents/WeChat Files/superahura/Files/rda.csv')

#DCA RDA, CCA,RDA
env<-read.csv(file.choose(),row.names=1)
species.rda <-rda(species,env)

plot(species.rda,display=c("si","bp"),choices=c(1,2))
ef=envfit(species.rda,env,permu=999)
ef

#
new<-species.rda$CCA
new

grp<-read.csv(file.choose(),header = T, row.names = 1)

samples<-data.frame(sample=row.names(new$u),RDA1=new$u[,1],RDA2=new$u[,2])
samples0<-data.frame(samples,grp)
head(samples0)

species<-data.frame(spece=row.names(new$v),RDA1=new$v[,1],RDA2=new$v[,2])
head(species)
envi<-data.frame(en=row.names(new$biplot),RDA1=new$biplot[,1],RDA2=new$biplot[,2])
envi

line_x = c(0,envi[1,2],0,envi[2,2],0,envi[3,2],0,envi[4,2],0,envi[5,2])
line_x
line_y = c(0,envi[1,3],0,envi[2,3],0,envi[3,3],0,envi[4,3],0,envi[5,3])
line_y
line_g = c("MC","MC","TOC","TOC","TC","TC",'IC','IC','TN','TN')
line_g
line_data = data.frame(x=line_x,y=line_y,group=line_g)
line_data


p<-ggplot(data=samples0,aes(RDA1,RDA2)) + 
  geom_point(aes(fill=group),shape= 21,size=4) +
  #geom_point(data=species,aes(shape=spece),size=2) +
  geom_text(data=envi,aes(label=en),color="blue") +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0)+
  geom_line(data=line_data,aes(x=x,y=y,group=group),color="black") +
  scale_fill_brewer(palette = 'Paired')+
  theme_bw(base_size = 18) + theme(panel.grid=element_blank())
p



