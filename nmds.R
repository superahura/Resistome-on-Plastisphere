# calculate nmds
sp=read.csv(file.choose(),row.names=1)
spe=t(sp)

dist=vegdist(spe,method="bray")     #将otu换成bray矩阵
nmds=monoMDS(dist)                  #计算nmds

plot(nmds)                          #画图
nmds                                #获取stress
# read group
G=read.csv(file.choose(),row.names=1)
# plot
s=scores(nmds,choices=c(1,2))
s1=data.frame(s,G)

s1$Source<-factor(s1$Source,levels = c('Water','Plastic'))
s1$Time<-factor(s1$Time,levels = c('0 Day','2 Day','14 Day','30 Day'))

p<-ggplot(s1,aes(x=MDS1,y=MDS2,shape=Source))+
  geom_point(aes(color=Time),size=4)+
  #scale_colour_brewer(palette="Paired")+
  scale_color_manual(values = c('black','#2179B4','#35A12E','#E31C1E'))+
  scale_shape_manual(values = c(8,9))+
  #scale_x_continuous(breaks = seq(-2,2,1),limits = c(-2.2,2))+#"#919DBA","#DB8282"
  #scale_y_continuous(breaks = seq(-2,2,1),limits = c(-2.2,2))+
  stat_ellipse(type = "norm",linetype=2)+
  labs(x='NMDS1',y='NMDS2')+
  annotate("text",x= -1.2,y= -1.6,size=5,label="Stress = 0.093")+ #R =; p = ; Stress =;Anosim= 
  theme_bw(base_size = 18)+
  theme(axis.text=element_text(colour="black"),
        legend.position = 'right',
        legend.background = element_blank())
p
#6.5*5