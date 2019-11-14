library(vegan)
library(labdsv)
library(mgcv)
library(nlme)
library(MASS)
spe.pc=read.csv(file.choose(),row.names=1)
gene.pc=read.csv(file.choose(),row.names=1)
group=read.csv(file.choose(),row.names=1)

#spe=otu file
#gene=ARGs file
#group FILE
spe=t(spe.pc)
gene=t(gene.pc)

spe.dis <- dsvdis(spe,'bray/curtis')
spe.pc <- pco(spe.dis)
#k=n-1(n=sample numbers)
gene.dis <- dsvdis(gene,'bray/curtis')
gene.pc <- pco(gene.dis)

spe.2.pc <- scores(spe.pc, choices = 1:2)
gene.2.pc <- scores(gene.pc, choices = 1:2)
pro <- protest(spe.2.pc,gene.2.pc,permutation=9999,scores = "sites")

point.color <- as.integer(group$group) + 1
#group= CSV file name ??treatment=?ļ???????????
plot(pro,type = "text")
#text??Ӧ??????λ??
pro
summary(pro)
plot(pro,xlab="PC1", ylab="PC2",     
     ar.col="red", len="0.02", pch = 21, bg=point.color,cex=2,
     to.target = TRUE,display = c("target", "rotated"),main="")


points(pro,display = "target",pch = 24,bg=point.color,cex=1.5)
legend("topright",c("SG_Fert","WQ_Fert"),bty="n",
       pch = 22,pt.bg=c("red","green3"),
       cex=1.1)
legend("bottomright",c("16S","ARGs"), pch = c(24,21),
       bty="n",cex=1.1)
pro
summary(pro)
residuals(pro)
?protest
plot(spe.pc)

mantel(spe.dis,gene.dis,method="spear",permutation=9999)



