#1、首先得有两个矩阵，一个是物种矩阵，另一个是影响物种组成的环境因子矩阵，两个矩阵有相同的行名称(如果有的话)及行数量，且物种矩阵每一行的和不能为0，暂且分别命名为otu和env。
#2、计算env矩阵的相关系数，并以图形的方式展示出来。
library("corrplot","ggplot2")
# mt函数 计算mantel test各相关系数与p等
##3个OTUs类型
otu1<-read.csv(file.choose(),row.names = 1,header=TRUE,check.names = 0)
otu2<-read.csv(file.choose(),row.names = 1,header=TRUE,check.names = 0)
otu3<-read.csv(file.choose(),row.names = 1,header=TRUE,check.names = 0)
##环境因子
env<-read.csv(file.choose(),row.names = 1,header=TRUE,check.names = 0)
# mt函数
mt<-function(otu,env){
  library(vegan)
  library(dplyr)
  vars <- colnames(env)
  models<-list()
  for (i in seq_along(vars)){
    otu_bray<-vegdist(otu,method = "bray")
    env_dis<-vegdist(env[vars[i]],method = "euclidean")
    model <- mantel(otu_bray,env_dis, permutations=9999)
    name <- vars[i]
    statistic <- model$statistic
    signif <- model$signif
    models[[i]] <- data.frame(name = name, statistic = statistic, signif = signif, row.names = NULL)
  }
  models %>%  bind_rows()
}
#求mantel test r, p
mantest1<-mt(otu1,env)
mantest2<-mt(otu2,env)
mantest3<-mt(otu3,env)
#计算环境因子相关矩阵
env<-read.table("clipboard",header=TRUE,sep='\t',check.names = 0)#读取excel里的剪切数据
env_cor<-cor(env)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(env_cor, method="square", col=col(200),  
         type="upper", order="hclust", number.cex = 0.7,
         #addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         # p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

#构建坐标

gpointx<-c(1)#, 2, 3) #视个数而定，两个亦可
gpointy<-c(3)#, 2.5, 2) #同上，n OTUs坐标
glabel<-c('16S OTUs')#,'α diversity','β diversity')
#构建 line 连接
n<-ncol(env)
df_line <- data.frame(
  glabel = rep(glabel, each = n), # 分组名称，每个重复n次
  gpointx = rep(gpointx, each = n), # 组X坐标，每个重复n次
  gpointy = rep(gpointy, each = n), # 组Y坐标，每个重复n次
  x = rep(0:(n - 1) - 0.1,1), # 变量连接点X坐标
  y = rep(n:0.1, 1) # 变量连接点Y坐标
)
#整合line坐标和mantel 的r与p
df_mantel <-rbind(mantest1)#, mantest2, mantest3)
df_segment<-cbind(df_line,df_mantel)
head(df_segment)#查看整合的数据框
#将p值划分不同范围进行线条映射，不完全按数值进行美学映射
df_segment <- df_segment %>% 
  mutate(
    lcol = ifelse(signif <= 0.001, '#4F9744', lcol), 
    # p值小于0.001时，颜色为绿色，下面依次类推
    lcol = ifelse(signif > 0.001 & signif <= 0.01, '#645D8A', lcol),
    lcol = ifelse(signif > 0.01 & signif <= 0.05, '#ABAD40', lcol),
    lcol = ifelse(signif > 0.05, '#CECDCA', lcol),
    lwd = ifelse(statistic >= 0.3,6, lwd),
    # statistic >= 0.3 时，线性宽度为6，下面依次类推
    lwd = ifelse(statistic >= 0.15 & statistic < 0.3, 3, lwd),
    lwd = ifelse(statistic < 0.15, 1, lwd)
  )
#进行画线
segments(df_segment$gpointx, df_segment$gpointy, 
         df_segment$x, df_segment$y, #起点x,y与终点x,y
         lty = 'solid', lwd = df_segment$lwd, 
         col = df_segment$lcol, xpd = TRUE)#线型，线宽，颜色。图例绘在制图区外,必须设置参数xpd=TRUE
points(gpointx, gpointy, pch = 24, col = '#3C56A5', bg = '#3C56A5', cex = 2, xpd = TRUE)
points(x, y, pch=21,col="#4B4B4A",bg="#CECDCA",cex=1,xpd=TRUE)

#legend 

