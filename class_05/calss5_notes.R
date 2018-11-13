#2A
Table1 <- read.table("bimm143_05_rstats/weight_chart.txt",header = T,sep = "\t")

# a <- plot(x = Table1$Age,y = Table1$Weight,type = "o",pch=15,cex=1.5)
plot(Table1,type = "o",pch=15,cex=1.5)

plot(1:10,pch=1:5,cex=1:2,col=c("red","blue"))

boxplot(cbind(rnorm(1000,0),rnorm(1000,4)))

#2B
Table2_feature <- read.table("bimm143_05_rstats/feature_counts.txt",sep="\t",header = T)
#View(Table2_feature)

par(mar=c(5,5,5,0),las=1,col="red") #if can't find help in bar, go par
barplot(Table2_feature$Count,names.arg = Table2_feature$Feature,horiz = T,xlim=c(0,80000),las=1,
        ylab = "y_lab",xlab = "x_lab",col="blue")
lines(seq(1,80000,1000))

#2C
hist(cbind(rnorm(10000,0),rnorm(10000,4)),breaks = 50)

# par(mfrow=c(3,3)) #subplot
plot(1:5)

#3A
tabele3_MF <- read.table("bimm143_05_rstats/male_female_counts.txt",header = T,sep = "\t")
par(las=2)
barplot(tabele3_MF$Count,names.arg = tabele3_MF$Sample,ylab = "aaa",col=c("blue2","red2"))

#col = rainbow(nrow(tabele3_MF))

#3B
table4_UD <- read.table("bimm143_05_rstats/up_down_expression.txt",header = T,sep = "\t")
table(table4_UD$State)
palette(c("green","blue","purple")) #zi zhi tiao se pan
plot(table4_UD$Condition1,table4_UD$Condition2,col=table4_UD$State)

#3C plot based on data density
table5_Exp <- read.table("bimm143_05_rstats/expression_methylation.txt",header = T,sep = "\t")
nrow(table5_Exp)

#temp_col <- densCols(table5_Exp$gene.meth,table5_Exp$expression)
#plot(table5_Exp$gene.meth,table5_Exp$expression,col = temp_col)

idx <- table5_Exp$expression>0 #only get genes with expression >0
new_col <- colorRampPalette(c("blue2","green2","red2","yellow"))
temp_col_2 <-  densCols(table5_Exp$gene.meth[idx],table5_Exp$expression[idx],colramp=new_col)
plot(table5_Exp$gene.meth[idx],table5_Exp$expression[idx],col = temp_col_2,pch=20)







