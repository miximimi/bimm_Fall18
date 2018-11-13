
###############################
# #lecture note
# #lecture_4: most basic
# ?exp
# ?plot.default
# help(topic = "stats-package") #use "TAB" for fun
# help(seq)
# 
# x <- 1:10 #short cut: alt- for <- and alt+ for ≠
# x = 1:10; x = seq(1,10); x <- c(1:10)
# plot(x,sin(x),type="o", col="blue",lwd=3)
# round(sqrt(x),3)
# x[4]; x[c(4,7)]
# date()
# z=c(T,F,T,F,T)
# x[z]
# 
#     #esc will stop the "+" for not able to close )
# rm(list=ls()) #clean up enviornment
# 
# # to create name and assign each a number: 
# 
# x <- 1:3; names(x) <- c("a","b","c")
# x[c("b","a")]
# x <- c(a=1,b=2,c=3); x["a"]
# which.max(x)
# sort(x)
# 
#     # matrix: data.frame, eg.: gene1, gene2, expression1, exp2 ... 
# 
# dat <- data.frame(id=letters[1:10],x=1:10,y=11:20); View(dat)
# dat$id; dat$y[3]  
# 
# # Read CSV into R
# # MyData <- read.csv(file="TheDataIWantToReadIn.csv", header=TRUE, sep=",")
# dep <- read.csv2("http://bio3d.uib.no/data/pdb_deposition2.csv")
# 
# # head();tail();View();dim();nrow();ncol();rownames();colnames();str();
# #use comman+enter to send fomr script to console
# #shortcut: control+1 control+2 to moce cursor
# #cbind(matrix1,matrix2) # or rbind #combine by column row
# fac <- factor(c("a","b"),ordered=T,levels=c("a","b")); levels(fac) <- c("aa","bb") #make fac leveled factor, and assign name of factor
# summary(fac)

#  x=1:3;y=c(T,T,F)
#  df_namebbbb <- data.frame(x,y)
#  str(df_namebbbb) # to review srtucture
#  df_namebbbb[2:3,"y"] #retrieve from dataframe: only columns are variable nae
#  subset(df_namebbbb,subset=y)
# order(subset(df_namebbbb,subset=y)) #outcome: index

# my_list=list(df_namebbbb,x,y,fac) #include everything inside
# names(my_list) <- c("vec", "mat","df","aa")
# #or, my_list <- list(vec = df_namebbbb,mat=x,df=y)
# # data.frams: diff class of columns
boxplot(rnorm(1000,0)) #1000 points, mean=0
# #Table1 <- read.table("bimm143_05_rstats/weight_chart.txt",header = T,sep = "\t")
# par($cex) #check the current value

# 
# cbind(rnorm(10,0),rnorm(10,4))
# rbind(rnorm(10,0),rnorm(10,4))
# 
# par(mfrow=c(3,3)) #subplot
# plot(1:5)
######################################

# lec6: function practice

# rescale <- function(x,na.rm = T,plot=T){
#   #x shall be a vector, return the portion x is larger than min
#   #rng <- range(x,na.rm = T) #added na.rm for remove NA. if na.rm=F(default), non-resistant to NA
#   rng <- range(x,na.rm = na.rm)
#   
#   answer <- (x - rng[1])/(rng[2]-rng[1])
#   
#   if(plot){ #user defined: whether to plot
#   plot(answer,type = "b")
#   }
# }
#   rescale(c(1,2,1,5,5,3),F,T)















