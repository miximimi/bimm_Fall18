# lac6 hands-on session

#A
rescale <- function(x,na.rm = T,plot=T){
  #x shall be a vector, return the portion x is larger than min
  #rng <- range(x,na.rm = T) #added na.rm for remove NA. if na.rm=F(default), non-resistant to NA
  rng <- range(x,na.rm = na.rm)
  
  answer <- (x - rng[1])/(rng[2]-rng[1])
  
  if(plot){ #user defined: whether to plot
    plot(answer,type = "b")
  }
}
rescale(c(1,2,1,5,5,3),F,T)

#B
install.packages("bio3d")
library(bio3d)

# Can you improve this analysis code?
library(bio3d)
s1 <- read.pdb("4AKE") # kinase with drug
s2 <- read.pdb("1AKE") # kinase no drug
s3 <- read.pdb("1E4Y") # kinase with drug
s1.chainA <- trim.pdb(s1, chain="A", elety="CA")
s2.chainA <- trim.pdb(s2, chain="A", elety="CA")
s3.chainA <- trim.pdb(s1, chain="A", elety="CA")
s1.b <- s1.chainA$atom$b
s2.b <- s2.chainA$atom$b
s3.b <- s3.chainA$atom$b
plotb3(s1.b, sse=s1.chainA, typ="l", ylab="Bfactor")
plotb3(s1.b, typ="l", ylab="Bfactor")
plotb3(s2.b, sse=s2.chainA, typ="l", ylab="Bfactor")
plotb3(s3.b, sse=s3.chainA, typ="l", ylab="Bfactor")

# points(s2.b,typ)

## plot altogether
plotb3(s1.b,sse=s1.chainA,typ="l",ylab="Bfactor")
points(s2.b,type = "l", col = "blue",lwd=3)
points(s3.b,type = "l", col = "yellow",lwd=3)

read_pdb <- function(string){
  s <- read.pdb(string)
  s.chainA <- trim.pdb(s,chain="A",elety="CA") #what is difference between $ and . ? what is the class of s
  s.b <- s.chainA$atom$b
  plotb3(s.b,sse=s.chainA,typ="l",ylab = "Bfactor")
  # if(length(string)>1){
  # points(s.b,type = "l",col = "blue",lwd=3)
  # }
}

# x <- "4AKE"
# x <- "1AKE"
# x <- "1E4Y"


read_pdb(x)













