# Hw6 - Yurong Wang

library(bio3d) #read package

read_pdb <- function(x){
  # The fuction plots how  B-factor looks like at different residue position
  # Take one string input of a PDB id 
  # after taking, it will plot the figure of how the Residue temperature factors (B-factor) changes with 
  # residue numbers. Annotations on secondary structure are marked at margin. 
  
  rawPDB <- read.pdb(x) #read a single PDB file
  rawPDB.chainA <- trim.pdb(rawPDB,chain="A",elety="CA") #create a new PDB object when only A chain's C-alpha atom are in
  rawPDB.b <- rawPDB.chainA$atom$b #assign: Residue temperature factors (b)
  
  plotb3(rawPDB.b,sse=rawPDB.chainA,typ="l",ylab = "Bfactor") #sse: annotation of secondary structure 
      # at marginal area. 
      # y-axis: Residue temperature factors (B-factor) assigned priviously (s.b) 
}


read_pdb("4AKE") #example of function in use
