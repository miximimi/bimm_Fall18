#### class 13

#read a pdb, and separate the useful protein and ligand

library('bio3d')
file.name <- get.pdb('1hsg')
hiv <- read.pdb(file.name)
hiv
prot <- trim.pdb(hiv, "protein")
lig <- trim.pdb(hiv, "ligand")

write.pdb(prot, file="1hsg_protein.pdb")
write.pdb(lig, file="1hsg_ligand.pdb")
