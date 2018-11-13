#### class 13

#read a pdb, and separate the useful protein and ligand for future

library('bio3d')
file.name <- get.pdb('1hsg')
hiv <- read.pdb(file.name)
hiv
prot <- trim.pdb(hiv, "protein")
lig <- trim.pdb(hiv, "ligand")

write.pdb(prot, file="1hsg_protein.pdb")
write.pdb(lig, file="1hsg_ligand.pdb")

## then use autodocktools to prepare .pdbqt file with charges
## in autodocktools, note the grid box's number that covers the suspected ligand binding dite

### dock with Vina  with command: 
# ~/Downloads/autodock_vina_1_1_2_mac/bin/vina --config config.txt --log log.txt

# read docking results: file all.pdbqt and log.txt

res <- read.pdb("all.pdbqt", multi=TRUE) # many pdb files inside
write.pdb(res, "results.pdb") # create normal pdb file called results.pdb

# check pdb: 14 total models

# res <- read.pdb("all.pdbqt", multi=TRUE)
ori <- read.pdb("1hsg_ligand_charge.pdbqt")
rmsd(ori, res) #the less, the better meet between perdiction and actual


#######
# library(bio3d)
pdb <- read.pdb("1HEL")
modes <- nma(pdb) # model NMA
plot(modes, sse=pdb) # the peak = high flexibility!!! NB

mktrj(modes, mode=7, file="nma_7.pdb")# generate pdb file nma










