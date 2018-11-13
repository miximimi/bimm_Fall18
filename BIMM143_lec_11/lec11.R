# what oercent of database is protein 
# pdbstat <- read.csv("Data Export Summary.csv",row.names = 1)
# 
# percentage <- round(csv_protein$Total*100 / nstr,2)
# sumTotal <- sum(pdbstat$Total)
# nstats <- pdbstat
# nstats$Percent <- percentage
# kable(nstats)
# 
# protein <- sum(pdbstat$Proteins)
# poportionP <- round(protein/sumTotal *100,2)

################# vusualize pdbm
library("bio3d")
pdb <- read.pdb("1hsg")
pdb

attributes(pdb) # summary of the thing


# Q6. How many amino acid residues are there in this pdb object and what are the two nonprotein
# residues?
dim(pdb$atom)
pdb$atom



head(pdb$atom)
# Print a subset of $atom data for the first two atoms
pdb$atom[1:2, c("eleno", "elety", "x","y","z")]

ligand.pdb <- atom.select(pdb,"ligand",valur=T)
write.pdb(ligand.pdb,)




atom.select(pdb, "protein") 
atom.select(pdb, "ligand") 


#automatic downloading multiple pdb files: alignment
ids <- c("1TND_B","1AGR_A","1TAG_A","1GG2_A","1KJY_A","4G5Q_A")
files <- get.pdb(ids, split = TRUE)

# Extract and align the chains we are interested in
pdbs <- pdbaln(files, fit = TRUE)
# Print to screen a summary of the 'pdbs' object
pdbs

# find invariant core: fit at the core where it is nor moving
core <- core.find(pdbs)
# superimpose all structures to core
pdbs$xyz = pdbfit(pdbs, core)
# Perform PCA
pc.xray <- pca(pdbs)


plot(pc.xray) #pc1 vs. pc2



#use this data + MVD = makes protein to move ???????????????
# ????????? see water in MVD? 
# why do we install muscle? 
# Goal of this class: make protien moce






library(bio3d.view)




