### 1. Load packages
library(geomorph)
library(readxl)

### 2. Set working directory
setwd("D:/Cetaceae/Landmarks_Coombs published specimens")

### 3. Import PTS landmarks 
import.pts <- function(Landmark) {
  wd <- getwd()
  ptslist <<- dir(path = wd, pattern = ".pts")
  ptsarray <<- array(dim=c(Landmark,3,length(ptslist)))
  for(i in 1:length(ptslist)) {
    ptsarray[,,i] <<- as.matrix(read.table(file=ptslist[i], skip=2, header=F, sep="", row.names=1))
  }
  dimnames(ptsarray)[[3]] <<- substr(ptslist,1,nchar(ptslist)-4)
  dimnames(ptsarray)[[1]] <<- read.table(file=ptslist[1], skip=2, header=F, sep="")[,1]
}

import.pts(Landmark=22)
array.lm <- ptsarray

### 4. GPA
ps <- gpagen(array.lm, ProcD = TRUE)

### 5. PCA
PCA <- gm.prcomp(ps$coords)
summary(PCA)

### 6. Read Excel
specimen_table <- read_excel("D:/Cetaceae/Table S1.xlsx")

### 7. Create match names
specimen_table$match_name <- paste(trimws(specimen_table$Genus), 
                                   trimws(specimen_table$Species))

### 8. Get PC scores
pc_scores <- as.data.frame(PCA$x)
pc_scores$rowname <- rownames(PCA$x)

### 9. Extract Genus Species from rownames
pc_scores$match_name <- sapply(strsplit(pc_scores$rowname, 
                                        "[ _]+"), function(x) paste(x[1], x[2]))

### 10. Merge Suborder
merged <- merge(pc_scores, 
                specimen_table[, c("match_name", "Suborder")], 
                by = "match_name", all.x = TRUE)

### 11. Manual fixes Suborder - .pts file name and excel sheet name mismatch
merged$Suborder[merged$match_name == "Patriocetid sp."] <- "Odontocete"
merged$Suborder[merged$match_name == "Waipatiid new"] <- "Odontocete"
merged$Suborder[merged$match_name == "Xenorophus new"] <- "Odontocete"

### 12. Merge Age 
merged <- merge(merged,
                specimen_table[, c("match_name", "Age")],
                by = "match_name", all.x = TRUE)

### 13. Manual fixes Age - .pts file name and excel sheet name mismatch
merged$Age[merged$match_name == "Patriocetid sp."] <- "Oligocene"
merged$Age[merged$match_name == "Waipatiid new"] <- "Oligocene"
merged$Age[merged$match_name == "Xenorophus new"] <- "Oligocene"


### 14. Flip PC1 to morphospace orientation matches Coombs et al.'s figure for comparison
merged$Comp1_flipped <- -merged$Comp1
merged$Comp2_flipped <- merged$Comp2

### 15. Colors and shapes, feel free to change if color code
age_cols <- c("Eocene"    = "#E69F00",
              "Oligocene" = "#F0E442",
              "Miocene"   = "#D4AC98",
              "Pliocene"  = "#56B4E9",
              "Extant"    = "#112F4E")

pchs <- c("Archaeocete" = 24,
          "Mysticete"   = 22,
          "Odontocete"  = 21)

### 16. Outliers and their label positions, outliners are used to compare two results 
outliers <- c("Ambulocetus natans",
              "Odobenocetops peruvianus",
              "Caperea marginata",
              "Protocetus atavus",
              "Platanista gangetica",
              "Kogia sima",
              "Xiphiacetus cristatus",
              "Miocaperea pulchra",
              "Fucaia goedertorum")

# Manual x/y nudges (in axis units) — tweak these if labels still overlap
x_nudge <- c( 0.01,   # Ambulocetus natans
              0.04,   # Odobenocetops peruvianus  
              0.02,   # Caperea marginata
              0.01,   # Protocetus atavus
              0.02,   # Platanista gangetica       
              0.02,   # Kogia sima                 
              0.01,   # Xiphiacetus cristatus
              0.02,   # Miocaperea pulchra
              0.03)   # Fucaia goedertorum

y_nudge <- c( 0.01,   # Ambulocetus natans
              0.02,   # Odobenocetops peruvianus
              0.01,   # Caperea marginata           
              0.01,   # Protocetus atavus
              0.01,   # Platanista gangetica        
              0.01,   # Kogia sima                  
              0.01,   # Xiphiacetus cristatus
              0.01,   # Miocaperea pulchra
              0.01)   # Fucaia goedertorum

### 17. Plot
png("Cetacean_Morphospace.png", width=5000, height=3500, res=300)

### Add left margin so "Odobenocetops peruvianus" label has room
par(mar = c(5, 5, 3, 2))

plot(merged$Comp1_flipped, merged$Comp2_flipped,
     bg       = age_cols[merged$Age],
     col      = "black",
     pch      = pchs[merged$Suborder],
     xlab     = "PC Axis 1 (34.7% of Total Variance)",
     ylab     = "PC Axis 2 (28.5% of Total Variance)",
     cex      = 1.5,
     cex.lab  = 1.5,
     cex.axis = 1.3,
     asp      = 1)

### Age section: 5 colored circles
### Suborder section: 3 shapes (filled black so shape is visible)
legend("bottomright",
       legend = c("Age",        # header – no symbol
                  "Eocene", "Oligocene", "Miocene", "Pliocene", "Extant",
                  "Suborder",   # header – no symbol
                  "Archaeoceti", "Mysticeti", "Odontoceti"),
       pch    = c(NA,           # Age header
                  21, 21, 21, 21, 21,
                  NA,           # Suborder header
                  24, 22, 21),
       pt.bg  = c(NA,
                  "#E69F00", "#F0E442", "#D4AC98", "#56B4E9", "#112F4E",
                  NA,
                  "black", "black", "black"),
       col    = c(NA,
                  "black", "black", "black", "black", "black",
                  NA,
                  "black", "black", "black"),
       pt.cex    = 2.2,
       cex       = 1.5,
       y.intersp = 1.1,
       bty       = "n")

#### Outlier labels with manual nudging 
outlier_rows <- merged[merged$match_name %in% outliers, ]
outlier_rows <- outlier_rows[match(outliers, outlier_rows$match_name), ]
outlier_rows <- outlier_rows[!is.na(outlier_rows$match_name), ]

text(outlier_rows$Comp1_flipped + x_nudge,
     outlier_rows$Comp2_flipped + y_nudge,
     labels = outlier_rows$match_name,
     cex    = 1.6,
     font   = 3)   # 1 = plain

dev.off()

