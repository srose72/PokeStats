raw <- read.csv("pokedex.csv")

colNames <- c("DexNum", "Name", "Type1", "Type2", "TotalStat",
              "HP", "Attack", "Defense", "SpAtk", "SpDef",
              "Speed", "Generation", "Legendary", "LevelingRate",
              "FemaleRatio", "CatchRate", "Smogon", "Height", "Weight",
              "EggCycles")

pokedex <- raw

names(pokedex) <- colNames

pokedex <- pokedex[, !(names(pokedex) %in% c("X", "X.1"))]



levels(pokedex$Type1)
type1 <- table(pokedex$Type1)
levels(pokedex$Type2)
type2 <- table(pokedex$Type2, exclude = "")
typings <- type1 + type2



##Leveling Rates
rates <- c("Fast", "Medium Fast", "Medium Slow", "Slow")


for(i in 0:2){
  pokedex$LevelingRate[pokedex$DexNum == 720 + i] <- rates[4]
}

pokedex$LevelingRate[pokedex$DexNum == 719] <- rates[4]


##Legendary Status
pokedex$Legendary <- "No"
#pokedex$Legendary[raw$Legendary == "True"] <- "Legendary"

categories <- c("Legendary", "Mythical", "Pseudo", "No")

pokedex$Legendary[pokedex$DexNum == 706] <- categories[3]


##Adding new pokemon
##DexNum  Name  Type1  Type2  TotalStat  HP  Attack  Defense  SpAtk  SpDef  Speed  Generation  Legendary  Leveling Rate
addition <- c("722", "Rowlet", "Grass", "Flying", 320, 68, 55, 50, 50, 42, 7, categories[4], rates[3])

test <- rbind(pokedex, addition)


##Gender Ratio
pokedex$FemaleRatio <- 50

for(i in 0:2){
  pokedex$FemaleRatio[pokedex$DexNum == 806 + i] <- NA
}

pokedex$FemaleRatio[pokedex$DexNum == 809] <- NA


##Catch Rate
pokedex$CatchRate <- 45

for(i in 0:2){
  pokedex$CatchRate[pokedex$DexNum == 255 + i] <- 45
}

pokedex$CatchRate[pokedex$DexNum == 26] <- 75

pokedex$CatchRate[pokedex$Legendary == categories[1]] <- 3


##Subset by generation
Kanto <- subset(pokedex, Generation == 1)
Johto <- subset(pokedex, Generation == 2)
Hoenn <- subset(pokedex, Generation == 3)
Sinnoh <- subset(pokedex, Generation == 4)
Unova <- subset(pokedex, Generation == 5)
Kalos <- subset(pokedex, Generation == 6)
Alola <- subset(pokedex, Generation == 7)

Kanto <- subset(pokedex, Generation == 1 & Form != "Mega")
Johto <- subset(pokedex, Generation == 2 & Form != "Mega")
Hoenn <- subset(pokedex, Generation == 3 & Form != "Mega")
Sinnoh <- subset(pokedex, Generation == 4 & Form != "Mega")
Unova <- subset(pokedex, Generation == 5 & Form != "Mega")
Kalos <- subset(pokedex, Generation == 6 & Form != "Mega")
Alola <- subset(pokedex, Generation == 7 & Form != "Mega")

MegaNum <- pokedex$DexNum[pokedex$Form == "Mega"]
Megas <- subset(pokedex, DexNum %in% MegaNum)
boxplot(Megas$TotalStat~Megas$Form == "Mega", horizontal = TRUE, ylab = "Mega Evolved", xlab = "Total IVs")
PreM <- Megas$TotalStat[Megas$Form != "Mega" & Megas$Name != "Charizard Y" & Megas$Name != "Mewtwo Y"]
PostM <- Megas$TotalStat[Megas$Form == "Mega" & Megas$Name != "Charizard Y" & Megas$Name != "Mewtwo Y"]

table(pokedex$Legendary, pokedex$Generation)
table(Kanto$Legendary)/length(Kanto$Legendary)
table(Johto$Legendary)/length(Johto$Legendary)
table(Hoenn$Legendary)/length(Hoenn$Legendary)
table(Sinnoh$Legendary)/length(Sinnoh$Legendary)
table(Unova$Legendary)/length(Unova$Legendary)
table(Kalos$Legendary)/length(Kalos$Legendary)
table(Alola$Legendary)/length(Alola$Legendary)

##Subset by type
grass <- subset(pokedex, Type1 == "Grass" | Type2 == "Grass")
fire <- subset(pokedex, Type1 == "Fire" | Type2 == "Fire")
water <- subset(pokedex, Type1 == "Water" | Type2 == "Water")
poison <- subset(pokedex, Type1 == "Poison" | Type2 == "Poison")
dark <- subset(pokedex, Type1 == "Dark" | Type2 == "Dark")
fairy <- subset(pokedex, Type1 == "Fairy" | Type2 == "Fairy")
steel <- subset(pokedex, Type1 == "Steel" | Type2 == "Steel")
flying <- subset(pokedex, Type1 == "Flying" | Type2 == "Flying")
normal <- subset(pokedex, Type1 == "Normal" | Type2 == "Normal")
dragon <- subset(pokedex, Type1 == "Dragon" | Type2 == "Dragon")
psychic <- subset(pokedex, Type1 == "Psychic" | Type2 == "Psychic")
fighting <- subset(pokedex, Type1 == "Fighting" | Type2 == "Fighting")
bug <- subset(pokedex, Type1 == "Bug" | Type2 == "Bug")
ice <- subset(pokedex, Type1 == "Ice" | Type2 == "Ice")
electric <- subset(pokedex, Type1 == "Electric" | Type2 == "Electric")
ground <- subset(pokedex, Type1 == "Ground" | Type2 == "Ground")
rock <- subset(pokedex, Type1 == "Rock" | Type2 == "Rock")

table(grass$Legendary)/length(grass$DexNum)

round(rnorm(n = 30, mean = 0.7, sd = 0.125), digits = 3)

write.csv(pokedex, file = "pokedex.csv", row.names = FALSE)
