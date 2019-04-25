###T test that dragons are taller/longer than average pokemon
dragons <- subset(pokedex, Type1 == "Dragon" | Type2 == "Dragon")
qqnorm(dragons$Height)
qqline(dragons$Height)
t.test(dragons$Height, mu = mean(pokedex$Height), alternative = "greater")
t.test(dragons$Height, mu = mean(pokedex$Height[pokedex$Type1 != "Dragon" & pokedex$Type2 != "Dragon"]), alternative = "greater")

###Independent two sample T test comparing total stats of mega vs nonmega forms
mega_stat <- pokedex$TotalStat[pokedex$Form == "Mega"]
nonmega_stat <- pokedex$TotalStat[pokedex$Form != "Mega"]
boxplot(mega_stat, nonmega_stat, names = c("Mega", "Non-Mega"))
t.test(mega_stat, nonmega_stat, alternative = "greater")

###Matched pair T test comparing size of mega and nonmega forms
nonmega_height <- pokedex$Height[pokedex$Form != "Mega"]
mega_height <- pokedex$Height[pokedex$Form == "Mega" & pokedex$Name != "Charizard Y" & pokedex$Name != "Mewtwo Y"]
boxplot(mega_height, nonmega_height, names = c("Mega", "Non-Mega"))
qqnorm(mega_height)
qqline(mega_height)
qqnorm(nonmega_height)
qqline(nonmega_height)
t.test(mega_height, nonmega_height, alternative = "greater")

###Matched Pair T test comparing total stat of Alolan Forms to their original Kanto form
AlolanForms <- subset(pokedex, Form == "Alolan")
qqnorm(AlolanForms$SpAtk)
qqline(AlolanForms$SpAtk)
KantoAlolan <- subset(pokedex, DexNum %in% AlolanForms$DexNum & Generation == 1)
qqnorm(KantoAlolan$SpAtk)
qqline(KantoAlolan$SpAtk)
t.test(AlolanForms$SpAtk, KantoAlolan$SpAtk, paired = TRUE, alternative = "two.sided")

###Anova for total stats as a function of legendary status
boxplot(pokedex$TotalStat~pokedex$Legendary)
legend.aov <- aov(pokedex$TotalStat~pokedex$Legendary)
anova(legend.aov)
TukeyHSD(legend.aov)

###Anova for Smogon Tiers
boxplot(pokedex$TotalStat~pokedex$Smogon)
smogon.aov <- aov(pokedex$TotalStat~pokedex$Smogon)
anova(smogon.aov)
TukeyHSD(smogon.aov)

##Power for Rattata sample - greater alt w/ d = 0.5
pwr.t.test(n = length(ratatta), d = 0.5, sig.level = 0.05, power = , type = "one.sample", alternative = "greater")

##Height and weight
cor(pokedex$Height, pokedex$Weight)
weight.lm <- lm(Weight ~ Height, data = pokedex)
summary(weight.lm)
plot(pokedex$Weight~pokedex$Height)
abline(-11.821, 65.531, col = "red", lwd = 2)
legend(9.3, 99, legend = c("y = 65.531x + -11.821", "R2 = 0.4443"), col = c("red"), lty=c(1), lwd=c(2), cex = 0.8)

###Total Stats and Catch Rate
cor(pokedex$TotalStat, pokedex$CatchRate)
catch.lm <- lm(CatchRate ~ TotalStat, data = pokedex)
summary(catch.lm)
plot(pokedex$CatchRate~pokedex$TotalStat)
abline(-0.430933, 282.65423, col = "red", lwd = 2)
