library(tidyverse)
rm(list = ls())
tmp <- read.csv2('I:/norgast/Rapportek_kvartalsrapportmottakere.csv')
maxant_epost <- 10
tmp2 <- separate(tmp, epost, letters[1:maxant_epost], sep = '\n')

samlet <- as.matrix(tmp2[,c('a', 'resh')])
for (i in 2:maxant_epost) {
  samlet <- rbind(samlet, as.matrix(tmp2[!is.na(tmp2[, i]), c(i, maxant_epost+1)]))
}

samlet <- as.data.frame(samlet)
names(samlet)[1] <- 'epost'
samlet$epost <- as.character(samlet$epost) %>% trimws()
samlet$resh <- as.character(samlet$resh) %>% trimws() %>% as.numeric()

write.csv2(samlet, 'I:/norgast/rapporteket_epost.csv', row.names = F)
