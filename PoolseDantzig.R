library(cbsodataR)
library(tidyverse)

query_url <- paste0('http://opendata.cbs.nl/solr/CBS_nl/select'
                    , "?q=Polen"
                    , "&wt=json"
                    , "&start=0"
                    , "&rows=200")

result <- jsonlite::read_json(query_url, simplifyVector = TRUE)

tbls <- result$response$docs %>% select(PublicationKey) %>% pull()

vars <- c('Landen', 'Land', 'WoonlandVanGasten', 'Woonland', 'Migratieachtergrond',
          'MigratieachtergrondKind', 'MigratieachtergrondMoeder', 'LandVanHerkomstVestiging', 
          'LandVanHerkomst', 'Herkomstgroepering', 'HerkomstgroeperingKind', 'TotaalHerkomstgroeperingen',
          'Uitvoerkenmerken', 'LandVanUiteindelijkeZeggenschapUCI', 'Geboorteland', 'LandenGroepen',
          'LandVanLaden', 'LandVanLossen', 'GeboortelandGroep')

result <- data.frame()
drop <- c()
for (tbl in tbls) {
#  tbl = "80498ned"
  meta <- cbs_get_meta(tbl)
  Identifier <- meta$TableInfos$Identifier   
  ShortTitle <- meta$TableInfos$ShortTitle
  for (Variable in vars) {
    desc <- data.frame(Key = NA,
                       Title = NA,
                       Description = NA,
                       CategoryGroupID = NA)
    try(desc <- meta[[Variable]] %>% filter(Title == "Polen"), silent = TRUE)
    if(nrow(desc) == 0) {    desc <- data.frame(Key = NA,
                                                Title = NA,
                                                Description = NA,
                                                CategoryGroupID = NA)}
    row <- cbind(Identifier, ShortTitle, Variable, desc)
    if(!is.na(row$Description)){
      result <- rbind(result, row)
    }
    
  }  
  print("errors can't stop me")
  
  if(length(agrep("Nationaliteit", meta$DataProperties$Key)) > 0 |
     length(agrep("Bedrijf", meta$DataProperties$Key)) > 0 |
     length(agrep("Luchthaven", meta$DataProperties$Key)) > 0 |
     length(agrep("WereldEUEnNietEU", meta$DataProperties$Key)) > 0|
     length(agrep("Plaats", meta$DataProperties$Key)) > 0
     ) {
    drop <- c(drop, Identifier)}
  try({tmp <- meta$Landen %>% filter(Title == "Polen"); if(is.na(tmp$Description)) {drop <- c(drop, Identifier)}
    }, silent = TRUE)
  try({tmp <- meta$LandVanHerkomst %>% filter(Title == "Polen"); if(is.na(tmp$Description)) {drop <- c(drop, Identifier)}
  }, silent = TRUE)
}

minor <- c('81202ned', '83135NED', '70095ned', '70022ned', '71454ned',
           '00374hvv', '71099ned', '70849ned', '83113NED', '71121NED',
           '71120NED') 

setdiff(setdiff(setdiff(tbls, drop), result$Identifier), minor)

unique(agrep("Dantzig", result$Description, value = T))

write.csv(result, "PoolseDantzig.csv", row.names = F)
