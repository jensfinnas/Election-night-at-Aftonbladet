
## Libraries
require(stringr)
require(httr)
require(rjson)
require(gdata)
require(xlsx)
require(data.table)


# Läs in filen med kommundata (arbetslöshet etc)
kommunDB <- read.xlsx("kommundata.xlsx", sheetIndex=1)

# Loopa alla kommuner för att hämta hem valresultat från Aftonbladets API
# Målet är att omforma JSON-datan från API:et till en tvådimensionell tabell
# som vi kan slå ihop med den socioekonomiska datan (arbetslöshet etc) från Excel.

# Spara kommunresultaten i en egen data frame
getResultDataFromAPI <- function(electionID) {
  kommunResults <- data.frame()
  distriktResults <- data.frame()
  parties <- data.frame(abbr=character(), name=character())
  
  for (kommunID in kommunDB$code) {
    # Skapa url
    baseUrl <- "http://valnatt.aftonbladet.se/api/election/ELECTIONID/KOMMUN"
    kommunUrl <- str_replace_all(baseUrl, "KOMMUN", formatC(kommunID, width=2, digits=4, flag="0")) 
    kommunUrl <- str_replace_all(kommunUrl, "ELECTIONID", electionID)
    print(kommunUrl)
    # Öppna och lös JSON-filen från API:et
    kommunDataJSON <- fromJSON(file=kommunUrl)
    
    # Spara datan för den enskilda kommunen tillfälligt i ett eget objekt
    kommunRow = list()
    # Spara metadata om om kommunen
    kommunRow$ID <- kommunDataJSON$ID
    kommunRow$largest_party <- kommunDataJSON$largest_party
    kommunRow$largest_party_percent = kommunDataJSON$largest_party
    kommunRow$valdeltagande = kommunDataJSON[['valdeltagande']]$PROCENT
    kommunRow$KLARA_VALDISTRIKT = kommunDataJSON$KLARA_VALDISTRIKT
    kommunRow$ALLA_VALDISTRIKT = kommunDataJSON$ALLA_VALDISTRIKT
    
    # Gå igenom alla partier och spara antal och andel röster, samt mandat
    for (party in names(kommunDataJSON$parties)) {
      d <- kommunDataJSON$parties[[party]]
      kommunRow[ paste(c("MANDAT", party), collapse="_") ] <- d$MANDAT
      kommunRow[ paste(c("MANDAT_ANDRING", party), collapse="_") ] <- d$MANDAT_ANDRING
      kommunRow[ paste(c("ROSTER", party), collapse="_") ] <- d$ROSTER
      kommunRow[ paste(c("PROCENT", party), collapse="_") ] <- d$PROCENT
      kommunRow[ paste(c("PROCENT_ANDRING", party), collapse="_") ] <- d$PROCENT_ANDRING
    }
    # Lägg kommundatan till databasfilen
    if (nrow(kommunResults) == 0) {
      kommunResults <- as.data.frame(kommunRow, stringsAsFactors=FALSE)
    }
    else {
      # Kolla om det finns flera partier i kommunen än i datafilen...
      newCols <- names(kommunRow)[!(names(kommunRow) %in% names(kommunResults))]
      missingCols <- names(kommunResults)[!(names(kommunResults) %in% names(kommunRow))]
      if (length(newCols) > 0 || length(missingCols) > 0) {
        for (newCol in newCols) {
          kommunResults[[newCol]] <- NA
        }
        
        for (missingCol in missingCols) {
          kommunRow[missingCol] <- NA
        }   
      }
      kommunResults <- rbind(kommunResults, kommunRow)
    }
      
    # Loopa alla valkretsar i kommunen för att hämta valresultat
    for (kkID in names(kommunDataJSON$kommun_kretsar)) {
      kommun_krets <- kommunDataJSON$kommun_kretsar[[kkID]]
      for (vdID in names(kommun_krets$valdistrikt)) {
        valdistrikt <- kommun_krets$valdistrikt[[vdID]]
        
        # Spara, liksom för kommunerna, tillfälligt datan för valkretsen i ett eget objekt
        distriktRow <- list()
        
        # Hämta metadata för valdistriktet
        distriktRow$ID = valdistrikt$ID
        distriktRow$NAMN = valdistrikt$NAMN
        distriktRow$valdeltagande = valdistrikt[['valdeltagande']]$PROCENT
        distriktRow$largest_party = valdistrikt$largest_party
        distriktRow$largest_party_percent = valdistrikt$largest_party_percent
        distriktRow$KOMMUN_NAMN = kommunRow$NAMN
        distriktRow$KOMMUN_ID = kommunRow$ID
        
        # Hämta antal och andel röster för partierna i valdistriktet 
        for (party in names(valdistrikt$parties)) {
          d <- valdistrikt$parties[[party]]
          distriktRow[ paste(c("ROSTER", party), collapse="_") ] <- d$ROSTER
          distriktRow[ paste(c("PROCENT", party), collapse="_") ] <- d$PROCENT
          distriktRow[ paste(c("PROCENT_ANDRING", party), collapse="_") ] <- d$PROCENT_ANDRING
        }
        distriktRow <- as.data.frame(distriktRow)
        if (nrow(distriktResults) == 0) {
          distriktResults <- as.data.frame(distriktRow, stringsAsFactors=FALSE)
        }
        else {
          # Kolla om det finns flera partier i kommunen än i datafilen...
          newCols <- names(distriktRow)[!(names(distriktRow) %in% names(distriktResults))]
          missingCols <- names(distriktResults)[!(names(distriktResults) %in% names(distriktRow))]
          if (length(newCols) > 0 || length(missingCols) > 0) {
            for (newCol in newCols) {
              distriktResults[[newCol]] <- NA
            }
            
            for (missingCol in missingCols) {
              distriktRow[missingCol] <- NA
            }   
          }
          distriktResults <- rbind(distriktResults, distriktRow)
        }
      }
    }
  }  
  return(list(kommun = kommunResults, distrikt = distriktResults))
}
dat2010R <- getResultDataFromAPI("val2010R") 
load("data/dat2010R.RData")
dat2010K <- getResultDataFromAPI("val2010K") 
# Slå ihop den socioekonomiska datan och valresultaten på kommunnivå
kommunDB <- merge(kommunDB, dat2010R$kommun, by.x="code", by.y="ID", all.x=TRUE)

# Välj ut de partier vi vill analysera, resten klumpas ihop som "övriga" 
kommunParties <- c("V", "S", "MP", "C", "FP", "M","KD", "SD", "FI", "PP", "SP")
kRes <- dat2010K$kommun[,1:6]
for (party in kommunParties) {
  procentCol <- paste(c("PROCENT",party), collapse="_")
  procentChangeCol <- paste(c("PROCENT_ANDRING",party), collapse="_")
  mandatCol <- paste(c("MANDAT",party), collapse="_")
  mandatChangeCol <- paste(c("MANDAT_ANDRING",party), collapse="_")
  votesCol <- paste(c("ROSTER",party), collapse="_")
  kRes[[procentCol]] <- dat2010K$kommun[[procentCol]]
  kRes[[procentChangeCol]] <- dat2010K$kommun[[procentChangeCol]]
  kRes[[mandatCol]] <- dat2010K$kommun[[mandatCol]]
  kRes[[mandatChangeCol]] <- dat2010K$kommun[[mandatChangeCol]]
  kRes[[votesCol]] <- dat2010K$kommun[[votesCol]]
}
colnames(kRes) <- lapply(colnames(kRes), function(d) paste(c("KV", d), collapse="_"))

# Slå ihop kommunvalsresultaten med kommunfilen
kommunDB <- merge(kommunDB, kRes, by.x="code", by.y="KV_ID", all.x=TRUE)


save(dat2010R, file="data/dat2010R.RData")
save(dat2010K, file="data/dat2010K.RData")

