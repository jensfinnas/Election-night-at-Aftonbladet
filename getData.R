# ==============================================
# Election night at Aftonbladet, 
# by Jens Finnäs, Journalism++ Stockholm
# ==============================================
# PART I: GET THE DATA
# ==============================================


## Libraries
require(stringr)
require(httr)
require(rjson)
require(gdata)
require(xlsx)
require(data.table)

# Open the xlsx file with socio-economic data about the municipalities
kommunDB <- read.xlsx("kommundata.xlsx", sheetIndex=1)

# Loopa alla kommuner för att hämta hem valresultat från Aftonbladets API
# Målet är att omforma JSON-datan från API:et till en tvådimensionell tabell
# som vi kan slå ihop med den socioekonomiska datan (arbetslöshet etc) från Excel.

# Spara kommunresultaten i en egen data frame
# This function fetches the results of a given election at municipality and district level from 
# the result API of Aftonbladet.
# electionID follows this pattern: "val{YEAR}{ELECTION}" where ELECTION is R for "riksdagsval" (national),
# K is "kommunval" (municipality) and L is "landstingsval" (regional).
# Results are stored in the global "res" environment.
# If overwrite is set to FALSE only non-existing municipalites are fetched.
getResultDataFromAPI <- function(electionID, overwrite) {
  # If overwrite, create an empty data frame for municpality results and district results
  if (overwrite) {
    res$kommun[[electionID]] <<- data.frame(ID=character())
    res$distrikt[[electionID]] <<- data.frame(ID=character())    
  }
  
  # Iterate municipalities
  for (kommunID in kommunDB$code) {
    # Make sure that the municiplaity does not already exist in database
    if (nrow(subset(res$kommun[[electionID]], ID == kommunID)) == 0 ) {
      # Generate URL
      baseUrl <- "http://valnatt.aftonbladet.se/api/election/ELECTIONID/KOMMUN"
      kommunUrl <- str_replace_all(baseUrl, "KOMMUN", formatC(kommunID, width=2, digits=4, flag="0")) 
      kommunUrl <- str_replace_all(kommunUrl, "ELECTIONID", electionID)
      print(kommunUrl)
      # Open and read JSON from API
      kommunDataJSON <- fromJSON(file=kommunUrl)
      
      # We'll store the data of the municipality in a temporary list. 
      kommunRow = list()
      # Get some meta data about the municipality
      kommunRow$ID <- kommunDataJSON$ID
      kommunRow$largest_party <- kommunDataJSON$largest_party
      kommunRow$largest_party_percent = kommunDataJSON$largest_party
      kommunRow$valdeltagande = kommunDataJSON[['valdeltagande']]$PROCENT
      kommunRow$KLARA_VALDISTRIKT = kommunDataJSON$KLARA_VALDISTRIKT
      kommunRow$ALLA_VALDISTRIKT = kommunDataJSON$ALLA_VALDISTRIKT
      
      # Iterate all parties to get the number and share of votes at municipality level
      for (party in names(kommunDataJSON$parties)) {
        d <- kommunDataJSON$parties[[party]]
        kommunRow[ paste(c("MANDAT", party), collapse="_") ] <- d$MANDAT
        kommunRow[ paste(c("MANDAT_ANDRING", party), collapse="_") ] <- d$MANDAT_ANDRING
        kommunRow[ paste(c("ROSTER", party), collapse="_") ] <- d$ROSTER
        kommunRow[ paste(c("PROCENT", party), collapse="_") ] <- d$PROCENT
        kommunRow[ paste(c("PROCENT_ANDRING", party), collapse="_") ] <- d$PROCENT_ANDRING
      }
      # Add the temporary municipality data list to the database
      # First make sure that the row has the same number of columns as the database
      # If new local parties appear we won't be able to merge.
      if (nrow(res$kommun[[electionID]]) == 0) {
        res$kommun[[electionID]] <<- as.data.frame(kommunRow, stringsAsFactors=FALSE)
      }
      else {
        # Check if there are new cols in the row list, or missing cols compared to the database
        newCols <- names(kommunRow)[!(names(kommunRow) %in% names(res$kommun[[electionID]]))]
        missingCols <- names(res$kommun[[electionID]])[!(names(res$kommun[[electionID]]) %in% names(kommunRow))]
        # Add missing cols to the row list and the database
        if (length(newCols) > 0 || length(missingCols) > 0) {
          for (newCol in newCols) {
            res$kommun[[electionID]][[newCol]] <<- NA
          }
          
          for (missingCol in missingCols) {
            kommunRow[missingCol] <- NA
          }   
        }
        res$kommun[[electionID]] <<- rbind(res$kommun[[electionID]], kommunRow)
      }
      
      # Iterate districts to get the results from them as well
      for (kkID in names(kommunDataJSON$kommun_kretsar)) {
        kommun_krets <- kommunDataJSON$kommun_kretsar[[kkID]]
        for (vdID in names(kommun_krets$valdistrikt)) {
          valdistrikt <- kommun_krets$valdistrikt[[vdID]]
          
          # Just as with the municipalities, store the district row data in a temporary list
          distriktRow <- list()
          
          # Get meta data
          distriktRow$ID = valdistrikt$ID
          distriktRow$NAMN = valdistrikt$NAMN
          distriktRow$valdeltagande = valdistrikt[['valdeltagande']]$PROCENT
          distriktRow$largest_party = valdistrikt$largest_party
          distriktRow$largest_party_percent = valdistrikt$largest_party_percent
          distriktRow$KOMMUN_NAMN = kommunRow$NAMN
          distriktRow$KOMMUN_ID = kommunRow$ID
          
          # Get votes for each party
          for (party in names(valdistrikt$parties)) {
            d <- valdistrikt$parties[[party]]
            distriktRow[ paste(c("ROSTER", party), collapse="_") ] <- d$ROSTER
            distriktRow[ paste(c("PROCENT", party), collapse="_") ] <- d$PROCENT
            distriktRow[ paste(c("PROCENT_ANDRING", party), collapse="_") ] <- d$PROCENT_ANDRING
          }
          distriktRow <- as.data.frame(distriktRow)
          # Merge row with database
          if (nrow(res$distrikt[[electionID]]) == 0) {
            res$distrikt[[electionID]] <<- as.data.frame(distriktRow, stringsAsFactors=FALSE)
          }
          else {
            # Add missing columns so that we can merge
            newCols <- names(distriktRow)[!(names(distriktRow) %in% names(res$distrikt[[electionID]]))]
            missingCols <- names(res$distrikt[[electionID]])[!(names(res$distrikt[[electionID]]) %in% names(distriktRow))]
            if (length(newCols) > 0 || length(missingCols) > 0) {
              for (newCol in newCols) {
                res$distrikt[[electionID]][[newCol]] <<- NA
              }
              
              for (missingCol in missingCols) {
                distriktRow[missingCol] <- NA
              }   
            }
            res$distrikt[[electionID]] <<- rbind(res$distrikt[[electionID]], distriktRow)
          }
        }
      } 
    }
  }  
  print("Done!")
}

# This is the global data object that we'll use to store the data
res <- new.env()
# ...it'll consists of municipalty results and district results
res$kommun <- new.env()
res$distrikt <- new.env()

# Example: how to use the function, get national ("R") and local election ("K") results from API and overwrite
# any previous data from these elections

getResultDataFromAPI("val2014R", TRUE) 
getResultDataFromAPI("val2014K", TRUE)

# Save the results file
save(res, file="valresultat.Rdata")

# Merge the socio-economic data with the parliament results data at municipality level
kommunDB <- merge(kommunDB, res$kommun$val2014R, by.x="code", by.y="ID", all.x=TRUE)

# Next step: merge the results from the local elections
# In the local elections there are tons of small local parties. We manually select the 
# most interesting parties to avoid column overflow.
kommunParties <- c("V", "S", "MP", "C", "FP", "M","KD", "SD", "FI", "PP", "SP")
kRes <- res$kommun$val2014K[,1:6]
dRes <- res$distrikt$val2014K[,1:6]
for (party in kommunParties) {
  procentCol <- paste(c("PROCENT",party), collapse="_")
  procentChangeCol <- paste(c("PROCENT_ANDRING",party), collapse="_")
  mandatCol <- paste(c("MANDAT",party), collapse="_")
  mandatChangeCol <- paste(c("MANDAT_ANDRING",party), collapse="_")
  votesCol <- paste(c("ROSTER",party), collapse="_")
  kRes[[procentCol]] <- res$kommun$val2014K[[procentCol]]
  kRes[[procentChangeCol]] <- res$kommun$val2014K[[procentChangeCol]]
  kRes[[mandatCol]] <- res$kommun$val2014K[[mandatCol]]
  kRes[[mandatChangeCol]] <- res$kommun$val2014K[[mandatChangeCol]]
  kRes[[votesCol]] <- res$kommun$val2014K[[votesCol]]

  dRes[[procentCol]] <- res$distrikt$val2014K[[procentCol]]
  dRes[[procentChangeCol]] <- res$distrikt$val2014K[[procentChangeCol]]
  dRes[[mandatCol]] <- res$distrikt$val2014K[[mandatCol]]
  dRes[[mandatChangeCol]] <- res$distrikt$val2014K[[mandatChangeCol]]
  dRes[[votesCol]] <- res$distrikt$val2014K[[votesCol]]
  
}
# Add a "KV_" prefix to the columns so that we can keep national and local results separate
colnames(kRes) <- lapply(colnames(kRes), function(d) paste(c("KV", d), collapse="_"))
colnames(dRes) <- lapply(colnames(dRes), function(d) paste(c("KV", d), collapse="_"))

# Merge!
kommunDB <- merge(kommunDB, kRes, by.x="code", by.y="KV_ID", all.x=TRUE)
distriktDB <- merge(res$distrikt$val2014R, dRes, by.x="ID", by.y="KV_ID", all.x=TRUE)

# Export to Excel -  for non-R reporters :)
write.xlsx(x = kommunDB, file = "data/2014_resultat_kommun.xlsx", sheetName = "data", row.names = TRUE)
write.table(distriktDB, "data/2014_resultat_distrikt.csv", sep="\t", row.names=TRUE, col.names=TRUE) 

