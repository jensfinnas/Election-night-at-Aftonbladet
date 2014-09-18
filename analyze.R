# ==============================================
# Election night at Aftonbladet, 
# by Jens Finnäs, Journalism++ Stockholm
# ==============================================
# PART II: UNDERSTAND THE DATA
# ==============================================

library(ggplot2)
library(gridExtra)
library(rjson)
library(xlsx)
library(png)
library(grid)

# Store the municipality data in this data file
kDat <- kommunDB
# Make the turnout column numeric
kDat$valdeltagande <- as.numeric(gsub(",",".", kDat$valdeltagande))
# Calculate the how many percent of the districts have been counted
kDat$KLAR_PROCENT <-  kDat$KLARA_VALDISTRIKT / kDat$ALLA_VALDISTRIKT 
summary(kDat$KLAR_PROCENT)

# Store the district data in this data file
dDat <- distriktDB
dDat$name <- dDat$NAMN # Just a hack
# Turnout => numeric
dDat$valdeltagande <- as.numeric(gsub(",",".", dDat$valdeltagande))

# Parties and party colors
parties <- c("V", "S", "MP", "C", "FP", "M","KD", "SD","FI")
partyColors <- c( "#AF0000", "#EE2020", "#83CF39", "#009933", "#6BB7EC", "#1B49DD", "#231977", "#DDDD00", "#D9308E")

# Function to add the Aftonbladet logo to a chart
addLogo <- function(p) {
  # Get the logo
  img <- readPNG("aftonbladet.png")
  g <- rasterGrob(img)
  
  size = unit(0.06, "npc")
  
  # Set up the layout for grid 
  heights = unit.c(unit(1, "npc") - size, size)
  widths = unit.c(unit(1, "npc"))
  lo = grid.layout(2, 1, widths = widths, heights = heights)
  # Show the layout
  grid.show.layout(lo)
  
  # Position the elements within the viewports
  grid.newpage()
  pushViewport(viewport(layout = lo))
  
  # The plot
  pushViewport(viewport(layout.pos.row=1, layout.pos.col = 1))
  print(p, newpage=FALSE)
  popViewport()
  
  # The logo
  pushViewport(viewport(layout.pos.row=2, layout.pos.col = 1))
  print(grid.draw(g), newpage=FALSE)
  popViewport()
  
  # The title
  pushViewport(viewport(layout.pos.row=1, layout.pos.col = 1))
  print(grid.draw(title), newpage=FALSE)
  popViewport()
  popViewport()  
}

# ========================================================== #
# VISUALIZATION/MAP: 
# Where to parties get the most/least votes?
# Where do they grow/shrink?
# This code will genereate a JSON file that is used to make this map:
# http://ab-compass-browser.herokuapp.com/valkarta/
# ========================================================== #

# Funtion that adds map to list of maps
addMap <- function(mapItem) {
  if (is.null(maps)) {
    maps <- list(mapItem)
  }
  else {
    maps <- c(maps, list(mapItem))
  }
  return(maps)
}
# Write to file
writeMapsFile <- function() {
  write(toJSON(maps), file = "~/Sites/aftonbladet-valkompass-browser/valkarta/data/maps.json")  
}
# Clear maps list
maps <- NULL

# Get the top and bottom municipalities for each party
for (party in parties) {
  print(party)
  # Strongest/weakest municipalities (by share of votes)
  col <- paste(c("PROCENT", party), collapse="_")
  top10 <- kDat[order(kDat[[col]], decreasing = TRUE),][1:10,]
  bottom10 <- kDat[order(kDat[[col]], decreasing = FALSE),][1:10,]
  # Strongest/weakest municipalities (by change)
  colChange <- paste(c("PROCENT_ANDRING", party), collapse="_")
  top10change <- kDat[order(kDat[[colChange]], decreasing = TRUE),][1:10,]
  bottom10change <- kDat[order(kDat[[colChange]], decreasing = FALSE),][1:10,]
  bottom10change <- bottom10change[bottom10change[[colChange]] < 0 ,] 
  
  # Make maps!
  mapItemTop <- list(
    "title" = paste("De starkaste kommunerna för ", party),
    "description" = "",
    "areas" = top10[['code']],
    "values" = top10[[col]],
    "category" = party
  )
  maps <- addMap(mapItemTop)
  mapItemBottom <- list(
    "title" = paste("De svagaste kommunerna för ", party),
    "description" = "",
    "areas" = bottom10[['code']],
    "values" = bottom10[[col]],
    "category" = party
  )
  maps <- addMap(mapItemBottom)

  mapItemBottom <- list(
    "title" = paste("Här gick ", party, " fram mest"),
    "description" = "",
    "areas" = top10change[['code']],
    "values" = top10change[[col]],
    "category" = party
  )
  maps <- addMap(mapItemBottom)
  
  mapItemBottom <- list(
    "title" = paste("Här backade ", party, " mest"),
    "description" = "",
    "areas" = bottom10change[['code']],
    "values" = bottom10change[[col]],
    "category" = party
  )
  maps <- addMap(mapItemBottom)
  
  # Print a list of the municipalites
  print(data.frame(top10[['name']], top10[[col]]))
  print(data.frame(bottom10[['name']], bottom10[[col]]))
  print(data.frame(top10change[['name']], top10change[[colChange]]))
  print(data.frame(bottom10change[['name']], bottom10change[[colChange]]))
}

# We can also make custom maps based on tailored filterings of the data

# All municipalities where SD got seats
areas <- subset(kDat, KV_MANDAT_SD > 0)
mapItem <- list(
  "title" = "De kommuner där SD fick mandat",
  "description" = "",
  "areas" = areas$code,
  "values" = areas$KV_MANDAT_SD,
  "category" = "SD"
)
maps <- addMap(mapItem)

# All mun. where FI got seats
areas <- subset(kDat, KV_MANDAT_FI > 0)
mapItem <- list(
  "title" = "Kommunerna där FI fick mandat i kommunvalet",
  "description" = "",
  "areas" = areas$code,
  "values" = areas$KV_MANDAT_FI,
  "category" = "FI"
)
maps <- addMap(mapItem)


# All mun. where Svenskarnas parti got votes
areas <- subset(kDat, KV_PROCENT_SP > 0)
subset(dDat, KV_PROCENT_SP > 0)
mapItem <- list(
  "title" = "Kommunerna där Svenskarnas Parti fick röster",
  "description" = "",
  "areas" = areas$code,
  "values" = areas$KV_MANDAT_SP,
  "category" = "other"
)
maps <- addMap(mapItem)

# All mun. where S+MP+V has a majority of the votes in the local elections
areas <- subset(kDat, (KV_PROCENT_S + KV_PROCENT_MP + KV_PROCENT_V)  > 50)
mapItem <- list(
  "title" = "Kommunerna där S, MP och V har egen majoritet",
  "description" = "",
  "areas" = areas$code,
  "values" = areas$KV_MANDAT_FI,
  "category" = "other"
)
maps <- addMap(mapItem)

# Municpalites where SD is bigger han MP
areas <- subset(kDat, (PROCENT_MP < PROCENT_SD))
nrow(areas)
mapItem <- list(
  "title" = "Kommunerna där SD är större än MP",
  "description" = "",
  "areas" = areas$code,
  "values" = areas$KV_MANDAT_FI,
  "category" = "other"
)
maps <- addMap(mapItem)

# Highest turnout
areas <- kDat[order(kDat$valdeltagande, decreasing = TRUE),][1:10,]
mapItem <- list(
  "title" = "Kommunerna med högst valdeltagande",
  "description" = "",
  "areas" = areas$code,
  "values" = areas$valdeltagande,
  "category" = "other"  
)
maps <- addMap(mapItem)

# Write to file
writeMapsFile()

# ============================================================================ #
# VISUALIZATION: If ___ got to choose?
# Subset the 20 municipalities that ranks top/bottom on various factors such as 
# unemployment. What would the parliament look like if they got to choose?
# Used to make charts like this: https://twitter.com/jensfinnas/status/511274147003920385
# ============================================================================ #

# Set: 
# a) a column to base the ranking on
# b) the number of municipalities
# c) the title of the chart
# d) the labels of the top/bottom charts

ifXGotToChoose <- function(col, n, title, topBottomLabels) {
  # Sort the dataset based on the chosen column
  subsetTop <- kDat[order(kDat[[col]], decreasing = TRUE),][1:n,]
  subsetBottom  <- kDat[order(kDat[[col]], decreasing = FALSE),][1:n,]
  print(subsetTop)
  # Count results by party in the subseted municipalities
  votesTop <- c()
  votesBottom <- c()
  for (party in parties) {
    col <- paste(c("PROCENT", party), collapse="_")
    percentTop <- mean(subsetTop[[col]])
    percentBottom <- mean(subsetBottom[[col]])
    votesTop <- union(votesTop, percentTop) 
    votesBottom <- union(votesBottom, percentBottom) 
  }
  votes <- union(votesTop, votesBottom)
  # Create a data frame that is used to build the chart
  cat <- c(rep(topBottomLabels[1], length(parties)), rep(topBottomLabels[2], length(parties)))
  df <- data.frame(party=factor(c(parties,parties), levels = parties , ordered = TRUE),votes=votes, cat=cat)    
  
  # Make the chart
  p <- ggplot(data=df, aes(x=party, y=votes, fill=party)) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values = partyColors) + # Color by party
    facet_grid(. ~ cat) + # Split into two charts horizontally
    theme(
      legend.position = "none", # Hide legend
      plot.title = element_text(face="bold", size=20,vjust = 2), # Style title
      strip.text.x = element_text(size = 16) 
    ) + 
    geom_text(aes(label=sprintf("%1.1f%%",votes)), position=position_dodge(width=0.9), vjust=-0.5, size = 4) + # Bar labels
    ggtitle(title) + # Chart title
    xlab("") + # X axis title
    ylab("Procent")  # Y axis title
    
  
  # Print a list of the selected municipalites
  print(topBottomLabels[1])
  print(subsetTop$name)
  print(topBottomLabels[2])
  print(subsetBottom$name)

  # Add logo
  addLogo(p)
  return(p)
} 
# Generate charts!
ifXGotToChoose('foreignBorn', 20, "Så här röstar kommunerna \n där andelen utlandsfödda är...", c("...högst.", "...lägst."))
ifXGotToChoose('medianIncome', 20, "Så här röstar kommunerna \n där medianinkomsten är...", c("...högst.", "...lägst."))
ifXGotToChoose('unemploymentChange', 20, "Så här röstar kommunerna \n där arbetslösheten de senaste åren...", c("...växt kraftigast.", "...minskat mest."))
ifXGotToChoose('refugees', 20, "Så här röstar kommunerna \n där flyktingmottagningen är...", c("...störst.", "...minst."))
ifXGotToChoose('hasEducation', 20, "Så här röstar kommunerna \n där utbildningsnivån är...", c("...högst.", "...lägst."))
ifXGotToChoose('unemployment2013', 20, "Så här röstar kommunerna \n där arbetslösheten är...", c("...högst.", "...lägst."))
ifXGotToChoose('populationShare65plus', 20, "Så här röstar kommunerna \n där andelen 65+ är...", c("...störst.", "...minst."))
ifXGotToChoose('tractors', 20, "Så här röstar kommunerna \n där antalet traktorer per invånare är...", c("...störst.", "...minst"))
ifXGotToChoose('snowmobiles', 20, "Så här röstar kommunerna \n där antalet snöskotrar per invånare är...", c("...störst.", "...minst"))


# ============================================================================ #
# VISUALIZATION: This is how ___ voted
# Similar to the previous chart, but with only on subset. Eg how did the three
# largest cities vote?
# ============================================================================ #
drawSupportChartForSelection <- function(selectedKommuner, title) {
  votes <- c()
  # Get average number of votes in the selected municipaliteis
  for (party in parties) {
    col <- paste(c("PROCENT", party), collapse="_")
    percent <- mean(selectedKommuner[[col]])
    votes <- union(votes, percent) 
  }
  # Transform data
  df <- data.frame(party=factor(parties, levels = parties , ordered = TRUE) ,votes=votes)
  # Draw the chart
  p <- ggplot(data=df, aes(x=party, y=votes, fill=party)) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values = partyColors) + # Color by party
    theme(
      legend.position = "none", # Hide legend
      plot.title = element_text(face="bold", size=16,vjust = 2), # Style title
      strip.text.x = element_text(size = 14) 
    ) + 
    geom_text(aes(label=sprintf("%1.1f%%",votes)), position=position_dodge(width=0.9), vjust=-0.5, size = 4) + # Bar labels
    ggtitle(title) + # Chart titles
    xlab("") + # X axis title
    ylab("Procent")  # Y axis title
  
  # Add AB logo
  addLogo(p)
}

# How did the biggest cities vote?
# First subset some rows
selectedKommuner <- subset(kDat, municipalityType == "Storstäder")
# ...then draw the chart
drawSupportChartForSelection(selectedKommuner, "Storstäderna")

# ============================================================================ #
# ANALYSIS/VISUALIZATION: Correlation hunt
# Look for correaltions in the dataset. Are there socio-economic factors that
# correlates with the election results?
# ============================================================================ #
# Get a list of all columns that we want to analyze (primarily the the numeric ones)
correlationCols <- c()
categoryCols <- c()
for (col in names(kDat)) {
  value <- kDat[[col]]
  # Exclude non-numeric cols, mandates and number of votes
  if (is.numeric(value) && !grepl("MANDAT", col) && !grepl("ROSTER", col)) {
    correlationCols <- union(correlationCols, col)
  }
  if (!is.numeric(value)) {
    categoryCols <- union(categoryCols, col) 
  }
}
# Selected the right columns
correlationData <- kDat[,correlationCols]
# Create a correlation matrix
correlationTable <- cor(correlationData)
# Write the the correlation matrix to an Excel file for further analysis
write.xlsx(x = correlationTable, file = "correlation-table.xlsx", sheetName = "Corrlations", row.names = TRUE)  


# Visualize interesting correlations in a scatterplot
# "highlight" is a vector of municipality names that are highlighted with labels
drawScatterPlot <- function(dat,x, y, xLabel, yLabel, title, highlight) {
  # Filter the municipalities that are to be highlighted
  correlationData$name <- apply(dat, 1, FUN = function(d) { if (d[['name']] %in% highlight) d[['name']] else "" })
  correlationData$highlighted <- apply(dat, 1, FUN = function(d) { if (d[['name']] %in% highlight) "0" else "1"})
  
  # Create chart
  p <- ggplot(correlationData, aes_string(x=x, y=y, color="highlighted")) +
    geom_point(shape=19, alpha=1/2) + # Dot styling
    geom_text(aes(label=name), size=4, vjust=-0.7) + # Label styles
    scale_color_manual(values = c("black","bisque4")) + # Dot colors
    theme(
      legend.position = "none", # Hide legend
      axis.title.x = element_text(color="black", size=14), # X axis title style
      axis.title.y = element_text(color="black", size=14), # Y axis title style
      plot.title = element_text(face="bold", size=20,vjust = 2) # Chart title style
    ) +
    ggtitle(title) + # Chart title
    xlab(xLabel) + # X axis title
    ylab(yLabel) # Y axis title 
  
  # Add logo
  addLogo(p)
}

# SD growth vs education level
highlight <- c("Filipstad", "Lund", "Danderyd", "Munkfors", "Simrishamn", "Vellinge", "Lomma")
drawScatterPlot(kDat,"hasEducation","PROCENT_ANDRING_SD","Andel med utbildning","Ökning för SD (%)","SD växer där utbildningsnivån är låg", highlight)

# SD growth was population change
highlight <- c("Sundbyberg", "Sigtuna", "Örnsköldsvik", "Älvkarleby", "Färgelanda", "Strömstad")
drawScatterPlot(kDat,"populationChange","PROCENT_ANDRING_SD","Befolkningsförändring senaste fem åren (%)","Ökning för SD (%)","SD växer mest i kommuner som krymper", highlight)
nrow(kDat)

# FI result vs education level
highlight <- c("Stockholm", "Lund", "Solna", "Göteborg", "Malmö", "Umeå", "Uppsala")
drawScatterPlot(kDat,"hasEducation","PROCENT_ANDRING_FI","Andel med eftergymnasial utbildning (%)","Ökning för FI (%)","FI växer mest i kommuner med hög utbildningsnivå", highlight)


# ============================================================================ #
# ANALYSIS: Party results in different municipality types
# What is the average support of a party in different kinds of municpalities?
# ============================================================================ #

# SD change vs municipality type
SDvsMunType <- aggregate(kDat$PROCENT_ANDRING_SD , by=list(kDat$municipalityType), FUN=mean, na.rm=TRUE)
# SD change vs current local coverning
SDvsGoverning <- aggregate(kDat$PROCENT_ANDRING_SD , by=list(kDat$governing), FUN=mean, na.rm=TRUE)
write.xlsx(x = SDvsMunType, file = "output/SDs ökning vs SKL-typ.xlsx", sheetName = "data", row.names = TRUE)
colnames(SDvsMunType) <- c("cat", "percent")
colnames(SDvsGoverning) <- c("cat", "percent")

# FI vs current local coverning
FIvsMunType <- aggregate(kDat$PROCENT_FI , by=list(kDat$municipalityType), FUN=mean, na.rm=TRUE)
colnames(SDvsGoverning) <- c("cat", "percent")
write.xlsx(x = FIvsMunType, file = "output/FI-stöd vs SKL-typ.xlsx", sheetName = "data", row.names = TRUE)



# ============================================================================ #
# ANALYSIS: What is typical of the municipalities where party X grows?
# Make a subset to municipalities and see how they differ from the national average
# on various socio-economic measurments.
# ============================================================================ #

# Get numeric cols
metaCols <- c()
for (col in names(kDat)) {
  value <- kDat[[col]]
  if (is.numeric(value) && !grepl("MANDAT", col) && !grepl("ROSTER", col) && !grepl("PROCENT", col) && !grepl("VALDISTRIKT", col)) {
    metaCols <- c(metaCols, col)
  }
}

# Subset the municipalities where SD support > 20 %
selectedKommuner <- subset(kDat, PROCENT_SD > 20.0)
nrow(selectedKommuner)

# Subset municpalites where Moderaterna is the largest party
selectedKommuner <- subset(kDat, KV_largest_party == "M")

# Compare the means of the selected columns with the national avg.
df <- data.frame(column=metaCols)
df$nationalMean <- apply(df, 1, function(d) mean(kDat[[d[['column']]]]))
df$subsetMean <- apply(df, 1, function(d) mean(selectedKommuner[[d[['column']]]]))
df$diff <- (df$subsetMean / df$nationalMean - 1) * 100

# Make a chart (for our own analysis only this time)
ggplot(data=df, aes(x=column, y=diffMedian)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  theme(
    legend.position = "none", 
    plot.title = element_text(face="bold", size=16,vjust = 2), # Stil för rubbe
    strip.text.x = element_text(size = 14) 
  ) + 
  geom_text(aes(label=sprintf("%1.1f%%",diff)), position=position_dodge(width=0.9), vjust=0.5, size = 4) + # Etiketter för staplarna
  ggtitle("") +
  xlab("") + 
  ylab("Procent") 

# Function that draws a basic chart that compares the subseted municpalities to the country avg on a 
# specific measurment.
drawCountryComparisonChart <- function(chartDf, title, ylab) {
  chartDf <- data.frame(cat = c("SD:s toppkommuner","Hela landet"), value=c(chartDf$subsetMean,chartDf$nationalMean))
  
  p <- ggplot(data=chartDf, aes(x=cat, y=value, fill=factor(cat)))+
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("#666666", "#EDF25E")) +
    theme(
      legend.position = "none", # Göm legend
      plot.title = element_text(face="bold", size=20,vjust = 2), # Stil för rubbe
      strip.text.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 14)
    ) + 
    ggtitle(title) + # Sätt rubrik för hela grafen 
    xlab("") +
    ylab(ylab)  # Rubrik för y-axeln
  
  addLogo(p)
}
# Compare number of refugees in SD municpalites and national avg
chartDfRefugees <- df[df$column=="refugees",] 
drawCountryComparisonChart(chartDfRefugees, "SD:s toppkommuner tar emot färre flyktingar", "Mottagna flyktingar per 10 000 invånare")
# Compare unemployment in SD municpalites and national avg
chartDfUnemployment <- df[df$column=="unemployment2013",] 
drawCountryComparisonChart(chartDfUnemployment, "SD:s toppkommuner har något högre arbetslöshet", "Mottagna flyktingar per 10 000 invånare")

         
# ============================================================================ #
# ANALYSIS: How much do the support for a party differ in the local and national
# elections in the same municipality.
# ============================================================================ #
kDat$SD_KV_R_DIFF <- kDat$KV_PROCENT_SD - kDat$PROCENT_SD
nrow(kDat[kDat$SD_KV_R_DIFF > 0, ])
nrow(kDat[kDat$SD_KV_R_DIFF < 0, ])


# ============================================================================ #
# ANALYSIS: In what districts do the parties win/loose the most. What are
# their strongholds?
# ============================================================================ #
# The results will be exported to a Excel file
wb <- createWorkbook()
# Iterate parties
for (party in parties) {
  print(party)
  col <- paste(c("PROCENT_ANDRING", party), collapse="_")
  # Get their top districts (by growth)
  top <- dDat[order(dDat[[col]], decreasing = TRUE),][1:20,]
  sheet <- createSheet(wb, sheetName=party)
  
  # Transform data to a pretty data frame
  dfTop <- data.frame(
    'Distrikt' = top[['NAMN']],
    'Kommun' = top[['NAMN']], 
    'Förändring' = top[[col]])
  
  # Add sheet
  addDataFrame(dfTop, sheet)
}
# Save Excel file
saveWorkbook(wb, "output/partiernas toppdistrikt enligt tillväxt.xlsx")

# ============================================================================ #
# ANALYSIS: Correlations
# Same as for the municipalities above
# ============================================================================ #

# Get columns to correlate
correlationCols <- c()
categoryCols <- c()
for (col in names(kDat)) {
  value <- dDat[[col]]
  # Get numeric cols
  if (is.numeric(value) && !grepl("MANDAT", col) && !grepl("ROSTER", col)) {
    correlationCols <- union(correlationCols, col)
  }
  if (!is.numeric(value)) {
    categoryCols <- union(categoryCols, col) 
  }
}
# Filter the right cols
correlationData <- dDat[,correlationCols]
# Create a correlation matrix
correlationTable <- cor(correlationData)

# Write to Excel file to for further analysis
write.xlsx(x = correlationTable, file = "correlation-table-distrikt.xlsx", sheetName = "Corrlations", row.names = TRUE)  

# Draw a scatterplot: compare change for M with change for SD
drawScatterPlot(dDat,"PROCENT_ANDRING_M","PROCENT_ANDRING_SD","Förändring för M (%)","Ökning för SD (%)","SD växer i distrikt där M backar", highlight)



