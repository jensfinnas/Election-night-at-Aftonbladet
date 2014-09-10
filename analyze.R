library(ggplot2)
library(gridExtra)
library(rjson)
library(xlsx)
library(png)
library(grid)

# Spara alla filterkartor i en lista
maps <- NULL
addMap <- function(mapItem) {
  if (is.null(maps)) {
    maps <- list(mapItem)
  }
  else {
    maps <- c(maps, list(mapItem))
  }
  return(maps)
}
writeMapsFile <- function() {
  write(toJSON(maps), file = "~/Sites/aftonbladet-valkompass-browser/valkarta/data/maps.json")  
}

# Spara data i variabler med lite smidigare namn
kDat <- kommunDB
dDat <- dat2010R$distrikt
# Rätta , > . i kolumnen valdeltagande
kDat$valdeltagande <- as.numeric(gsub(",",".", kDat$valdeltagande))
parties <- c("V", "S", "MP", "C", "FP", "M","KD", "SD")
partyColors <- c( "#AF0000", "#EE2020", "#83CF39", "#009933", "#6BB7EC", "#1B49DD", "#231977", "#DDDD00")
# FI:#D9308E

# I vilka kommuner får partierna mest och minst röster?
for (party in parties) {
  print(party)
  col <- paste(c("PROCENT", party), collapse="_")
  top10 <- kDat[order(kDat[[col]], decreasing = TRUE),][1:10,]
  bottom10 <- kDat[order(kDat[[col]], decreasing = FALSE),][1:10,]
  
  mapItemTop <- list(
    "title" = paste("De starkaste kommunerna för ", party, "."),
    "description" = "",
    "areas" = top10[['code']],
    "values" = top10[[col]]
  )
  maps <- addMap(mapItemTop)
  mapItemBottom <- list(
    "title" = paste("De svagaste kommunerna för ", party),
    "description" = "",
    "areas" = bottom10[['code']],
    "values" = bottom10[[col]]
  )
  maps <- addMap(mapItemBottom)
  
  print(data.frame(top10[['name']], top10[[col]]))
  print(data.frame(bottom10[['name']], bottom10[[col]]))
}

# Specifika frågor
mapItem <- list(
  "title" = "De kommuner där S ökade",
  "description" = "",
  "areas" = kDat[kDat$PROCENT_ANDRING_S > 0,]$code,
  "values" = kDat[kDat$PROCENT_ANDRING_S > 0,]$PROCENT_ANDRING_S  
  ) 
maps <- addMap(mapItem)

mapItem <- list(
  "title" = "De kommuner där MP ökade",
  "description" = "",
  "areas" = kDat[kDat$PROCENT_ANDRING_MP > 0,]$code,
  "values" = kDat[kDat$PROCENT_ANDRING_MP > 0,]$PROCENT_ANDRING_MP  
)
maps <- addMap(mapItem)

mapItem <- list(
  "title" = "De kommuner där MP INTE ökade",
  "description" = "",
  "areas" = kDat[kDat$PROCENT_ANDRING_MP < 0,]$code,
  "values" = kDat[kDat$PROCENT_ANDRING_MP < 0,]$PROCENT_ANDRING_MP  
)
maps <- addMap(mapItem)

# FI kom in
areas <- kDat[kDat$KV_MANDAT_FI > 0,]
mapItem <- list(
  "title" = "Kommunerna där FI fick mandat i kommunvalet",
  "description" = "",
  "areas" = areas$code,
  "values" = areas$KV_MANDAT_FI  
)
maps <- addMap(mapItem)

areas <- kDat[order(kDat$valdeltagande, decreasing = TRUE),][1:10,]
mapItem <- list(
  "title" = "Kommunerna med högst valdeltagande",
  "description" = "",
  "areas" = areas$code,
  "values" = areas$valdeltagande  
)
maps <- addMap(mapItem)

# Skapa kartfil
writeMapsFile()

# ============================================================================ #
# Om ___ fick bestämma? 
# Filtrera fram ett antal kommuner (t.ex. de med högst arbetslöshet), räkna ut
# vilket partier de röstar på i medeltal och rita ut en graf för fördelning.
# ============================================================================ #
# Funktion för att lägga till AB-logga
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
ifXGotToChoose <- function(col, n, title, topBottomLabels) {
  col <- 'reportedCrime'
  n <- 20
  # Sortera vår dataframe och filtrera de n första raderna
  subsetTop <- kDat[order(kDat[[col]], decreasing = TRUE),][1:n,]
  subsetBottom  <- kDat[order(kDat[[col]], decreasing = FALSE),][1:n,]
  print(subsetTop)
  # Loopa partierna och kolla hur många procents understöd de i snitt har i 
  # de valda kommunerna.
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
  # Skapa en data frame som blir underlag för graf
  cat <- c(rep(topBottomLabels[1], length(parties)), rep(topBottomLabels[2], length(parties)))
  df <- data.frame(party=factor(c(parties,parties), levels = parties , ordered = TRUE),votes=votes, cat=cat)    
  
  # Bygg grafen
  p <- ggplot(data=df, aes(x=party, y=votes, fill=party)) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values = partyColors) + # Färgsätt med partifärger
    facet_grid(. ~ cat) +
    theme(
      legend.position = "none", # Göm legend
      plot.title = element_text(face="bold", size=16,vjust = 2), # Stil för rubbe
      strip.text.x = element_text(size = 14) 
    ) + 
    geom_text(aes(label=sprintf("%1.1f%%",votes)), position=position_dodge(width=0.9), vjust=-0.5, size = 4) + # Etiketter för staplarna
    ggtitle(title) + # Sätt rubrik för hela grafen
    xlab("") + # Ingen rubrik för x-axeln
    ylab("Procent")  # Rubrik för y-axeln
    
  
  # Printa vilka kommuner det handlar om
  print(topBottomLabels[1])
  print(subsetTop$name)
  print(topBottomLabels[2])
  print(subsetBottom$name)
  #g <- arrangeGrob(p, sub = textGrob(paste(subset$name, collapse = ", "), x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontsize = 10)))
  # Lägg till logo
#  addLogo(p)
  return(p)
} 
ifXGotToChoose('foreignBorn', 20, "Så här röstar kommunerna \n där andelen utlandsfödda är...", c("...högst.", "...lägst."))
ifXGotToChoose('medianIncome', 20, "Så här röstar kommunerna \n där medianinkomsten är...", c("...högst.", "...lägst."))
ifXGotToChoose('unemploymentChange', 20, "Så här röstar kommunerna \n där arbetslösheten de senaste åren...", c("...växt kraftigast.", "...minskat mest"))




# ============================================================================ #
# Korrelationsjakt: Skapa en korrelationsmatris för att se om det finns samband
# som till exempel kan förklara ett partis upp- eller nedgång. 
# ============================================================================ #
# Skapa en lista över de kolumner som vi vill korrelera mot varandra
correlationCols <- c()
# Spara alla kategorikolumner i en 
categoryCols <- c()
for (col in names(kDat)) {
  value <- kDat[[col]]
  # Ta endast med kolumner med numeriska värden, xxkludera mandat och antal röster
  if (is.numeric(value) && !grepl("MANDAT", col) && !grepl("ROSTER", col)) {
    correlationCols <- union(correlationCols, col)
  }
  if (!is.numeric(value)) {
    categoryCols <- union(categoryCols, col) 
  }
}
# Filtrera de utvalda kolumnerna
correlationData <- kDat[,correlationCols]
# Skapa en korrelationsmatris
correlationTable <- cor(correlationData)
# Skriv matrisen till en Excel-fil
write.xlsx(x = correlationTable, file = "correlation-table.xlsx", sheetName = "Corrlations", row.names = TRUE)  


# Funktion för att rita scatterplots
drawScatterPlot <- function(x, y, xLabel, yLabel, title, highlight) {
  # Filtrera fram de kommuner som ska få etiketter
  correlationData$name <- apply(kDat, 1, FUN = function(d) { if (d[['name']] %in% highlight) d[['name']] else "" })
  correlationData$highlighted <- apply(kDat, 1, FUN = function(d) { if (d[['name']] %in% highlight) "0" else "1"})
  
  # Skapa grafen
  p <- ggplot(correlationData, aes_string(x=x, y=y, color="highlighted")) +
    geom_point(shape=19, alpha=1/2) + # Stil för punkterna
    geom_text(aes(label=name), size=4, vjust=-0.7) + # Stil för punkternas etiketter
    scale_color_manual(values = c("black","bisque4")) + # Färger för punkterna
    theme(
      legend.position = "none", # Göm legenden
      axis.title.x = element_text(color="black", size=14), # Stil för x-axelns rubbe
      axis.title.y = element_text(color="black", size=14), # Stil för y-axelns rubbe
      plot.title = element_text(face="bold", size=16,vjust = 2) # Stil för huvudrubbe
    ) +
    ggtitle(title) + # Rubrik för hela grafen
    xlab(xLabel) + # Rubrik för x-axeln
    ylab(yLabel) # Rubrik för y-axeln 
  
  # Lägg till logga
  addLogo(p)
}

# Exempelanvändning på scatterplot-ritaren
highlight <- kDat[kDat$unemployment2013 > 13,]$name
x <- "urbanDegree"
y <- "PROCENT_C"
xLabel <- "Kommunens tätortsgrad"
yLabel <- "Andel som röstar C"
title <- "Många röstar C på landet"
drawScatterPlot(x,y,xLabel,yLabel,title, highlight)


# ============================================================================ #
# Vad utmärker de kommuner där XXXXX?
# ============================================================================ #
metaCols <- c()
for (col in names(kDat)) {
  value <- kDat[[col]]
  # Ta endast med kolumner med numeriska värden, xxkludera mandat och antal röster
  if (is.numeric(value) && !grepl("MANDAT", col) && !grepl("ROSTER", col) && !grepl("PROCENT", col) && !grepl("VALDISTRIKT", col)) {
    metaCols <- c(metaCols, col)
  }
}
subset <- kDat[kDat$PROCENT_SD > 10,]
df <- data.frame(column=metaCols)
df$nationalMean <- apply(df, 1, function(d) mean(kDat[[d[['column']]]]))
df$subsetMean <- apply(df, 1, function(d) mean(subset[[d[['column']]]]))
df$diff <- (df$subsetMean / df$nationalMean - 1) * 100
df <- within(df, 
       diff <- factor(diff, 
                          levels=names(sort(table(diff), 
                                            decreasing=TRUE))))

ggplot(data=df, aes(x=column, y=diff)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  theme(
    legend.position = "none", # Göm legend
    plot.title = element_text(face="bold", size=16,vjust = 2), # Stil för rubbe
    strip.text.x = element_text(size = 14) 
  ) + 
  geom_text(aes(label=sprintf("%1.1f%%",diff)), position=position_dodge(width=0.9), vjust=0.5, size = 4) + # Etiketter för staplarna
  ggtitle("") + # Sätt rubrik för hela grafen
  xlab("") + # Ingen rubrik för x-axeln
  ylab("Procent")  # Rubrik för y-axeln

# ============================================================================ #
#    VALDISTRIKT
# ============================================================================ #
# I vilka valdistrikt ökar partierna mest?
# Skapa en ny workbook för Excel
wb <- createWorkbook()
# Loopa alla partier 
for (party in parties) {
  print(party)
  col <- paste(c("PROCENT_ANDRING", party), collapse="_")
  # Sortera efter förändring och hämta de n största distrikten
  top <- dDat[order(dDat[[col]], decreasing = TRUE),][1:20,]
  sheet <- createSheet(wb, sheetName=party)
  
  # Skapa en data frame som underlag för det vi till slut skriver till Excel-filen
  dfTop <- data.frame(
    Distrikt = top[['NAMN']],
    Kommun = top[['NAMN']], 
    'Förändring' = top[[col]])

  addDataFrame(dfTop, sheet)
}
# Spara Excel-filen
saveWorkbook(wb, "output/partiernas toppdistrikt enligt tillväxt.xlsx")

# Korrelationer
correlationCols <- c()
# Spara alla kategorikolumner i en 
categoryCols <- c()
for (col in names(kDat)) {
  value <- dDat[[col]]
  # Ta endast med kolumner med numeriska värden, xxkludera mandat och antal röster
  if (is.numeric(value) && !grepl("MANDAT", col) && !grepl("ROSTER", col)) {
    correlationCols <- union(correlationCols, col)
  }
  if (!is.numeric(value)) {
    categoryCols <- union(categoryCols, col) 
  }
}
# Filtrera de utvalda kolumnerna
correlationData <- dDat[,correlationCols]
# Skapa en korrelationsmatris
correlationTable <- cor(correlationData)
# Skriv matrisen till en Excel-fil
write.xlsx(x = correlationTable, file = "correlation-table-distrikt.xlsx", sheetName = "Corrlations", row.names = TRUE)  

drawScatterPlot()

ggplot(correlationData, aes(x=PROCENT_ANDRING_SD, y=PROCENT_ANDRING_M)) +
  geom_point(shape=19, alpha=1/2)

