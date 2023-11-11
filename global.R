########################
#######  R-STAT  #######
########################

library(shiny)
library(bs4Dash)
library(plotly)
library(DT)
library(shinyWidgets)
library(data.table)     
library(readODS)      
library(ggplot2)        
library(ggdist)         
library(psych)         
library(tigerstats)     
library(lsr)           
library(Rmisc)         
library(nortest)        
library(emmeans)        
library(rstatix)        
library(afex)          
library(gmodels)       
library(car)            
library(GPArotation)    
library(yacca)          
library(MASS)           
library(candisc)        
library(dendextend)
library(irr)
library(ggdendro)
library(distributional)
library(fresh)
library(datamods)
library(waiter)
library(RSQLite)

# theme------------------
boja   <- "#4472C4"
tekst  <- "#9D9D9D"
pravac <- "#C0504D"

colors <-c("#4472C4","#c0504e","#9dbb61","#7f66a0","#4bafc7", 
           "#f19f53","#2C4D75","#772C2A","#5F7530","#4D3B62",
           "#276A7C","#B65708","#729ACA","#CD7371","#AFC97A",
           "#9983B5","#6FBDD1","#F9AB6B","#3A679C","#9F3B38",
           "#7E9D40","#664F83","#358EA6","#F3740B","#95B3D7",
           "#D99694","#C3D69B","#B3A2C7","#93CDDD","#FAC090",
           "#88E36F","#AE9CC4","#8DA7DB","#FF8585","#E9CE83"
)

thema <- 2

if (thema == 1) {
  # Theme 1
  mytheme <- create_theme(bs4dash_status(primary = "#007F5F", success = "#007F5F", info = "#272c30"))
  status <- "success"
  css <- "css/style1.css"
  logo <- "logo.png"
  home <- "home1.png"
  preloader <- "#007F5F"
  
  
} else if (thema == 2) {
  # Theme 2
  mytheme <- create_theme(bs4dash_status(primary = "#5E81AC", info = "#272c30"))
  status <- "primary"
  css <- "css/style2.css"
  logo <- "logo.png"
  home <- "home2.png"
  preloader <- "#5E81AC"

} else {
  # Theme 3
  mytheme <- create_theme(bs4dash_status(primary = "#BF616A", danger = "#BF616A", info = "#272c30"))
  status <- "danger"
  css   <- "css/style3.css"
  logo  <- "logo.png"
  home <- "home3.png"
  preloader <- "#BF616A"
}

# box------------
headerBorder <- F
solidHeader <- F
elevation <- 1

# table----------
style <- "bootstrap5"
class <- "compact"

# Database----------
db <- dbConnect(SQLite(), "KM-DATABASE.db")
file <- dbListTables(db)

