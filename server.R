#based on movie explorer app
#https://shiny.rstudio.com/gallery/movie-explorer.html
library(ggvis)
library(dplyr)
library(openxlsx)
library(DescTools)
library(DT)

# Datan noutaminen verkosta
#HILA
hila_osoite<-"http://www.hila.fi/c/document_library/get_file?folderId=2470305&name=DLFE-10810.xlsx"
hila<-read.xlsx(hila_osoite, sheet=1, detectDates = TRUE)


#FIMEA perusrekisteri
fimea<-"https://www.fimea.fi/documents/160140/1128399/pakkaus1.txt/1fdad9d7-7933-4cb5-b459-fed2eb25b32d"
temp=tempfile(fileext=".txt")
download.file(fimea, destfile=temp)
perusrek<-read.table(temp, header=TRUE, sep=";")
#perusrek<-perusrek[,c(2,5:6,9:12,14,19:21,29:31,38)]
##########################################################
data<-merge(perusrek, hila, by.x="VNRNRO", by.y="VNRO")

data$vuosi<-substring(data$KAUPPAANTULOPVM,1,4)
data$vuosi<-as.numeric(paste(data$vuosi))
data$EUlupa<-0
data$EUlupa<-ifelse(data$EUMYYNTILUPANRO != "", 1, 0)
data$ATCpaa<-substring(data$ATCKOODI,1,1)

####################################################
shinyServer(function(input, output, session) {
  
# Datan filtterointi
  laakkeet <- reactive({
    # tarvitaan temp variablet inputeille
    EULUPA <- input$EUlupa
    kaupan <- input$KAUPAN
    minvuosi <- input$vuosi[1]
    maxvuosi <- input$vuosi[2]
    minvahhinta <- input$ALVILLINENHINTA[1] 
    maxvahhinta <- input$ALVILLINENHINTA[2] 
    
    # Apply filters
    m <- data %>%
      filter(
        EUlupa >= EULUPA,
       # KAUPAN == kaupan,
        vuosi >= minvuosi,
        vuosi <= maxvuosi,
        ALVILLINENHINTA >= minvahhinta,
        ALVILLINENHINTA <= maxvahhinta
      ) %>%
      arrange(vuosi)
    
    #filter by valmistenimi
    if (!is.null(input$vaine) && input$valmiste != "") {
      valmiste <- paste0("%", input$valmiste, "%")
      m <- m %>% filter(VALMISTE %like% valmiste)
    }
    #filter by vaikuttava aine
    if (!is.null(input$vaine) && input$vaine != "") {
      vaine <- paste0("%", input$vaine, "%")
      m <- m %>% filter(VAIKUTTAVA.AINE %like% vaine)
    }
# filter by ....
#   if (!is.null(input$cast) && input$cast != "") {
#    cast <- paste0("%", input$cast, "%")
#   m <- m %>% filter(Cast %like% cast)
#}

m <- as.data.frame(m)
    
    # lisataan sarake kaupan/ ei kaupa
    # 
    m$on_kaupan <- character(nrow(m))
    m$on_kaupan[m$KAUPAN == 0] <- "EI"
    m$on_kaupan[m$KAUPAN >= 1] <- "KYLLA"
    m
  })
  
  # Function for generating tooltip text
  laake_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$VNRNRO)) return(NULL)
    
    # poimi tietty ID
    data2 <- isolate(laakkeet())
    laake <- data2[data2$VNRNRO == x$VNRNRO, ]
    
    paste0("<b>", laake$VALMISTE, "</b><br>",
          laake$vuosi, "<br>",
           format(laake$ALVILLINENHINTA, big.mark = ",", scientific = FALSE)
    )
  }

  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normaalisti props(x = ~BoxOffice, y = ~Reviews),
    #  inputs are strings
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    laakkeet %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~on_kaupan, key := ~VNRNRO) %>%
      add_tooltip(laake_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", title = "Edelleen kaupan", values = c("KYLLA", "EI")) %>%
      scale_nominal("stroke", domain = c("KYLLA", "EI"),
                    range = c("orange", "#aaa")) %>%
      set_options(width = 900, height = 700)
########################    

##################################################
  })
  
  vis %>% bind_shiny("plot1")
  
  output$n_laakkeet <- renderText({ nrow(laakkeet()) })
  #output$ATCjakauma<- renderText({taulukko()})
  output$taulukko<-DT::renderDataTable({data})
#  
})
