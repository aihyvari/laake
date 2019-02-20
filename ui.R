library(ggvis)

# alasvetovalikko
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

fluidPage(
  titlePanel("LAAKETUTKAIN"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
#             sliderInput("jotain", "Minimi jotain....",
#                        10, 300, 80, step = 10),
             sliderInput("vuosi", "Kaupaantulovuosi", 1995, 2019, value = c(1995, 2019), sep = ""),
             sliderInput("EUlupa", "Kaikki / vain EU lupa", 0, 1, 0, step = 1),
             sliderInput("ALVILLINENHINTA", "Kuluttajahinta",
                         0, 800, c(0, 800), step = 1),
#             selectInput("ATC", "ATC koodi paaluokka",
#                         c("Kaikki yht.", "A", "B", "C", "D", "G","H", "J", "L", "M", "N", "P",
#                           "R", "S", "V")
#             ),
             textInput("valmiste", "Tuotenimi (sisaltaa)"),
             textInput("vaine", "Vaikuttava aine (sisaltaa)")
           ),
           wellPanel(
             selectInput("xvar", "X-akseli", axis_vars, selected = "vuosi"),
             selectInput("yvar", "Y-akseli", axis_vars, selected = "ALVILLINENHINTA"),
             tags$small(paste0("HUOM: data pakkauskohtainen:  eri pakkauskoot & vahvuudet = eri tuote"))
           )
    ),
    column(9,
           ggvisOutput("plot1"),
           wellPanel(
             span("Kuinka monta valittu?",
                  textOutput("n_laakkeet"))),
           wellPanel(
             span("Taulukko",
                 DT:: dataTableOutput("taulukko")))
    )
  )
)