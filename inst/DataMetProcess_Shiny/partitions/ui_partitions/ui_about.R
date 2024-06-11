ui_about <- function(id){
  tagList(
    fluidRow(
      tags$section(
        id="logo_about",
        imageOutput("logoabout")
      ),
      tags$section(
        id = "text_about",
        tags$h3("About"),
        tags$p("The DataMetProcess application is a shiny tool designed for basic and fundamental meteorological data processing based on the DataMetProcess package (link). Although it was developed using INMET data, it was programmed to be easily applicable to other databases."),
        tags$h3("Authors"),
        tags$p("
               Wagner Martins dos Santos,
               Edimir Xavier Leal Ferraz,
               Lady Daiane Costa de Sousa Martins
               "),
        tags$h3("Citation"),
        tags$p("
               Santos, W. M. et al.(2016). DataMetProcess: A package for processing meteorological data, calculating reference evapotranspiration, acquiring data from the National Institute of Meteorology (INMET) and processing meteorological data....
               "),
        tags$h3("Contacts"),
        tags$p("
               wagnnerms97@gmail.com
               "),
        tags$h3("Help"),
        tags$a(
          "Visit github for help on how to use the application",
          href = "https://github.com/wagnnerms97/DataMetProcess-Global/tree/main/DataMetProcess_Shiny",
          target="_blank"
        ),
        tags$h3("Acknowledgements"),
        tags$section(
          class="logos_ack",
          tags$a(
            href = "https://www.ufrpe.br/",
            imageOutput("ufrpe",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "http://www.uast.ufrpe.br/br",
            imageOutput("uast",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "http://www.pgea.ufrpe.br/",
            imageOutput("pgea",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "https://www.gov.br/capes/",
            imageOutput("capes",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "https://www.gov.br/cnpq/pt-br",
            imageOutput("cnpq",height = "70px"),
            target="_blank"
          ),
          tags$a(
            href = "https://www.facepe.br/",
            imageOutput("facepe",height = "70px"),
            target="_blank"
          )
        )
      )
    )#fluidRow
  )
}
