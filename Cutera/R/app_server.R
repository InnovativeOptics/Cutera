#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import purrr
#' @noRd
our_data <- readxl::read_excel("data/Master_2.xlsx",
                               sheet = "Lens_details") %>%
  mutate(VLT = scales::percent(as.numeric(VLT)),
         `Price(from)` = scales::dollar(as.numeric(`Price(from)`)))
oem_data <- read.csv("data/oemDataSearch1.csv") %>%
  filter(Mfg == "Cutera")
app_server <- function(input, output, session) {
  # Your application server logic
  selected_data_oem <- eventReactive(input$mod,{
    req(input$mod)
    oem_data %>%
      filter(`Mod` == input$mod)
  })
  selected_data <- eventReactive(input$mod,{
    req(input$mod)
    map(unique(selected_data_oem()$Lens), ~tibble(filter(our_data, Lens == .x)))
  })
  userInfoTable <- eventReactive(input$mod,{
    select(selected_data_oem(), `Eyewear.Requirement`) %>%
      unique() %>%
      mutate("Selected Laser Specifications (nm)" = Eyewear.Requirement, .keep = "none") %>%
      head(1) %>%
      tibble()
  })
  output$userInfo <- renderTable(striped = F,
                                 hover = T,
                                 bordered = T,
                                 spacing = c("s"),
                                 width = "auto",
                                 align = "c",
                                 {
                                   userInfoTable()
                                 })
  output$tablesOEM <- renderUI({
    req(selected_data())
    map(1:length(selected_data()), ~renderTable(bordered = T,
                                                align = "c",
                                                striped=T,
                                                hover = F,
                                                width = "100%",
                                                colnames = T, na = "-",{
                                                  selected_data()[[.x]] %>%
                                                    select(c("Lens", "OD", "CE", "VLT","Material","Price(from)"))
                                                })
    )
  })
  output$linksOEM <- renderUI({
    req(selected_data())
    map(1:length(selected_data()), ~HTML(
      c('<a href="',
        selected_data()[[.x]]$Website,
        '", target = "_blank", style = color:#ff4714, title = "', selected_data()[[.x]]$Lens,'frame styles">',
        selected_data()[[.x]]$Lens,'</a>'

      ))
    )
  })
  output$imagesOEM <- renderUI({
    req(selected_data())
    map(1:length(selected_data()), ~HTML(
      c('<a href="',
        selected_data()[[.x]]$Website,
        '", target = "_blank", title = "', selected_data()[[.x]]$Lens,'frame styles">
        <img src="',
        selected_data()[[.x]]$Image,
        '", height = 65em></a>')
    ))

  })
  output$graphsOEM <- renderUI({
    req(selected_data())
    map(1:length(selected_data()), ~HTML(
      c('<a href="',
        selected_data()[[.x]]$Website,
        '", target = "_blank", title = "', selected_data()[[.x]]$Lens,'frame styles">
        <img src="',
        selected_data()[[.x]]$Graph,
        '", height = 225px></a>')
    ))

  })
}
