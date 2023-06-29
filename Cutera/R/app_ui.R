#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import dplyr
#' @noRd
oem_data <- read.csv("data/oemDataSearch1.csv") %>%
  filter(Mfg == "Cutera")
# theming options
cutera_theme <- bs_theme(version = 5,
                       bg = "white",
                       fg = "#1f0900",
                       base_font = font_google("Montserrat"),
                       secondary = "#7AC66E")

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # Your application UI logic
    page_fluid(
      theme = cutera_theme,
      card(card_header(class = "bg-dark",
        inverse = T,
                       fluidRow(
                         column(6,
                                align = 'left',
                                a(href = "https://cutera.com/",
                                  img(src = "https://cutera.com/wp-content/themes/cutera-corp/images/footer-logo.svg",
                                      width = "256px"))
                         ),
                         column(6, align= 'right',
                                h5(
                                  a(style = {
                                    "color: #7AC66E;"},
                                    "Contact Cutera",
                                    href = "https://cutera.com/customer-support/"))

                         )))
           ,fluidRow(column(12,align='center',
                            h2(strong("Search eye protection by selecting a laser device"))))
      ),
      fluidRow(
        column(
          6,
          align = 'center',
          selectInput(
            inputId = "mod",
            label = h4(strong("Model")),
            choices = sort(unique(oem_data$`Mod`)),
            selected = NULL
          )
        ),
        column(
          6,
          align = "center",
          br(),
          actionButton("run",
                       icon = icon("magnifying-glass"),
                       style='padding-left:50px;padding-right:50px;padding-top:1px;padding-bottom:1px; font-size:80%',
                       h5(strong("Search")),
                       class = "btn-secondary"))),
      br(),
      conditionalPanel(
        condition = "input.run",
        fluidRow(column(12, align = "center",
                        h3(
                          tableOutput("userInfo")))
        ),
        card(
          fluidRow(column(12,
                          align = "center",
                          h3(
                            em("Compatible Innovative Optics Products")),
                          h1(
                            htmlOutput("linksOEM")),
                          htmlOutput("imagesOEM"),
                          htmlOutput("graphsOEM"),
                          htmlOutput("tablesOEM"),
                          h6("Click the link or the image above to select your favorite frame color"),
                          h6("Add the item to your cart and check-out or return to the app to find additional safety products"),
                          h6("The items you add to your cart will remain there until you are finished searching")
          ))
        ),
        card(
          fluidRow(column(12,
                          align = 'center',
                          h3(
                            em("Frequently Purchased Together")))),
          fluidRow(
            column(6, align = 'center',
                   a(target = "_blank",
                     href = "https://innovativeoptics.com/product/laser-vee-shield-disposable-patient-protection/",
                     img(src = "https://innovativeoptics.com/wp-content/uploads/2022/04/Laser-vee-shield.new-copy.jpg",
                         width = "300px")),
                   h4("Disposable Laser Protection for Patients")),
            column(6, align = 'center',
                   a(target = "_blank",
                     href = "https://innovativeoptics.com/product/ipl-vee-shield-disposable-patient-eye-protection/",
                     img(src = "https://innovativeoptics.com/wp-content/uploads/2022/04/IPL-flex-foam.website.jpg",
                         width = "300px")),
                   h4("Disposable IPL Protection for Patients"))),
          fluidRow(
            column(6, align = 'center',
                   a(target = "_blank",
                     href = "https://innovativeoptics.com/product/ptceiii-blue-adjustable-patient-goggle/",
                     img(src = "https://innovativeoptics.com/wp-content/uploads/2019/07/PTCEIII-Blue-NEW.jpg",
                         width = "300px")),
                   h4("Reusable Laser Protection for Patients"),
                   h4("w/ Independent Eyecup Motion")),
            column(6, align = 'center',
                   a(target = "_blank",
                     href = "https://innovativeoptics.com/product/bbce/",
                     img(src = "https://innovativeoptics.com/wp-content/uploads/2021/04/BBCE-white-silicone-straps.jpg",
                         width = "300px")),
                   h4("Reusable Laser Protection for Patients"),
                   h4("w/o Independent Eyecup Motion"))
          )
        )),
      card(card_footer(class = "bg-dark",
        h5(
        style = {
          "color: white;
                         text-shadow: 1px 1px 1px white;"
        },
        "Powered by Innovative Optics")))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Cutera"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
