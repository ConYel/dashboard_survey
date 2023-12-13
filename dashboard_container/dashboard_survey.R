args <- commandArgs(TRUE)
print(paste0("file to be used: ", args[1]))
print("Loading libraries")
# load libraries -----
# to_install_for_dboard <- c("bit64","tidyselect","mime","digest","xtable",
# "ellipsis", "crayon","lifecycle")
my_packages <- c("vroom", "shiny", "shinydashboard", "DT",
                 "purrr", "viridis",
                 "dplyr", "stringr", "ggplot2", "plotly")
suppressPackageStartupMessages(
    invisible(lapply(my_packages, library, character.only = TRUE)))
print("--- Libraries are loaded ---")

# read the dataset  -----
#main_dataset <- vroom("../staircase_response_count_parsed_2023-11-08.csv", del=",", show_col_types = FALSE)
main_dataset <- vroom(args[1], del=",", show_col_types = FALSE)
main_dataset <- main_dataset %>% 
    mutate(
        country = case_when(
            is.na(country) ~ "Not selected",
            str_detect(country, "unknown") ~ "Unknown",
            str_detect(country, "other") ~  "Other",
            TRUE ~ country
    ), 
        institution = case_when(
            is.na(institution) ~ "I prefer not to say",
            TRUE ~ institution
        ))
print("Dataset was imported")
#print(head(main_dataset))

# theme for the plots -----
PhD_theme <-
    list(
        theme_bw() +
            theme(
                panel.border = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(size = 12, colour = "black"),
                axis.line = element_line(),
                panel.grid.major = element_line(linewidth = 0.2),
                panel.grid.minor = element_line(linewidth = 0.1),
                text = element_text(size = 12),
                legend.position="none",
                axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10),
                                            colour = "black"),
                axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10),
                                            colour = "black"),
                axis.text.x = element_text(hjust = 1, vjust = 1, size = 12),
                axis.text.y = element_text(size = 12, colour = "black"),
                plot.title = element_text(hjust = 0.5, colour = "black")
            )
    )

# checking how many countries exit to plot per plot -----
print("Calculate how many countries per plot")
num_groups <- main_dataset$country %>% unique() %>% length() %/% 20
remai <- main_dataset$country %>% unique() %>% length() %% 20
if (remai > 0) { num_groups <- num_groups + 1}

country_codes <- main_dataset %>%
    group_by(country) %>%
    group_keys() %>%
    group_by((row_number()-1) %/% (n()/num_groups)) %>%
    tidyr::nest() %>%
    pull(data) %>%
    bind_rows(.id = "country_table")

main_dataset <- main_dataset %>%
    left_join(country_codes, by = join_by(country))

coloring <- unique(main_dataset$country_table) %>%
    length() %>%
    sample(LETTERS[1:8], ., replace = TRUE)

print("Preparing Shiny Dashboard: plots")
list_plots_country <- unique(main_dataset$country_table) %>%
    as.numeric() %>%
    sort() %>%
    purrr::map2(.y = coloring, ~main_dataset %>%
                   mutate(country = str_trunc(country, 22)) %>%
                   filter(country_table == .x) %>%
                   ggplot(aes(reorder(country, country,
                      function(x) length(x)), fill = country))+
                   geom_bar(position = "identity") +
                   xlab("Country")+ 
                   ggtitle(label = if_else(.x == 1,
                                   "Responses by countries",
                                   "Responses by countries continued"))+
                   coord_flip()+
                   scale_fill_viridis(discrete = TRUE, option = .y)+
                   PhD_theme
    ) %>%
    purrr::map(~ggplotly(p=.x))


list_plots_inst <- country_codes$country %>%
    purrr::map(~main_dataset %>%
                   filter(country == .x) %>%
    ggplot(aes(reorder(institution, country,
                      function(x) length(x)), fill = institution))+
                   geom_bar(position = "identity") +
                   xlab("Institution") +
                   ggtitle(label = "Responses by institution") +
                   coord_flip() +
                   scale_fill_viridis(discrete = TRUE) +
                   PhD_theme
    ) %>%
    purrr::map(~ggplotly(p=.x))

names(list_plots_inst) <- country_codes$country

# Modules
## UI etc -----
plotUI <- function(id) {
    ns <- NS(id)
    plotlyOutput(ns("plot"))
}

plotServer <- function(id, table_number, list_plots) {
    moduleServer(
        id,
        function(input, output, session) {
            output$plot <- renderPlotly({
                req(table_number)
                list_plots[[table_number]]
            })
        }
    )
}


print("Preparing Shiny Dashboard: sidebar")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            text = "Country Statistics",
            tabName = "gen_stat",
            icon = icon("earth-europe")),
        menuItem(
            text = "Institution Statistics",
            tabName = "dt_stat",
            icon = icon("chart-line"))
    )
)

print("Preparing Shiny Dashboard: body")
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "gen_stat",
                h2("General dataset per country barplots"),
                fluidRow(
                    uiOutput("plotUI")
                    )),
        tabItem(tabName = "dt_stat",
                h2("Dataset per institution barplots"),
                fluidRow(
                column(8,
                    sidebarPanel(
                        selectInput(
                            inputId = "select_country",
                            label = "Select country to plot answers from institutions",
                            choices = country_codes$country,
                            selected = country_codes$country[1],
                            multiple = FALSE
                        )    
                    )
                )
                ),
                fluidRow(
                    plotlyOutput("plotcountry")
                )
        )
    )
)

print("Preparing Shiny Dashboard: UI")
ui <- dashboardPage(
    dashboardHeader(title = "REMO Survey Results"),
    sidebar = sidebar,
    body = body
)

print("Preparing Shiny Dashboard: Server")
server <- function(input, output, session) {
    # end the session when browser is closed
    # will be removed if it is on server
    #session$onSessionEnded(function() {
    #   stopApp()
    #})

    output$plotUI <- renderUI({
        ns <- session$ns
        tagList(
            lapply(1:num_groups,
                   function(i) {
                       plotUI(paste0("plot", i))
                   })
            
        )
    })
    
#    observeEvent(c(input$x, input$y), {
    plotServerList <- lapply(
       1:num_groups,
         function(i) {
             plotServer(paste0("plot", i), i, list_plots_country )
         }
    )
#    })
    output$plotcountry <- renderPlotly({
                req(input$select_country)
                list_plots_inst[[input$select_country]]
            })

}

print("Deploying Dashboard")
shinyApp(ui, server,
        # options = list("port" = 3838, "host" = '0.0.0.0')
         )
print("Finished.")
