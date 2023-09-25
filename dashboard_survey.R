args <- commandArgs(TRUE)
print(paste0("file to be used: ", args[1]))
print("Loading libraries")
# load libraries -----
my_packages <- c("vroom", "shiny", "shinydashboard", "DT",
                 "purrr", "viridis",
                 "dplyr", "stringr", "ggplot2", "plotly")
suppressPackageStartupMessages(
    invisible(lapply(my_packages, library, character.only = TRUE)))
print("--- Libraries are loaded ---")

# read the dataset  -----
main_dataset <- vroom(args[1], show_col_types = FALSE)
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
num_groups <- main_dataset$country %>% unique() %>% length() %/%20
remai <- main_dataset$country %>% unique() %>% length() %%20
if (remai > 0) { num_groups <- num_groups + 1}

country_codes <- main_dataset %>%
    group_by(country) %>%
    group_keys() %>%
    group_by((row_number()-1) %/% (n()/num_groups)) %>%
    tidyr::nest() %>%
    pull(data) %>%
    bind_rows(.id = "country_table")

main_dataset <- main_dataset %>%
    left_join(country_codes,by = join_by(country) )

coloring <- unique(main_dataset$country_table) %>%
    length() %>%
    sample(LETTERS[1:8], ., replace = TRUE)

print("Preparing Shiny Dashboard: plots")
list_plots <- unique(main_dataset$country_table) %>%
    as.numeric() %>%
    sort() %>%
    purrr::map2(.y = coloring, ~main_dataset %>%
                   mutate(country = str_trunc(country, 30)) %>%
                   filter(country_table == .x) %>%
                   #ggplot(aes(country, fill = group))+
                   ggplot(aes(country, fill = country))+
                   geom_bar(position = "identity") +
                   ggtitle(label = str_c("Responses from part:", .x, " countries"))+
                   coord_flip()+
                   scale_fill_viridis(discrete = TRUE, option = .y)+
                   PhD_theme
    ) %>%
    purrr::map(~ggplotly(p=.x))

# UI etc -----
plotUI <- function(id) {
    ns <- NS(id)

    plotlyOutput(ns("plot"))
}

plotServer <- function(id, table_number) {
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
            text = "General Statistics",
            tabName = "gen_stat",
            icon = icon("earth-europe")),
        menuItem(
            text = "Table",
            tabName = "dt_stat",
            icon = icon("dashboard"))
    )
)

print("Preparing Shiny Dashboard: body")
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "gen_stat",
                h2("General dataset barplots"),
                fluidRow(
                    uiOutput("plotUI")
                    )),
        tabItem(tabName = "dt_stat",
                h2("Dataset Table"),
                fluidRow(
                    DT::dataTableOutput("rawtable"))
        )
    )
)

print("Preparing Shiny Dashboard: UI")
ui <- dashboardPage(
    dashboardHeader(title = "REMO Survey"),
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
                   }
            )
        )
    })

#    observeEvent(c(input$x, input$y), {
        plotServerList <- lapply(
            1:num_groups,
            function(i) {
                plotServer(paste0("plot", i), i)
            }
        )
#    })

    output$rawtable <- DT::renderDataTable((DT::datatable(main_dataset)))
}

print("Deploying Dashboard")
shinyApp(ui, server,
         options = list("port" = 3131, "host" = '0.0.0.0')
         )
print("Finished.")
