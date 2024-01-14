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
#main_dataset <- vroom("../staircase_response_count_parsed_2023-11-08.csv", del=",", show_col_types = false)
main_dataset <- vroom(args[1], del=",", show_col_types = false)
main_dataset <- main_dataset %>% 
    mutate(
        country = case_when(
            is.na(country) ~ "not selected",
            str_detect(country, "unknown") ~ "unknown",
            str_detect(country, "other") ~  "other",
            true ~ country), 
        institution = case_when(
            is.na(institution) ~ "i prefer not to say",
            true ~ institution)
    )
print("dataset was imported")
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
calc_n_plots <- function(x, dataset){
    num_groups <- dataset %>% pull(x) %>% unique() %>% length() %/% 20
    remai <- dataset %>% pull(x) %>% unique() %>% length() %% 20
    if (remai > 0) { num_groups <- num_groups + 1}
    return(num_groups)
}

country_groups <- calc_n_plots("country", main_dataset)
#num_groups <- main_dataset$country %>% unique() %>% length() %/% 20
#remai <- main_dataset$country %>% unique() %>% length() %% 20
#if (remai > 0) { num_groups <- num_groups + 1}

max_inst_count <- main_dataset %>% 
    count(country , institution) %>% 
    count(country, sort = T) %>%
    filter (n > 17) %>%
    pull(country)

country_codes <- main_dataset %>%
    group_by(country) %>%
    group_keys() %>%
    group_by((row_number()-1) %/% (n()/ country_groups)) %>%
    tidyr::nest() %>%
    pull(data) %>%
    bind_rows(.id = "country_table")

ints_codes <- purrr::map(max_inst_count, ~main_dataset %>%
        filter(country == .x) %>%
        count(institution, sort = TRUE) %>%
        group_by((row_number()-1) %/% (n()/calc_n_plots("institution", 
        filter(main_dataset, country == .x)))) %>%
#    main_dataset %>%
#    filter(country == .x) %>%
#    group_by(institution) %>%
#    group_keys() %>%
#    group_by((row_number()-1) %/% (n()/calc_n_plots("institution", 
#        filter(main_dataset, country == .x)))) %>%
    mutate(country = .x) %>%
    tidyr::nest() %>%
    pull(data) %>%
    bind_rows(.id = "country_in_table") %>%
    mutate(country_r = str_c(country,"__v", country_in_table)) %>%
    dplyr::select(institution,country, country_r )# %>%
    #group_by(country_r) %>% 
    #group_split()
) %>% bind_rows()

inst_countries_names  <- ints_codes$country_r %>% unique()
#names(ints_codes) <- max_inst_count 

plot_multiple <- function(dataset, Country){
    max_inst_count <- filter(dataset, country == Country) %>% 
        count(institution) %>% 
        pull(n) %>% 
        max()
    grp_insts <- filter(dataset, country == Country) %>% pull(country_r) %>% unique()
    grp_insts %>% 
        set_names() %>%
        purrr::imap(~filter(dataset, country_r == .x) %>%
                    mutate(institution = str_trunc(institution , 35)) %>%
                    ggplot(aes(reorder(institution, country,
                      function(x) length(x)), fill = institution))+
                    geom_bar(position = "identity") +
                    xlab("Institution")+ 
                    ggtitle(label = "Responses by institution")+
                    coord_flip()+
                    ylim(0, max_inst_count + 10 ) +
                    PhD_theme
        ) %>%
        purrr::map(~ggplotly(p=.x))
}

main_dataset <- main_dataset %>%
    left_join(country_codes, by = join_by(country)) %>%
    left_join(ints_codes, by = join_by(country,institution))

max_country_count <- main_dataset %>% count(country) %>% pull(n) %>% max()

coloring <- unique(main_dataset$country_table) %>%
    length() %>%
    sample(LETTERS[1:8], ., replace = TRUE)

print("Preparing Shiny Dashboard: plots")
list_plots_country <- unique(main_dataset$country_table) %>%
    as.numeric() %>%
    sort() %>%
    purrr::map2(.y = coloring, ~main_dataset %>%
                   mutate(country = str_trunc(country, 25)) %>%
                   filter(country_table == .x) %>%
                   ggplot(aes(reorder(country, country,
                      function(x) length(x)), fill = country))+
                   geom_bar(position = "identity") +
                   xlab("Country")+ 
                   ggtitle(label = if_else(.x == 1,
                                   "Responses by countries",
                                   "Responses by countries continued"))+
                   coord_flip()+
                   ylim(0, max_country_count + 10 ) +
                   scale_fill_viridis(discrete = TRUE, option = .y)+
                   PhD_theme
    ) %>%
    purrr::map(~ggplotly(p=.x))
  
list_plots_inst_1 <- country_codes$country %>%
    str_subset(max_inst_count %>% str_c(collapse ="|"), negate = T) %>%
    purrr::map(~main_dataset %>%
                   filter(country == .x) %>%
                   mutate(institution = str_trunc(institution , 35)) %>%
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

names(list_plots_inst_1) <- country_codes$country %>%
    str_subset(max_inst_count %>% str_c(collapse ="|"), negate = T)

list_plots_inst_2 <- map(max_inst_count, 
    ~plot_multiple(main_dataset, .x)
)

list_plots_inst <- c(list_plots_inst_1, unlist(list_plots_inst_2, recursive =FALSE))

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
                    uiOutput("plot_countries")
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
                    uiOutput("plot_institutions")
                )
        )
    )
)

print("Preparing Shiny Dashboard: UI")
ui <- tagList(dashboardPage(
    dashboardHeader(title = "REMO Survey Results"),
    sidebar = sidebar,
    body = body),
    #end dashboardPage position:absolute;z-index: 1000;
    tags$footer(
        HTML("<span style='font-size:18px;'>
            2023-2024,&nbsp&nbsp&nbsp 
            Licence:&nbsp&nbsp <i class='fa-regular fa-copyright'></i>
            <a href='https://github.com/ConYel/dashboard_survey/blob/main/LICENSE'>
            &nbsp AGPL-3.0 &nbsp</a>.
            &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp 
            Made with  &nbsp<i class='fa-solid fa-heart'></i> &nbsp by:&nbsp
            Konstantinos Geles &nbsp&nbsp
            <a href='https://github.com/ConYel'><i class='fa-brands fa-github'></i></a>
            </span><br>"), 
              align = "center", style = "
              text-align:center; 
              bottom:0;
              right:100;
              width:100%;
              height:30px;   /* Height of the footer */
              color: white;
              padding: 2px;
              background-color: #063838;")

)#end tagList

print("Preparing Shiny Dashboard: Server")
server <- function(input, output, session) {
    # end the session when browser is closed
    # will be removed if it is on server
    #session$onSessionEnded(function() {
    #   stopApp()
    #})
    output$plot_countries <- renderUI({
        ns <- session$ns
        tagList(
            lapply(1:country_groups,
                   function(i) {
                       plotUI(paste0("plot", i))
                   }
            )
        )
    })
    
#    observeEvent(c(input$x, input$y), {
    plotServerList <- lapply(
       1:country_groups,
         function(i) {
             plotServer(paste0("plot", i), i, list_plots_country )
         }
    )
#    })
    # plot server side for institutions 
    plot_server <- lapply(
       names(list_plots_inst),
         function(i) {
             plotServer(paste0("plot", i), i, list_plots_inst )
         }
    )

    state <- reactiveValues()

    observe({
       req(input$select_country)
       state$x <- input$select_country
       if(state$x %in% max_inst_count){
            state$y <- str_subset(names(list_plots_inst), state$x)
       } else {state$y  <- state$x} 
       #     #plot_multiple(main_dataset, ints_codes, state$x),
       #     list_plots_inst[[state$x]])
    
    output$plot_institutions <- renderUI({
        ns <- session$ns
        tagList(
            lapply(state$y,
                   function(i) {
                       plotUI(paste0("plot", i))
                   }
            )
        )
    })
    })
}

print("Deploying Dashboard")
shinyApp(ui, server,
        # options = list("port" = 3838, "host" = '0.0.0.0')
        )
print("Finished.")
