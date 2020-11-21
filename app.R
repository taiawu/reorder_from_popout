
library(sortable)
library(shiny)

wrap_by <- function(.wrap_by) {
    facet_wrap(vars(!!wrap_by), labeller = label_both)
}

test_plot <- function(df, .wrap_by, .lines_by) {
    wrap_by  <- enquo(.wrap_by)
    lines_by <- enquo(.lines_by)
    df %>%
        ggplot(aes(x = Temperature, y = value_norm, group = well, linetype = !!lines_by)) +
        geom_line() +
        facet_wrap(vars(!!wrap_by))
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("update", "Update"),
            uiOutput("order_var_ui"),
            uiOutput("order_var_ui2")
                # rank_list(
                #     input_id = "list1",
                #     text = "You can select multiple items and drag as a group:",
                #     labels = c("one", "two", "three", "four", "five"),
                #   
                #     options = sortable_options(
                #         multiDrag = TRUE
                #     )
                # )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("order1"),
           verbatimTextOutput("refactor_list_text")
          # verbatimTextOutput("structure1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    raw <- reactive({readRDS("input_data/sample_dsf_data.rds")  %>%
                        filter(well %in% c("A1", "A2"))})
    layout <- reactive({readRDS("input_data/sample_layout_file.rds") %>% 
                        filter(well %in% c("A1", "A2")) %>% 
                        mutate(protein = c("protein", "buffer"))})

    selected_var1 <- reactiveVal("dye")
    selected_var2 <- reactiveVal("protein")
    
    output$order_var_ui <- renderUI({
        req(selected_var1())
        req(layout())
        
        rank_list(
            input_id = "list1",
            text = "You can select multiple items and drag as a group:",
            labels = layout()[[selected_var1()]] %>% unique(),

            options = sortable_options(
                multiDrag = TRUE
            )
        )
    })
    
    output$order_var_ui2 <- renderUI({
        req(selected_var2())
        req(layout())
        
        rank_list(
            input_id = "list2",
            text = "You can select multiple items and drag as a group:",
            labels = layout()[[selected_var2()]] %>% unique(),
            
            options = sortable_options(
                multiDrag = TRUE
            )
        )
    })
    
    values <- reactiveValues()
 
    observe({
        input$list1 # have to call here because they're isolated below
        input$list2
        values$refactor_list[[selected_var1()]] <- isolate(input$list1)
        values$refactor_list[[selected_var2()]] <- isolate(input$list2)
        
    })
    
    output$refactor_list_text<-renderPrint({
        values$refactor_list
    })
    
  
    output$order1 <- renderText({ input$list1 })
}

# Run the application 
shinyApp(ui = ui, server = server)
