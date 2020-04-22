#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
imbu_all <- data.table(read.csv2("imbu_list.csv", stringsAsFactors = F))
item_list <- data.table(read.csv2("item_list.csv", stringsAsFactors = F))

imbu_levels <- c("Basic", "Intricate", "Powerful")

imbu_all[category %in% c("crit", "paral", "cap", "speed"), category := "other"]
imbu_select_list <- split(x = imbu_all$name, imbu_all$category )

# do Obliczen
item_list <- merge(imbu_all, item_list, by.x = "var", by.y = "imbuement", all = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Imbuement price"),
    
    fluidRow(width = 12,
             tags$head( tags$style(HTML(" .selectize-input {  min-width: 150px; padding-top: 5px;} ")) ),
             fluidRow(
                 column(6,
                        tags$head(
                            tags$style(type="text/css", "label{ display: table-cell; text-align: left; vertical-align: middle; margin-top: 25px;} 
                .form-group { display: table-row;}")
                        ),
                        selectizeInput(
                            inputId = "s_imbu",
                            label = "Select imbuent:",
                            choices = imbu_select_list),
                        selectizeInput("s_level",
                                       label = "Select level:",
                                       choices = imbu_levels,
                                       selected = imbu_levels[length(imbu_levels)])
                 ),
                 h3("Gold Token Price:"),
                 fluidRow(column(2,numericInput(inputId = "price_token", label = "Price:", value = 30000, step = 1)),
                 )
             ),
             hr(),
             fluidRow(
                 column(12,
                        uiOutput(outputId = "ui_items")
                 )
             )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ui_items <- renderUI({
        lvl <- switch(input$s_level,
                      Basic=1,
                      Intricate=2,
                      Powerful=3)
        rows.selected <- item_list[name == input$s_imbu]
        
        # output$tot_tokens <- renderText({paste0("TOT: ", input$price_token * lvl * 2)})
        
        
        
        # Basic
        output$choice_1 <- renderText({paste0(input$price_1 * input$quant_1, "vs.", input$price_token * 1 * 2)})

        # output$choice_1_buy <- renderText({paste0(input$price_token_sell * 1 * 2)})
        # column(3, textOutput("tot_tokens")),
        
        fluidPage(
            tags$head( tags$style(HTML(" .numeric-input {  margin-top: 150px;} ")) ),
            h1(sprintf("Items for %s %s", input$s_level, input$s_imbu)),

            fluidRow(
                column(4,offset = 1, 
                       h3(rows.selected[stage == 1]$item),
                       numericInput(inputId = "quant_1", label = "Quantity", value = rows.selected[stage == 1]$number_of),
                       numericInput(inputId = "price_1", label = "Price:", value = 1000, step = 1)
                ),
                column(4, 
                       textOutput("choice_1")
                )
          
            ),
            hr(),
            conditionalPanel("input.s_level != 'Basic'",
                             fluidRow(
                                 h3(rows.selected[stage == 2]$item),
                                 numericInput(inputId = "quant_2", label = "Quantity:", value = rows.selected[stage == 2]$number_of),
                                 numericInput(inputId = "price_3_buy", label = "Buy Price:", value = 1000, width = "25%", step = 1)
                             )
            ),
            conditionalPanel("input.s_level == 'Powerful'",
                             fluidRow(
                                 h3(rows.selected[stage == 3]$item),
                                 numericInput(inputId = "quant_3", label = "Quantity:", value = rows.selected[stage == 3]$number_of, width = "25%", step = 1),
                                 numericInput(inputId = "price_3_buy", label = "Buy Price:", value = 1000, width = "25%", step = 1)

                             )
            ),
        )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
