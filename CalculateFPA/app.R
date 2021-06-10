
library("shiny")
library("DT")

calcFPA <- function(x, type = "Bed breakfast", fpa.prop = 0.13){
    
    if(type == "Bed breakfast"){
        prop <- 0.95
    }
    
    if(type == "Half board"){
        prop <- 0.85
    }
    
    a <- round(x*prop, digits = 2)
    b <- x - a
    
    vec <- c(a, round(a/(1 + fpa.prop), digits = 2), a - round(a/(1 + fpa.prop), digits = 2), 
             b, round(b/(1 + fpa.prop), digits = 2), b - round(b/(1 + fpa.prop), digits = 2))
    
    nam <- c(
        paste0(prop*100, "% price"), 
        paste0("Excluding ", fpa.prop*100, "% FPA"),
        paste0("FPA ", fpa.prop*100, "%"),
        paste0(100-prop*100, "% price"), 
        paste0("Excluding ", fpa.prop*100, "% FPA"),
        paste0("FPA ", fpa.prop*100, "%")
    )
    
    data <- data.frame("Variable" = nam, "Price" = vec)
    return(data)
}




ui <- fluidPage(
    
    # App title ----
    titlePanel("FPA calculator"),
    
    # Sidebar layout with input and output definitions ----
    
    # Sidebar panel for inputs ----
    selectInput(
        inputId = "fpafunction",
        label = "Select type of accommodation:",
        choices = c("Bed breakfast", "Half board")
    ),
    
    numericInput(
        inputId = "fpaprop",
        label = "Select FPA %:",
        value = 0.13, 
        min = 0, 
        max = 1, 
        step = 0.1
    ), 
    
    numericInput(
        inputId = "price",
        label = "Select price:",
        value = 150, 
        min = 0, 
        max = NA, 
        step = NA
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
        tabsetPanel(
            id = 'dataset',
            tabPanel("Results", DT::dataTableOutput("tbl"))
        )
        
    )
)




server <- function(input, output) {
    
    output$tbl <- 
        DT::renderDataTable({DT::datatable(calcFPA(x = as.numeric(input$price), 
                                                  type = input$fpafunction, 
                                                  fpa.prop = as.numeric(input$fpaprop))
        ) %>% 
            formatRound(columns = c(2), digits = 2)
        
        })
    
}

shinyApp(ui, server)




