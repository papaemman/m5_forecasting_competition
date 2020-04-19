#######################################
#                                     #
# Shiny app: M5 data exploration      #
#                                     #
#######################################

# Load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)

setwd("../..")
print(getwd())

# Import datasets
sales_aggregation_level_1 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_1_df.rds")
sales_aggregation_level_2 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_2.rds")
sales_aggregation_level_3 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_3.rds")
sales_aggregation_level_4 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_4.rds")
sales_aggregation_level_5 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_5.rds")
sales_aggregation_level_6 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_6.rds")
sales_aggregation_level_7 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_7.rds")
sales_aggregation_level_8 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_8.rds")
sales_aggregation_level_9 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_9.rds")
sales_aggregation_level_10 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_10.rds")
sales_aggregation_level_11 <- readRDS("data/processed/aggregated_time_series/sales_aggregation_level_11.rds")
sales_aggregation_level_12 <- fread("data/raw/sales_train_validation.csv")

# Calendar
calendar <- read.csv(file = "data/raw/calendar.csv", stringsAsFactors = F, na.strings = c(""))
calendar$date <- as.Date(calendar$date)


# Helper functions

plot_data <- function(ts_data, frc_val, frc_eval, calendar, input){
    
    ## Test
    
    # ts_data = sales_aggregation_level_8[1,]
    # frc_val = h8_frc_val[1,F1:F28]
    # frc_eval = h8_frc_eval[1,F1:F28]
    
    
    if(input$add_forecastings == 0){
        
        calendar <- calendar %>% filter(date < as.Date("2016-04-24"))
        
        # Keep title based on non-demand variables
        title <- ts_data %>% dplyr::select(-starts_with("d_")) %>% colnames()
        
        # Create data frame
        df <- data.frame(dates = calendar$date,
                         snap_CA = calendar$snap_CA,
                         snap_TX = calendar$snap_TX,
                         snap_WI = calendar$snap_WI,
                         event_type = calendar$event_type_1,
                         sales = ts_data %>% select(!!(calendar$d)) %>% as.numeric())
        
        # Make plots
        p <- ggplot(df, aes(x = dates, y = sales)) +
            geom_point(size = 0.5) + geom_line(size = 0.2, alpha = 0.5) +
            {if("SNAP_CA" %in% input$events) geom_point(data = df %>% filter(snap_CA != 0), aes(dates, sales), col = "pink", size = 0.3) } +
            {if("SNAP_TX" %in% input$events) geom_point(data = df %>% filter(snap_TX != 0), aes(dates, sales), col = "pink3", size = 0.3) } +
            {if("SNAP_WI" %in% input$events) geom_point(data = df %>% filter(snap_WI != 0), aes(dates, sales), col = "pink4", size = 0.3) } +
            {if("Events"%in% input$events) geom_point(data = df %>% filter(!is.na(event_type)), aes(dates, sales), col = "blue", size = 0.3) }+
            scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
            ggtitle(paste(title, collapse = " - "))
        
    
    } else { # else keep all calendar
        
        # Keep title based on non-demand variables
        title <- ts_data %>% dplyr::select(-starts_with("d_")) %>% colnames()
        
        # Create data frame
        df <- data.frame(dates = calendar$date,
                         snap_CA = calendar$snap_CA,
                         snap_TX = calendar$snap_TX,
                         snap_WI = calendar$snap_WI,
                         event_type = calendar$event_type_1,
                         sales = c(ts_data %>% select(starts_with("d_")) %>% as.numeric(), rep(NA, 28*2)),
                         frc_val = c(rep(NA,1913), as.numeric(frc_val[,F1:F28]), rep(NA,28)),
                         frc_eval = c(rep(NA, 1941), as.numeric(frc_eval[,F1:F28])))
        
        
        # Make plots
        p <- ggplot(df) +
            geom_line(aes(x = dates, y = sales), size = 0.2, alpha = 0.5) +
            geom_point(aes(x = dates, y = sales), size = 0.5) +
            
            geom_line(aes(x = dates, y = frc_val), size = 0.3, alpha = 0.7, col = "goldenrod") +
            geom_point(aes(x = dates, y = frc_val), size = 0.5, col = "goldenrod") +
            
            geom_line(aes(x = dates, y = frc_eval), size = 0.3, alpha = 0.7, col = "gold") +
            geom_point(aes(x = dates, y = frc_eval), size = 0.5, col = "gold") +
            
            {if("SNAP_CA" %in% input$events) geom_point(data = df %>% filter(snap_CA != 0), aes(dates, sales), col = "pink", size = 0.3) } +
            {if("SNAP_TX" %in% input$events) geom_point(data = df %>% filter(snap_TX != 0), aes(dates, sales), col = "pink3", size = 0.3) } +
            {if("SNAP_WI" %in% input$events) geom_point(data = df %>% filter(snap_WI != 0), aes(dates, sales), col = "pink4", size = 0.3) } +
            {if("Events"%in% input$events) geom_point(data = df %>% filter(!is.na(event_type)), aes(dates, sales), col = "blue", size = 0.3) }+
            scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
            ggtitle(paste(title, collapse = " - "))
        
    }
    

    p <- ggplotly(p)
    return(p)
}





## Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("M5 Forecasting data exploration"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            # Input: Select a submission file
            fileInput("file", "Choose a submission CSV File",
                      multiple = FALSE,
                      accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
            
            checkboxInput(inputId = "add_forecastings", label = "Show forecastings", value = 0),
            
            tags$hr(),
            
            selectInput(inputId = "hierarhy_lvl", label = "Select Hierarhy level",
                        choices = c("Total", "State", "Store", "Category", "Department", "State - Category", "State - Department",
                          "Store - Category", "Store - Department", "Product", "Product - State", "Product - Store"),
                        selected = "Total", multiple = F),
            
            dateRangeInput("daterange", "Define Date range",
                           start = "2011-01-29", end = "2016-04-24",
                           min = "2011-01-29", max = "2016-04-24"),
            
            checkboxGroupInput(inputId = "events", label = "Add events in plot",
                               choices = c("SNAP_CA", "SNAP_TX", "SNAP_WI", "Events"), selected = NULL),
            
            
            # hierarhy_lvl 1
            conditionalPanel(condition = "input.hierarhy_lvl == 'Total'",
                             
                            p("Total sales")
                    
                             ),
            
            # hierarhy_lvl 2
            conditionalPanel(condition = "input.hierarhy_lvl == 'State'",
                             
                             p("Total Sales per state"),
                             
                             radioButtons(inputId = "h_state", label = "Select state:",
                                          choices = c("CA", "TX", "WI"), selected = "CA")
                             
            ),
            
            # hierarhy_lvl 3
            conditionalPanel(condition = "input.hierarhy_lvl == 'Store'",
                             
                             p("Total sales per store"),
                             
                             radioButtons(inputId = "h_store", label = "Select store:",
                                          choices = c("CA_1","CA_2","CA_3","CA_4", "TX_1","TX_2", "TX_3", "WI_1", "WI_2", "WI_3"),
                                          selected = "CA_1")
                             
            ),
            
            # hierarhy_lvl 4
            conditionalPanel(condition = "input.hierarhy_lvl == 'Category'",
                             
                             p("Total sales per category"),
                             
                             radioButtons(inputId = "h_category", label = "Select Category:",
                                          choices = c("HOBBIES", "FOODS", "HOUSEHOLD"), selected = "FOODS")
                             
            ),
            
            # hierarhy_lvl 5
            conditionalPanel(condition = "input.hierarhy_lvl == 'Department'",
                             
                             p("Total sales per departments"),
                             
                             radioButtons(inputId = "h_department", label = "Select store:",
                                          choices = c("HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2", "FOODS_1", "FOODS_2","FOODS_3"),
                                          selected = "HOBBIES_1")
            ),
            
            
            # hierarhy_lvl 6
            conditionalPanel(condition = "input.hierarhy_lvl == 'State - Category'",
                             
                             p("Total sales per State and Category"),
                             
                             radioButtons(inputId = "h_state_category_st", label = "Select state:",
                                          choices = c("CA", "TX", "WI"),
                                          selected = "CA"),
                             
                             radioButtons(inputId = "h_state_category_cat", label = "Select category:",
                                          choices = c("HOBBIES", "FOODS", "HOUSEHOLD"),
                                          selected = "HOBBIES")
                             
            ),
            
            
            
            # hierarhy_lvl 7
            conditionalPanel(condition = "input.hierarhy_lvl == 'State - Department'",
                             
                             p("Total sales per State and Department"),
                             
                             radioButtons(inputId = "h_state_department_st", label = "Select state:",
                                          choices = c("CA", "TX", "WI"),
                                          selected = "CA"),
                             
                             radioButtons(inputId = "h_state_department_dep", label = "Select department:",
                                          choices = c("HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2", "FOODS_1", "FOODS_2","FOODS_3"),
                                          selected = "HOBBIES_1")
                             
            ),
            
            
            # hierarhy_lvl 8
            conditionalPanel(condition = "input.hierarhy_lvl == 'Store - Category'",
                             
                             p("Total sales per Store and Category"),
                             
                             radioButtons(inputId = "h_store_category_st", label = "Select store:",
                                          choices = c("CA_1","CA_2","CA_3","CA_4", "TX_1","TX_2", "TX_3", "WI_1", "WI_2", "WI_3"),
                                          selected = "CA_1"),
                             
                             radioButtons(inputId = "h_store_category_cat", label = "Select Category:",
                                          choices = c("HOBBIES", "FOODS", "HOUSEHOLD"),
                                          selected = "HOBBIES")
                             
            ),
            
            # hierarhy_lvl 9
            conditionalPanel(condition = "input.hierarhy_lvl == 'Store - Department'",
                             
                             p("Total sales per Store and Department"),
                             
                             radioButtons(inputId = "h_store_department_st", label = "Select store:",
                                          choices = c("CA_1","CA_2","CA_3","CA_4", "TX_1","TX_2", "TX_3", "WI_1", "WI_2", "WI_3"),
                                          selected = "CA_1"),
                             
                             radioButtons(inputId = "h_store_department_dep", label = "Select department:",
                                          choices = c("HOBBIES_1", "HOBBIES_2", "HOUSEHOLD_1", "HOUSEHOLD_2", "FOODS_1", "FOODS_2","FOODS_3"),
                                          selected = "HOBBIES_1")
                             
                             
            ),
            
            # hierarhy_lvl 10
            conditionalPanel(condition = "input.hierarhy_lvl == 'Product'",
                             
                             h3("Total sales per product:"),
                             
                             
                             tableOutput("product_codes_1"),
                             textInput(inputId = "h_product", label = "Type the product id", value = "FOODS_2_241")
                             
            ),
            
            
            # hierarhy_lvl 11
            conditionalPanel(condition = "input.hierarhy_lvl == 'Product - State'",
                             
                             h3("Total Sales per product and State"),
                             
                             
                             radioButtons(inputId = "h_product_state_st", label = "Select state:",
                                          choices = c("CA", "TX", "WI"),
                                          selected = "CA"),
                             
                             
                             tableOutput("product_codes_2"),
                             
                             textInput(inputId = "h_product_state_prod", label = "Type the product id", value = "FOODS_2_241")
                             
            ),
            
            # hierarhy_lvl 12
            conditionalPanel(condition = "input.hierarhy_lvl == 'Product - Store'",
                             
                             p("Total Sales per product and Store"),
                             
                             selectInput(inputId = "h_product_store_st", label = "Select store:",
                                          choices = c("CA_1","CA_2","CA_3","CA_4", "TX_1","TX_2", "TX_3", "WI_1", "WI_2", "WI_3"),
                                          selected = "CA_1"),
                             
                             tableOutput("product_codes_3"),
                             textInput(inputId = "h_product_store_prod", label = "Type the product id", value = "FOODS_2_241")
                             
            ),
            
            actionButton(inputId = "update_plot", label = "Update plot")
            
        ),
        

        # Show the plot 
        mainPanel(
           plotlyOutput("sales_plot")
        )
    )
)


# Define server logic required to draw plots

server <- function(input, output) {

    options(shiny.maxRequestSize=50*1024^2)  # 50 mb maximum size for uploaded dataset
    
    submission_file <- reactive({
            
        # Submission file
        inFile <- input$file
        
        if (is.null(inFile))
            return(NULL)
        
        submission <- read.csv(inFile$datapath, header = T, stringsAsFactors = F)
        # submission <- read.csv("data/pnt_submissions/sample_submission.csv", stringsAsFactors = F)
        
        submission$cat_id <- sapply(strsplit(submission$id, split = "_"), function(x){x[1]})
        submission$dept_id <- sapply(strsplit(submission$id, split = "_"), function(x){paste(x[1], x[2], sep = "_")})
        submission$item_id <- sapply(strsplit(submission$id, split = "_"), function(x){paste(x[1], x[2], x[3], sep = "_")})
        submission$state_id <- sapply(strsplit(submission$id, split = "_"), function(x){x[4]})
        submission$store_id <- sapply(strsplit(submission$id, split = "_"), function(x){paste(x[4], x[5], sep = "_")})
        submission$type <- sapply(strsplit(submission$id, split = "_"), function(x){x[6]})
        
        # head(submission)
        # table(submission$cat_id)
        # table(submission$store_id)
        # table(submission$state_id)
        # str(submission)
        
        submission <- as.data.table(submission)
        return(submission)
    })
    
    
    
    output$sales_plot <- renderPlotly({
        
        
        cols <- c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "F13", "F14",
                  "F15", "F16", "F17", "F18", "F19", "F20", "F21", "F22", "F23", "F24", "F25", "F26", "F27", "F28")
        
        
        frc <- submission_file()
        
        
        if(input$hierarhy_lvl == "Total"){# hierarhy_lvl 1
            
            if(input$add_forecastings){
                h1_frc_val <- frc[type=="validation", lapply(.SD, sum), .SDcols = cols]
                h1_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), .SDcols = cols]
            }
            
            
            p <- plot_data(ts_data = sales_aggregation_level_1, frc_val = h1_frc_val, frc_eval = h1_frc_eval, calendar = calendar, input)
            
            
        } else if(input$hierarhy_lvl == "State"){ # hierarhy_lvl 2
            
            ts_data <- sales_aggregation_level_2[sales_aggregation_level_2$state_id == input$h_state, ]
            
            if(input$add_forecastings){
                h2_frc_val <- frc[type=="validation", lapply(.SD, sum), by = state_id, .SDcols = cols]
                h2_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = state_id, .SDcols = cols]
            }

            p <- plot_data(ts_data = ts_data,
                           frc_val = h2_frc_val[state_id == input$h_state,],
                           frc_eval = h2_frc_eval[state_id == input$h_state,],
                           calendar = calendar, input)
            
            
        } else if(input$hierarhy_lvl == "Store"){ # hierarhy_lvl 3
            
            ts_data <- sales_aggregation_level_3[sales_aggregation_level_3$store_id == input$h_store, ]
            
            if(input$add_forecastings){
                h3_frc_val <- frc[type=="validation", lapply(.SD, sum), by = store_id, .SDcols = cols]
                h3_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = store_id, .SDcols = cols]
            }
            
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h3_frc_val[store_id == input$h_store,],
                           frc_eval = h3_frc_eval[store_id == input$h_store],
                           input)
            
            
        } else if(input$hierarhy_lvl == "Category"){ # hierarhy_lvl 4
     
            ts_data <- sales_aggregation_level_4[sales_aggregation_level_4$cat_id == input$h_category, ]
            
            if(input$add_forecastings){
                h4_frc_val <- frc[type=="validation", lapply(.SD, sum), by = cat_id, .SDcols = cols]
                h4_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = cat_id, .SDcols = cols]
            }
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h4_frc_val[cat_id==input$h_category,],
                           frc_eval = h4_frc_eval[cat_id == input$h_category, ],
                           input)
            
            
        } else if(input$hierarhy_lvl == "Department"){ # hierarhy_lvl 5
            
            ts_data <- sales_aggregation_level_5[sales_aggregation_level_5$dept_id == input$h_department, ]
            
            if(input$add_forecastings){
                h5_frc_val <- frc[type=="validation", lapply(.SD, sum), by = dept_id, .SDcols = cols]
                h5_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = dept_id, .SDcols = cols]
            }
            
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h5_frc_val[dept_id == input$h_department, ],
                           frc_eval = h5_frc_eval[dept_id == input$h_department, ],
                           input)

            
        } else if(input$hierarhy_lvl == "State - Category"){ # hierarhy_lvl 6
            
            ts_data <- sales_aggregation_level_6[sales_aggregation_level_6$state_id == input$h_state_category_st &
                                                     sales_aggregation_level_6$cat_id == input$h_state_category_cat, ]
            
            if(input$add_forecastings){
                h6_frc_val <- frc[type=="validation", lapply(.SD, sum), by = .(state_id, cat_id), .SDcols = cols]
                h6_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = .(state_id, cat_id), .SDcols = cols]
            }
            
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h6_frc_val[state_id == input$h_state_category_st & cat_id == input$h_state_category_cat, ],
                           frc_eval = h6_frc_eval[state_id == input$h_state_category_st & cat_id == input$h_state_category_cat, ],
                           input)
            
            
        } else if(input$hierarhy_lvl == "State - Department"){ # hierarhy_lvl 7
            
            ts_data <- sales_aggregation_level_7[sales_aggregation_level_7$state_id == input$h_state_department_st &
                                                     sales_aggregation_level_7$dept_id == input$h_state_department_dep, ]
            
            if(input$add_forecastings){
                h7_frc_val <- frc[type=="validation", lapply(.SD, sum), by = .(state_id, dept_id), .SDcols = cols]
                h7_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = .(state_id, dept_id), .SDcols = cols]
            }
            
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h7_frc_val[state_id == input$h_state_department_st & dept_id == input$h_state_department_dep, ], 
                           frc_eval =  h7_frc_eval[state_id == input$h_state_department_st & dept_id == input$h_state_department_dep,],
                           input)
            
            
        } else if (input$hierarhy_lvl == "Store - Category"){ # hierarhy_lvl 8
        
            ts_data <- sales_aggregation_level_8[sales_aggregation_level_8$store_id == input$h_store_category_st &
                                                     sales_aggregation_level_8$cat_id == input$h_store_category_cat, ]
            
            if(input$add_forecastings){
                h8_frc_val <- frc[type=="validation", lapply(.SD, sum), by = .(store_id, cat_id), .SDcols = cols]
                h8_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = .(store_id, cat_id), .SDcols = cols]
            }
           
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h8_frc_val[store_id == input$h_store_category_st & cat_id == input$h_store_category_cat,],
                           frc_eval = h8_frc_val[store_id == input$h_store_category_st & cat_id == input$h_store_category_cat,],
                           input)
            
            
        } else if(input$hierarhy_lvl == "Store - Department"){ # hierarhy_lvl 9
            
            ts_data <- sales_aggregation_level_9[sales_aggregation_level_9$store_id == input$h_store_department_st &
                                                     sales_aggregation_level_9$dept_id == input$h_store_department_dep, ]
            
            if(input$add_forecastings){
                h9_frc_val <- frc[type=="validation", lapply(.SD, sum), by = .(store_id, dept_id), .SDcols = cols]
                h9_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = .(store_id, dept_id), .SDcols = cols]
            }
            
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h9_frc_val[store_id == input$h_store_department_st & dept_id == input$h_store_department_dep,], 
                           frc_eval = h9_frc_eval[store_id == input$h_store_department_st & dept_id == input$h_store_department_dep,],
                           input)
            
            
        } else if(input$hierarhy_lvl == "Product"){ # hierarhy_lvl 10
            
            ts_data <- sales_aggregation_level_10[sales_aggregation_level_10$item_id == input$h_product, ]
            
            if(input$add_forecastings){
                h10_frc_val <- frc[type=="validation", lapply(.SD, sum), by = item_id, .SDcols = cols]
                h10_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = item_id, .SDcols = cols]
            }
            
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h10_frc_val[item_id == input$h_product, ],
                           frc_eval = h10_frc_eval[item_id == input$h_product,],
                           input)
            
            
        } else if(input$hierarhy_lvl == "Product - State"){ # hierarhy_lvl 11
            
            ts_data <- sales_aggregation_level_11[sales_aggregation_level_11$item_id == input$h_product_state_prod &
                                                      sales_aggregation_level_11$state_id == input$h_product_state_st, ]
            
            if(input$add_forecastings){
                h11_frc_val <- frc[type=="validation", lapply(.SD, sum), by = .(item_id, state_id), .SDcols = cols]
                h11_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = .(item_id, state_id), .SDcols = cols]
            }
           
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h11_frc_val[item_id == input$h_product_state_prod & state_id == input$h_product_state_st, ],
                           frc_eval = h11_frc_eval[item_id == input$h_product_state_prod & state_id == input$h_product_state_st, ],
                           input)
            
            
        } else if(input$hierarhy_lvl == "Product - Store"){ # hierarhy_lvl 12
            
            ts_data <- sales_aggregation_level_12[sales_aggregation_level_12$item_id == input$h_product_store_prod &
                                                      sales_aggregation_level_12$store_id == input$h_product_store_st, ]
            
            # h12_frc_val <- frc[type=="validation", lapply(.SD, sum), by = .(item_id, store_id), .SDcols = cols]
            # h12_frc_eval <- frc[type=="evaluation", lapply(.SD, sum), by = .(item_id, store_id), .SDcols = cols]
            
            if(input$add_forecastings){
                h12_frc_val <- frc[type == "validation",]
                h12_frc_eval <- frc[type == "evaluation",]
            }
            
            
            p <- plot_data(ts_data = ts_data, calendar = calendar,
                           frc_val = h12_frc_val[item_id == input$h_product_store_prod & store_id == input$h_product_store_st],
                           frc_eval = h12_frc_eval[item_id == input$h_product_store_prod & store_id == input$h_product_store_st],
                           input)
        }
        
        return(p)
    })
    
    
    output$product_codes_1 <- output$product_codes_2 <- output$product_codes_3 <- renderTable({
        
        data.frame(product_codes = c("HOBBIES_1_001 - HOBBIES_1_424",
                   "HOBBIES_2_001 - HOBBIES_2_149",
                   "HOUSEHOLD_1_001 - HOUSEHOLD_1_541",
                   "HOUSEHOLD_2_001 - HOUSEHOLD_2_516",
                   "FOODS_1_001 - FOODS_1_219",
                   "FOODS_2_001 - FOODS_2_399", 
                   "FOODS_3_001 - FOODS_3_827"))
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
