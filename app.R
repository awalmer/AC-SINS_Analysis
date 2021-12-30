#------------------------------------------------------------------------------------------------------
# AC-SINS R Shiny App
# Auralee Walmer
# Application Script
#------------------------------------------------------------------------------------------------------

# Set-Up
library(shiny)
library(dplyr)
library(xlsx)
options(DT.options = list(pageLength = 384)) #default setting for DT tables

source("./functions.R") #source

# User Interface:
ui <- fluidPage(
  titlePanel("AC-SINS Data Analysis"),
  tags$script(HTML(
    "document.body.style.backgroundColor = '#D4ECEF';"
  )),
  sidebarLayout(
    sidebarPanel(
      fileInput("acsins_file",h4("Upload Raw AC-SINS Data"),multiple=TRUE,accept=c(".xlsx",".XLS",".xls",".xlsm", ".xltx", ".csv")),
      fileInput("acsins_names",h4("Upload Sample Names"),multiple=TRUE,accept=c(".xlsx",".XLS",".xls",".xlsm", ".xltx", ".csv")),
      helpText("Please upload the raw AC-SINS data in Excel format."),
      tags$hr(),
      h4("Manually Exclude Wells"),
      helpText("Highlight the rows of the Summary Table that you would like to exclude from analysis. (Removes only numerical results; table rows remain.)"),
      uiOutput("rows_summary_selected"),
      actionButton("exclude", "Apply Exclusion"),
      actionButton("refresh", "Refresh Data"), 
      tags$hr(),
      uiOutput("plot_select"),
      helpText("Select the data column to view the corresponding Loess Model. Visit the 'Individual Plot' tab."),
      tags$hr(),
      uiOutput("multiple_1"),
      uiOutput("multiple_2"),
      uiOutput("multiple_3"),
      uiOutput("multiple_4"),
      uiOutput("multiple_5"),
      uiOutput("multiple_6"),
      uiOutput("multiple_7"),
      uiOutput("multiple_8"),
      helpText("Initial default data column is A1. The multi-curve plot will only display unique selections."),
      tags$hr(),
      h4("Download Reports"),
      helpText("Highlight the rows of interest in the normal 96-well table to generate abridged report."),
      uiOutput("rows_selected_2"),
      downloadButton("report_norm_full", "Full Report"),
      downloadButton("report_norm", "Abridged Report"),
      helpText("Highlight the rows of interest in the optimized 96-well table to generate abridged optimized report."),
      uiOutput("rows_selected"),
      downloadButton("report_full", "Full Optimized Report"),
      downloadButton("report", "Abridged Optimized Report")
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Raw Data",
                           h4("Raw Data"),
                           tags$hr(),
                           uiOutput("datetime"),
                           tags$hr(),
                           tableOutput("cleandata")
                  ),
                  tabPanel("Summary (384-Well)", 
                           h4("Summary Table (384-Well)"),
                           helpText("Immediately below is a table containing the Average Buffer Plasmon Wavelength value for each Buffer Name group. Below that is the summary table containing the variables of interested for each data column (well), excluding those with blank sample names."),
                           helpText("The 'Delta_PWL' column represents the change/shift in Plasmon Wavelength from the Average Buffer value for the corresponding Buffer Name group (first table)."),
                           uiOutput("avg_buffer"),
                           helpText("Average Buffer PW not showing up? Make sure you've labeled your Blanks/Buffers as 'Buffer' in your sample name data! "),
                           tags$hr(),
                           DTOutput("summary"),
                           downloadButton("download_summary", "Download")
                  ),
                  tabPanel("96 Well Table (Avg PW)",
                           h4("96 Well Table"),
                           h5("(Average Plasmon Wavelength of 4 Readings)"),
                           helpText("This table condenses the 384 readings into the 96-well format; that is, one row for each sample."),
                           uiOutput("controls"),
                           helpText("For the control name table to appear correctly, there should be a unique control name for each Buffer Name group."),
                           uiOutput("avg_buffer_96"),
                           tags$hr(),
                           DTOutput("averages"),
                           downloadButton("download_96", "Download")
                  ),
                  tabPanel("Optimized 96 Well Table", 
                           h4("96 Well Table - Optimized"),
                           h5("Reactive Table to Exclude Outliers"),
                           helpText("Adjust the slider to select the reactive threshold. Plasmon wavelenghth values that are a distance of this number of standard deviations from the mean or greater will be excluded. The average and standard deviation will be recalculated each time the slider is moved."),
                           uiOutput("controls_2"),
                           helpText("For the control name table to appear correctly, there should be a unique control name for each Buffer Name group."),
                           uiOutput("avg_buffer_96_opt"),
                           h4("Exclusion Threshold"),
                           h5("(Number of SDs away from Avg PW)"),
                           sliderInput("threshold", "", min = 0, max = 4,
                                       value = 1, step = 0.25),
                           DTOutput("optimized"),
                           downloadButton("download_96_opt", "Download")
                  ),
                  tabPanel("Individual Plot", plotOutput("plot",width = "100%", height = "600px")),
                  tabPanel("Plot Grid: Curve Fit", 
                           h4("Top Eight Plots of Concern"),
                           h5("Based on 'Percent in Range' Parameter"),
                           h5("'Percent in Range' = A measurement of how well the Loess curve fits the data given."),
                           plotOutput("plot_grid",width = "100%", height = "1700px")
                  ),
                  tabPanel("Plot Grid: RMSE", 
                           h4("Top Eight Plots of Concern"),
                           h5("Based on 'Root Mean Squared Error' Parameter"),
                           h5("'Root Mean Squared Error (RMSE)' = A measurement of how noisy the given data is."),
                           plotOutput("plot_grid_scatter",width = "100%", height = "1700px")
                  ),
                  tabPanel("Multi-Curve Plot",
                           h4("Loess Model Comparison"),
                           h5("(Side Panel Selection)"),
                           checkboxInput("checkbox","Include Vertical Lines?"),
                           tags$hr(),
                           plotOutput("multiple_loess",width = "100%", height = "700px")
                  ),
                  tabPanel("Goodness of Fit Parameters", 
                           h4("Goodness of Fit Measurement"),
                           h5("Percent in Range:"),
                           helpText("Taking the Root Mean Squared Error (below) of the Loess Model and doubling it, we assess what proportion of points in the plot fall within that distance from the Loess curve. This number reflects how reliable the Loess model is for a given plot. 
                                    This method functions as a psuedo-confidence-interval assessment. If it is a good curve fit, 90% of the data points should fall within 2 RMSE measurements."),
                           h5("RMSE (Root Mean Squared Error):"),
                           helpText("The RMSE gives us a measure of the noisiness of the data - that is, how much the error (distance from data point to Loess curve) varies.
                                    The formula for RMSE is the SQUARE ROOT of the following: the sum of (loess value - raw value) squared, divided by the total number of data points."),
                           #fluidRow(column(4, actionButton('sort_percent','Sort by Percent in Range')), column(4, actionButton('sort_scatter','Sort by Root Mean Squared Error'))),
                           DTOutput("guide"),
                           downloadButton("download_gfit", "Download")
                  ),
                  tabPanel("Benchling",
                           h4("Download Benchling Result File"),
                           uiOutput("benchling"),
                           br(),
                           br()
                  )
      )
    )
  )
)


# Server:
server <- function(input, output, session) {
  
  ##-------- Reactive Values --------##
  
  #Reactive values: (cleaning raw data)
  ## Have one reactive data frame as input for following functions
  
  names_dataframe_reactive <- reactive({
    req(input$acsins_names)
    if (see_if(has_extension(input$acsins_names$datapath, 'csv')) == TRUE) {
      return(read_modify_acsins_csv(input$acsins_names$datapath))
    } else {
      return(read_excel(input$acsins_names$datapath))
    }
  })
  
  cleanse_data_reactive <- reactive({clean_acsins_excel_new(input$acsins_file$datapath)})
  cleanse_data_reactive_num <- reactive({clean_acsins_excel_num(input$acsins_file$datapath, for_display = 0)}) # Numeric for calculation
  cleanse_data_reactive_display <- reactive({clean_acsins_excel_num(input$acsins_file$datapath, for_display = 1)}) # Numeric for display
  date_time_reactive <- reactive({extract_date_time(input$acsins_file$datapath)})
  date_time_iso_reactive <- reactive({ convert_date_format_for_API(extract_date_time(input$acsins_file$datapath)) })
  cleanse_names_384_reactive <- reactive({isolate_names_384(names_dataframe_reactive())})
  cleanse_names_96_reactive <- reactive({convert_names_384_to_96(names_dataframe_reactive())})
  
  #Special reactive name list (so that goodness of fit table maintains functionality even when sample names not uploaded):
  cleanse_names_384_gfit_reactive <- reactive({
    if(is.null(input$acsins_names)){
      return(build_blank_names_gfit())
    } else {
      return(cleanse_names_384_reactive()[c('Well','Sample Name')])
    }
  })
  
  ##Reactive error message:
  neednames <- reactive({
    validate(need(input$acsins_names != "", "Please upload the sample name data."))
    get(input$acsins_names, 'package:datasets')
  })
  
  ##Reactive error message:
  needrawdata <- reactive({
    validate(need(input$acsins_file, message = "Please upload the raw AC-SINS data."))
    get(input$acsins_file, 'package:datasets', inherits = FALSE)
  })
  
  #Reactive Sample Names: (blank if not uploaded)
  cleanse_names_reactive <- reactive({
    if(is.null(input$acsins_names)){
      return(build_blank_names())
    } else {
      return(capture_fullname_df_384(input$acsins_names$datapath)['Sample Name'])
    }
  })
  
  #Reactive name indicator for plots
  nameindicator_reactive <- reactive({
    if(is.null(input$acsins_names)){
      return(0)
    } else {
      return(1)
    }
  })
  
  
  #Render Date/Time in Output:
  output$datetime <- renderText({
    if(is.null(input$acsins_file)){
      return(NULL)
    } else {
      return(paste0("Acquisition Date & Time: ", date_time_reactive()))
    }
  })
  
  
  
  ##-------- Drop-Down Menus --------##
  
  ### NOTE: if tried using cleanse data reactive while name data blank did not work. Had to add an additional "else" in this condition set
  output$plot_select <- renderUI({ if(is.null(input$acsins_file)){
    return(NULL) } else  if(is.null(input$acsins_names)) { selectInput("plot_select", h4("Select Individual Plot"), extract_column_list(cleanse_data_reactive()) )} 
    else { selectInput("plot_select", h4("Select for Individual Plot"), paste(abridge_well(cleanse_names_384_reactive()), abridge_names(cleanse_names_384_reactive()), abridge_buffers(cleanse_names_384_reactive()), sep=' ') )}
  })
  
  output$multiple_1 <- renderUI({ if(is.null(input$acsins_file)){
    return(NULL) } else if(is.null(input$acsins_names)) { selectInput("multiple_1", "Select Column", extract_column_list(cleanse_data_reactive()))} 
    else {selectInput("multiple_1", h4("Select for Multi-Curve Plot"), paste(abridge_well(cleanse_names_384_reactive()), abridge_names(cleanse_names_384_reactive()), abridge_buffers(cleanse_names_384_reactive()), sep=' ') )}
  })
  
  output$multiple_2 <- renderUI ({ if(is.null(input$acsins_file)){
    return(NULL) } else  if(is.null(input$acsins_names)) { selectInput("multiple_2", "",  append("No_Plot", extract_column_list(cleanse_data_reactive()) ))} 
    else {selectInput("multiple_2", "", append("No_Plot", paste(abridge_well(cleanse_names_384_reactive()), abridge_names(cleanse_names_384_reactive()), abridge_buffers(cleanse_names_384_reactive()), sep=' ')) )}
  })
  
  output$multiple_3 <- renderUI ({ if(is.null(input$acsins_file)){
    return(NULL) } else  if(is.null(input$acsins_names)) { selectInput("multiple_3", "",  append("No_Plot", extract_column_list(cleanse_data_reactive()) ))} 
    else {selectInput("multiple_3", "", append("No_Plot", paste(abridge_well(cleanse_names_384_reactive()), abridge_names(cleanse_names_384_reactive()), abridge_buffers(cleanse_names_384_reactive()), sep=' ')) )}
  })
  
  output$multiple_4 <- renderUI ({ if(is.null(input$acsins_file)){
    return(NULL) } else  if(is.null(input$acsins_names)) { selectInput("multiple_4", "",  append("No_Plot", extract_column_list(cleanse_data_reactive()) ))} 
    else {selectInput("multiple_4", "", append("No_Plot", paste(abridge_well(cleanse_names_384_reactive()), abridge_names(cleanse_names_384_reactive()), abridge_buffers(cleanse_names_384_reactive()), sep=' ')) )}
  })
  
  output$multiple_5 <- renderUI ({ if(is.null(input$acsins_file)){
    return(NULL) } else  if(is.null(input$acsins_names)) { selectInput("multiple_5", "",  append("No_Plot", extract_column_list(cleanse_data_reactive()) ))} 
    else {selectInput("multiple_5", "", append("No_Plot", paste(abridge_well(cleanse_names_384_reactive()), abridge_names(cleanse_names_384_reactive()), abridge_buffers(cleanse_names_384_reactive()), sep=' ')) )}
  })
  
  output$multiple_6 <- renderUI ({ if(is.null(input$acsins_file)){
    return(NULL) } else  if(is.null(input$acsins_names)) { selectInput("multiple_6", "",  append("No_Plot", extract_column_list(cleanse_data_reactive()) ))} 
    else {selectInput("multiple_6", "", append("No_Plot", paste(abridge_well(cleanse_names_384_reactive()), abridge_names(cleanse_names_384_reactive()), abridge_buffers(cleanse_names_384_reactive()), sep=' ')) )}
  })
  
  output$multiple_7 <- renderUI ({ if(is.null(input$acsins_file)){
    return(NULL) } else  if(is.null(input$acsins_names)) { selectInput("multiple_7", "",  append("No_Plot", extract_column_list(cleanse_data_reactive()) ))} 
    else {selectInput("multiple_7", "", append("No_Plot", paste(abridge_well(cleanse_names_384_reactive()), abridge_names(cleanse_names_384_reactive()), abridge_buffers(cleanse_names_384_reactive()), sep=' ')) )}
  })
  
  output$multiple_8 <- renderUI ({ if(is.null(input$acsins_file)){
    return(NULL) } else  if(is.null(input$acsins_names)) { selectInput("multiple_8", "",  append("No_Plot", extract_column_list(cleanse_data_reactive()) ))} 
    else {selectInput("multiple_8", "", append("No_Plot", paste(abridge_well(cleanse_names_384_reactive()), abridge_names(cleanse_names_384_reactive()), abridge_buffers(cleanse_names_384_reactive()), sep=' ')) )}
  })
  
  
  ##-------- Tab Outputs --------##
  
  #Raw Data:
  output$cleandata <- renderTable({
    if(is.null(input$acsins_file)){
      return(NULL)
    } else {
      return(cleanse_data_reactive_display())
    }
  })
  
  #Render Average Buffer Table above Summary Table:
  output$avg_buffer <- renderTable({
    if(is.null(input$acsins_names)){
      return(NULL)
    } else {
      return(calc_meanbuffers_table(build_summary_table(cleanse_data_reactive(), cleanse_names_384_reactive())))
    }
  })
  output$avg_buffer_96 <- renderTable({
    if(is.null(input$acsins_names)){
      return(NULL)
    } else {
      return(calc_meanbuffers_table(build_summary_table(cleanse_data_reactive(), cleanse_names_384_reactive())))
    }
  })
  output$avg_buffer_96_opt <- renderTable({
    if(is.null(input$acsins_names)){
      return(NULL)
    } else {
      return(calc_meanbuffers_table(build_summary_table(cleanse_data_reactive(), cleanse_names_384_reactive())))
    }
  })
  
  ##Summary 384-Well:
  output$summary <- DT::renderDT({
    if(is.null(input$acsins_file)){
      return(NULL)
    } else if(is.null(input$acsins_names)){
      return(NULL)
    } else {
      datatable(build_summary_table(cleanse_data_reactive(), cleanse_names_384_reactive()), rownames= FALSE, 
                filter = list(position = 'top'),
                options = list(columnDefs = list(list(className = 'dt-center', targets = 0:10)), search = list(regex = TRUE), searchCols = list(NULL)) )
    }
  })
  
  # Static Control Name Table
  output$controls <- renderTable({
    if(is.null(input$acsins_names)){
      return(NULL)
    } else {
      return(create_control_table(cleanse_data_reactive_num(), cleanse_names_96_reactive()))
    }
  })
  output$controls_2 <- renderTable({
    if(is.null(input$acsins_names)){
      return(NULL)
    } else {
      return(create_control_table(cleanse_data_reactive_num(), cleanse_names_96_reactive()))
    }
  })
  
  
  #96-well Format:
  output$averages <- DT::renderDT({
    if(is.null(input$acsins_file)){
      return(NULL)
    } else if(is.null(input$acsins_names)){
      return(NULL)
    } else {
      datatable(build_96_well_table_new(cleanse_data_reactive_num(), cleanse_names_384_reactive()), filter = list(position = 'top'), options = list(pageLength = 96, columnDefs = list(list(className = 'dt-center', targets = 0:15)), search = list(regex = TRUE), searchCols = list(NULL)), rownames= FALSE) %>% 
        formatStyle('Sample_Name', fontWeight = styleEqual('Buffer', 'bold'), color = styleEqual('Buffer', '#337AB7')) %>%
        formatStyle('Standard_Deviation', color = styleInterval(1, c('', 'red')), fontWeight = styleInterval(1, c('','bold'))) %>%
        formatStyle(c('Well1', 'Well2', 'Well3', 'Well4'), backgroundColor = '#EFFCD0' ) %>% 
        formatStyle(c('PW1', 'PW2', 'PW3', 'PW4'), color = styleInterval(c(625),c('green','black')), fontWeight = 'bold') %>%
        formatStyle('Delta_PWL', backgroundColor = styleInterval(c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30), c('#5F9566','#79BD82','#AFCC90','#D6DD8E','#EADC8D','#F9DB88','#F8CC8C','#F3C47E','#F8B086','#F78D7C','#F77561','#F77852','#F25C45','#F02F2C')), fontWeight = 'bold') 
    }
  })
  
  #Reactive slider threshold value:
  #(For Optimized 96-well table)
  threshold_reactive <- reactive({input$threshold})
  
  #Optimized 96-well table:
  output$optimized <- DT::renderDT({
    if(is.null(input$acsins_file)){
      return(NULL)
    } else if(is.null(input$acsins_names)){
      return(NULL)
    } else {
      datatable(build_96_well_optimized_table(cleanse_data_reactive_num(), cleanse_names_384_reactive(), threshold_reactive()),  filter = list(position = 'top'), options = list(pageLength = 96, columnDefs = list(list(className = 'dt-center', targets = 0:16)), search = list(regex = TRUE), searchCols = list(NULL)), rownames= FALSE) %>% 
        formatStyle('Sample_Name', fontWeight = styleEqual('Buffer', 'bold'), color = styleEqual('Buffer', '#337AB7')) %>%
        formatStyle('NA?', target = 'row', backgroundColor = styleEqual(c('', '*'), c('', '#FCFDD7')) ) %>% 
        formatStyle('Standard_Deviation', color = styleInterval(1, c('', 'red')), fontWeight = 'bold') %>%
        formatStyle(c('PW1', 'PW2', 'PW3', 'PW4'), color = styleInterval(c(625),c('green','black')), fontWeight = 'bold' ) %>%
        formatStyle(c('Well1', 'Well2', 'Well3', 'Well4'), backgroundColor = '#EFFCD0' ) %>%
        formatStyle('Delta_PWL', backgroundColor = styleInterval(c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30), c('#5F9566','#79BD82','#AFCC90','#D6DD8E','#EADC8D','#F9DB88','#F8CC8C','#F3C47E','#F8B086','#F78D7C','#F77561','#F77852','#F25C45','#F02F2C')), fontWeight = 'bold') 
      
    }
  })
  
  
  #Plot:
  output$plot <- renderPlot({
    if(is.null(input$acsins_file)){
      return(NULL)
    } else if(is.null(input$acsins_names)) {
      return(loessplot_acsins_noname(cleanse_data_reactive_num(),input$plot_select)) } ## Had to add this step; there was an error with the plot if a partially read plate was uplaoded.
    else {
      return(loessplot_acsins_names(cleanse_data_reactive_num(),input$plot_select, cleanse_names_384_reactive()))
    }
  })
  
  #Plot Grid (Percent in Range):
  output$plot_grid <- renderPlot({
    if(is.null(input$acsins_file)){
      return(NULL)
    } else {
      return(build_plot_grid(cleanse_data_reactive_num(), cleanse_names_384_gfit_reactive(), nameindicator_reactive(), sortby="percent" ))
    }
  }) 
  
  #Plot Grid (RMSE):
  output$plot_grid_scatter <- renderPlot({
    if(is.null(input$acsins_file)){
      return(NULL)
    } else {
      return(build_plot_grid(cleanse_data_reactive_num(), cleanse_names_384_gfit_reactive(), nameindicator_reactive(), sortby="rmse" ))
    }
  })
  
  #Reactive Checkbox: (For multi-curve plot page)
  #Adds vertical lines to plot if checked
  checkbox_reactive <- reactive({
    if (input$checkbox == FALSE) {
      return(0)
    } else {
      return(1)
    }
  })
  
  ##Multiple Loess Models GGPLOT:
  output$multiple_loess <- renderPlot({
    if(is.null(input$acsins_file)){
      return(NULL)
    } else {
      return(plot_loess_multiple(cleanse_data_reactive_num(), input$multiple_1, input$multiple_2, input$multiple_3, input$multiple_4, 
                                 input$multiple_5, input$multiple_6, input$multiple_7, input$multiple_8, checkbox_reactive()))
    }
  })
  
  
  #Goodness of Fit table:
  output$guide <- DT::renderDT({
    req(input$acsins_file)
    req(input$acsins_names)
    t <- table_fit_categories(cleanse_data_reactive_num(), cleanse_names_384_gfit_reactive(), sortby = "percent")
    colnames(t) <- c('Well','Sample Name','Percent in Range','RMSE')
    t$Well <- factor(t$Well, levels=levels(build_blank_names_gfit()$Well))
    return(datatable(t, options = list(columnDefs = list(list(className = 'dt-center', targets = 0:3)), search = list(regex = TRUE)), rownames= FALSE))
  })
  
  
  ##-------- Manual Exclusion of Wells from Analysis --------##
  
  ## Reactive selected wells from summary table:
  selected_wells_reactive <- reactive({ build_summary_table(cleanse_data_reactive(), cleanse_names_384_reactive())$Well[input$summary_rows_selected] })
  
  ## Print selected rows in sidebar (SUMMARY TABLE):
  output$rows_summary_selected <- renderPrint ({
    s = input$summary_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
  
  ## Observe event of clicking "Apply Exclusion" (id: exclude) button
  observeEvent(input$exclude, {
    newdata <- exclude_selected_columns_rawdata(clean_acsins_excel_new(input$acsins_file$datapath), selected_wells_reactive())
    newdata_num <- exclude_selected_columns_rawdata(clean_acsins_excel_num(input$acsins_file$datapath, for_display = 0), selected_wells_reactive())
    
    output$summary <- DT::renderDT({ datatable(build_summary_table(newdata, cleanse_names_384_reactive()), rownames= FALSE,  filter = list(position = 'top'), options = list(columnDefs = list(list(className = 'dt-center', targets = 0:7)), search = list(regex = TRUE), searchCols = list(NULL)) ) })
    
    output$averages <- DT::renderDT({datatable(build_96_well_table_new(newdata_num, cleanse_names_384_reactive()),  filter = list(position = 'top'), options = list(pageLength = 96, columnDefs = list(list(className = 'dt-center', targets = 0:12)), search = list(regex = TRUE), searchCols = list(NULL)), rownames= FALSE) %>% 
        formatStyle('Sample_Name', fontWeight = styleEqual('Buffer', 'bold'), color = styleEqual('Buffer', '#337AB7')) %>%
        formatStyle('Standard_Deviation', color = styleInterval(1, c('', 'red')), fontWeight = styleInterval(1, c('','bold'))) %>%
        formatStyle(c('Well1', 'Well2', 'Well3', 'Well4'), backgroundColor = '#EFFCD0' ) %>% 
        formatStyle(c('PW1', 'PW2', 'PW3', 'PW4'), color = styleInterval(c(625),c('green','black')), fontWeight = 'bold') %>%
        formatStyle('Delta_PWL', backgroundColor = styleInterval(c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30), c('#5F9566','#79BD82','#AFCC90','#D6DD8E','#EADC8D','#F9DB88','#F8CC8C','#F3C47E','#F8B086','#F78D7C','#F77561','#F77852','#F25C45','#F02F2C')), fontWeight = 'bold') })
    
    output$optimized <- DT::renderDT({datatable(build_96_well_optimized_table(newdata_num, cleanse_names_384_reactive(), threshold_reactive()),  filter = list(position = 'top'), options = list(pageLength = 96, columnDefs = list(list(className = 'dt-center', targets = 0:13)), search = list(regex = TRUE), searchCols = list(NULL)), rownames= FALSE) %>% 
        formatStyle('Sample_Name', fontWeight = styleEqual('Buffer', 'bold'), color = styleEqual('Buffer', '#337AB7')) %>%
        formatStyle('NA?', target = 'row', backgroundColor = styleEqual(c('', '*'), c('', '#FCFDD7')) ) %>% 
        formatStyle('Standard_Deviation', color = styleInterval(1, c('', 'red')), fontWeight = 'bold') %>%
        formatStyle(c('PW1', 'PW2', 'PW3', 'PW4'), color = styleInterval(c(625),c('green','black')), fontWeight = 'bold' ) %>%
        formatStyle(c('Well1', 'Well2', 'Well3', 'Well4'), backgroundColor = '#EFFCD0' ) %>%
        formatStyle('Delta_PWL', backgroundColor = styleInterval(c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30), c('#5F9566','#79BD82','#AFCC90','#D6DD8E','#EADC8D','#F9DB88','#F8CC8C','#F3C47E','#F8B086','#F78D7C','#F77561','#F77852','#F25C45','#F02F2C')), fontWeight = 'bold') })
    
    output$plot_grid <- renderPlot({ build_plot_grid(newdata_num, cleanse_names_384_gfit_reactive(), nameindicator_reactive(), sortby="percent" )  }) 
    
    output$plot_grid_scatter <- renderPlot({build_plot_grid(newdata_num, cleanse_names_384_gfit_reactive(), nameindicator_reactive(), sortby="rmse" ) })
    
    output$guide <- DT::renderDT({ datatable(table_fit_categories(newdata_num, cleanse_names_384_gfit_reactive(), sortby = "percent"), options = list(columnDefs = list(list(className = 'dt-center', targets = 0:3)), search = list(regex = TRUE)), rownames= FALSE) })
    
  })
  
  
  ## Observe event of clicking "Refresh Data" (id: refresh) button
  observeEvent(input$refresh, {
    cleanse_data_reactive <- reactive({clean_acsins_excel_new(input$acsins_file$datapath)})
    
    output$summary <- DT::renderDT({ datatable(build_summary_table(cleanse_data_reactive(), cleanse_names_384_reactive()), rownames= FALSE,  filter = list(position = 'top'), options = list(columnDefs = list(list(className = 'dt-center', targets = 0:7)), search = list(regex = TRUE), searchCols = list(NULL)) ) })
    
    output$averages <- DT::renderDT({datatable(build_96_well_table_new(cleanse_data_reactive(), cleanse_names_384_reactive()),  filter = list(position = 'top'), options = list(pageLength = 96, columnDefs = list(list(className = 'dt-center', targets = 0:12)), search = list(regex = TRUE), searchCols = list(NULL)), rownames= FALSE) %>% 
        formatStyle('Sample_Name', fontWeight = styleEqual('Buffer', 'bold'), color = styleEqual('Buffer', '#337AB7')) %>%
        formatStyle('Standard_Deviation', color = styleInterval(1, c('', 'red')), fontWeight = styleInterval(1, c('','bold'))) %>%
        formatStyle(c('Well1', 'Well2', 'Well3', 'Well4'), backgroundColor = '#EFFCD0' ) %>% 
        formatStyle(c('PW1', 'PW2', 'PW3', 'PW4'), color = styleInterval(c(625),c('green','black')), fontWeight = 'bold') %>%
        formatStyle('Delta_PWL', backgroundColor = styleInterval(c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30), c('#5F9566','#79BD82','#AFCC90','#D6DD8E','#EADC8D','#F9DB88','#F8CC8C','#F3C47E','#F8B086','#F78D7C','#F77561','#F77852','#F25C45','#F02F2C')), fontWeight = 'bold') })
    
    output$optimized <- DT::renderDT({datatable(build_96_well_optimized_table(cleanse_data_reactive(), cleanse_names_384_reactive(), threshold_reactive()),  filter = list(position = 'top'), options = list(pageLength = 96, columnDefs = list(list(className = 'dt-center', targets = 0:13)), search = list(regex = TRUE), searchCols = list(NULL)), rownames= FALSE) %>% 
        formatStyle('Sample_Name', fontWeight = styleEqual('Buffer', 'bold'), color = styleEqual('Buffer', '#337AB7')) %>%
        formatStyle('NA?', target = 'row', backgroundColor = styleEqual(c('', '*'), c('', '#FCFDD7')) ) %>% 
        formatStyle('Standard_Deviation', color = styleInterval(1, c('', 'red')), fontWeight = 'bold') %>%
        formatStyle(c('PW1', 'PW2', 'PW3', 'PW4'), color = styleInterval(c(625),c('green','black')), fontWeight = 'bold' ) %>%
        formatStyle(c('Well1', 'Well2', 'Well3', 'Well4'), backgroundColor = '#EFFCD0' ) %>%
        formatStyle('Delta_PWL', backgroundColor = styleInterval(c(0,2.5,5,7.5,10,12.5,15,17.5,20,22.5,25,27.5,30), c('#5F9566','#79BD82','#AFCC90','#D6DD8E','#EADC8D','#F9DB88','#F8CC8C','#F3C47E','#F8B086','#F78D7C','#F77561','#F77852','#F25C45','#F02F2C')), fontWeight = 'bold') })
    
    output$plot_grid <- renderPlot({ build_plot_grid(cleanse_data_reactive(), cleanse_names_384_gfit_reactive(), nameindicator_reactive(), sortby="percent" )  }) 
    
    output$plot_grid_scatter <- renderPlot({build_plot_grid(cleanse_data_reactive(), cleanse_names_384_gfit_reactive(), nameindicator_reactive(), sortby="rmse" ) })
    
    output$guide <- DT::renderDT({ datatable(table_fit_categories(cleanse_data_reactive(), cleanse_names_384_gfit_reactive(), sortby = "percent"), options = list(columnDefs = list(list(className = 'dt-center', targets = 0:3)), search = list(regex = TRUE)), rownames= FALSE) })
    
  })
  
  
  
  ##-------- Downloading Data (Side Panel Buttons) --------##
  
  #Excel Workbook Containing All Tables:
  output$exportData = downloadHandler(paste0("ac-sins_",Sys.Date(),".xlsx"), content = function(file) {
    write.xlsx(build_summary_table(cleanse_data_reactive(), cleanse_names_384_reactive()), file, sheetName = "Summary Table", row.names = FALSE, append = TRUE)
    write.xlsx(calc_meanbuffers_table(build_summary_table(cleanse_data_reactive(), cleanse_names_384_reactive())), file, sheetName = "Mean Buffer Table", row.names = FALSE, append = TRUE)
    write.xlsx(create_control_table(cleanse_data_reactive_num(), cleanse_names_96_reactive()), file, sheetName = "Control Name Table", row.names = FALSE, append = TRUE)
    write.xlsx(build_96_well_table_new(cleanse_data_reactive_num(), cleanse_names_384_reactive()), file, sheetName = "96-Well Table", row.names = FALSE, append = TRUE)
    write.xlsx(build_96_well_optimized_table(cleanse_data_reactive_num(), cleanse_names_384_reactive(), threshold_reactive()), file, sheetName = "96-Well Optimized Table", row.names = FALSE, append = TRUE)
    write.xlsx(table_fit_categories(cleanse_data_reactive_num(), cleanse_names_384_gfit_reactive(), sortby = "percent"), file, sheetName = "Goodness of Fit Table", row.names = FALSE, append = TRUE)
    write.xlsx(generate_report(cleanse_data_reactive_num(), cleanse_names_384_reactive(), date_time_reactive()), file, sheetName = "Full 96-Well Report", row.names = FALSE, append = TRUE)
  })
  
  ### SEPARATE DOWNLOADS PER TABLE:
  output$download_summary = downloadHandler(paste0("ac-sins_",Sys.Date(),"_SummmaryTable.xlsx"), content = function(file) {
    write.xlsx(build_summary_table(cleanse_data_reactive(), cleanse_names_384_reactive()), file, sheetName = "Summary Table", row.names = FALSE)
  })
  
  output$download_96 = downloadHandler(paste0("ac-sins_",Sys.Date(),"_96WellTable.xlsx"), content = function(file) {
    write.xlsx(build_96_well_table_new(cleanse_data_reactive_num(), cleanse_names_384_reactive()), file, sheetName = "96-Well Table", row.names = FALSE)
  })
  
  output$download_96_opt = downloadHandler(paste0("ac-sins_",Sys.Date(),"_Opt96WellTable.xlsx"), content = function(file) {
    write.xlsx(build_96_well_optimized_table(cleanse_data_reactive_num(), cleanse_names_384_reactive(), threshold_reactive()), file, sheetName = "Optimized 96-Well Table", row.names = FALSE)
  })
  
  output$download_gfit = downloadHandler(paste0("ac-sins_",Sys.Date(),"_Opt96WellTable.xlsx"), content = function(file) {
    write.xlsx(table_fit_categories(cleanse_data_reactive_num(), cleanse_names_384_gfit_reactive(), sortby = "percent"), file, sheetName = "Goodness of Fit Table", row.names = FALSE)
  })
  
  ## Print selected rows in sidebar:
  output$rows_selected <- renderPrint ({
    s = input$optimized_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
  output$rows_selected_2 <- renderPrint ({
    s = input$averages_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(s, sep = ', ')
    }
  })
  
  
  #DOWNLOAD REPORTS:
  output$report_norm = downloadHandler(paste0("ac-sins_report_96norm_",Sys.Date(),".xlsx"), content = function(file) {
    
    report <- generate_report(cleanse_data_reactive_num(), cleanse_names_384_reactive(), date_time_reactive())[c(input$averages_rows_selected),]
    
    # order the data frame AFTER selection
    report$control <- 0
    report$control[which(report$Control_Name!='')] <- 1
    report <- report[order(report$control, decreasing = TRUE),]
    report$control <- NULL
    
    wb <- createWorkbook()
    addWorksheet(wb, "Abridged 96-Well Table")
    writeData(wb, "Abridged 96-Well Table", report, startCol = 1)
    conditionalFormatting(wb, "Abridged 96-Well Table", cols = 3, rows = 1:nrow(report)+1, 
                          type = "colourScale", style = c("#119F1A", "yellow", "red"), rule = c(5, 8, 33), showValue = FALSE, gradient = FALSE)
    
    saveWorkbook(wb, file = file, overwrite = TRUE)
  })
  
  
  output$report_norm_full = downloadHandler(paste0("ac-sins_report_96norm_full_",Sys.Date(),".xlsx"), content = function(file) {
    
    report <- generate_report(cleanse_data_reactive_num(), cleanse_names_384_reactive(), date_time_reactive())
    
    #order the data frame AFTER selection
    report$control <- 0
    report$control[which(report$Control_Name!='')] <- 1
    report <- report[order(report$control, decreasing = TRUE),]
    report$control <- NULL
    
    wb <- createWorkbook()
    addWorksheet(wb, "Full 96-Well Table")
    writeData(wb, "Full 96-Well Table", report, startCol = 1)
    conditionalFormatting(wb, "Full 96-Well Table", cols = 3, rows = 1:nrow(report)+1, 
                          type = "colourScale", style = c("#119F1A", "yellow", "red"), rule = c(5, 8, 33), showValue = FALSE, gradient = FALSE)
    
    saveWorkbook(wb, file = file, overwrite = TRUE)
  })
  
  
  output$report = downloadHandler(paste0("ac-sins_report_96opt_",Sys.Date(),".xlsx"), content = function(file) {
    
    report <- generate_report_opt(cleanse_data_reactive(), cleanse_names_384_reactive(), threshold_reactive(), date_time_reactive())[c(input$optimized_rows_selected),]
    
    # order the data frame AFTER selection
    report$control <- 0
    report$control[which(report$Control_Name!='')] <- 1
    report <- report[order(report$control, decreasing = TRUE),]
    report$control <- NULL
    
    wb <- createWorkbook()
    addWorksheet(wb, "Abridged Opt 96-Well")
    writeData(wb, "Abridged Opt 96-Well", report, startCol = 1)
    conditionalFormatting(wb, "Abridged Opt 96-Well", cols = 3, rows = 1:nrow(report)+1, 
                          type = "colourScale", style = c("#119F1A", "yellow", "red"), rule = c(5, 8, 33), showValue = FALSE, gradient = FALSE)
    
    saveWorkbook(wb, file = file, overwrite = TRUE)
  })
  
  
  output$report_full = downloadHandler(paste0("ac-sins_report_96opt_full_",Sys.Date(),".xlsx"), content = function(file) {
    
    report <- generate_report_opt(cleanse_data_reactive_num(), cleanse_names_384_reactive(), threshold_reactive(), date_time_reactive())
    
    # order the data frame AFTER selection
    report$control <- 0
    report$control[which(report$Control_Name!='')] <- 1
    report <- report[order(report$control, decreasing = TRUE),]
    report$control <- NULL
    
    wb <- createWorkbook()
    addWorksheet(wb, "Full Optimized 96-Well Table")
    writeData(wb, "Full Optimized 96-Well Table", report, startCol = 1)
    conditionalFormatting(wb, "Full Optimized 96-Well Table", cols = 3, rows = 1:nrow(report)+1, 
                          type = "colourScale", style = c("#119F1A", "yellow", "red"), rule = c(5, 8, 33), showValue = FALSE, gradient = FALSE)
    
    saveWorkbook(wb, file = file, overwrite = TRUE)
  })
  
  
  
  ##-------- Send Data to Biologica --------##
  
  #Reactive time stamp and default experiment name:
  #experiment_name_default <- reactive({return(print(paste0(input$experiment_name,Sys.time()) )) }) 
  
  #Reactive data frame for database:
  df_for_db_reactive <- reactive({ build_results_df(cleanse_data_reactive(), cleanse_names_96_reactive(), threshold_reactive()) })
  
  ## Render the action button only if both datasets uploaded.
  output$call_APIs <- renderUI({ 
    if(is.null(input$acsins_file)){
      return(NULL) 
    } else if(is.null(input$acsins_names)) { return(NULL) 
    } else { return(actionButton('call_APIs',"Send Data to Biologica")) } 
  })
  
  observeEvent(input$call_APIs, {
    source("./call_API.R")
    output$token_api_response <- renderPrint({ return(post_token_api()) })
    #output$experiment_api_response <- renderPrint({ return( post_experiment_api(paste0(input$experiment_name,Sys.time()), date_time_iso_reactive()) ) })
    # Need reactive values: (1) experiment id, (2) column of tripled sample ids, (3) reactive data frame of reshaped results.
    
    experiment_id_reactive <- reactive({ post_experiment_api(paste0(input$experiment_name,Sys.time()), date_time_iso_reactive()) })
    output$experiment_api_response <- renderPrint({ experiment_id_reactive() })
    
    sample_id_reactive
    sample_id_column_reactive
    
    #This needs to be the reactive data frame that contains the results in reshaped format and the reactive experiment and sample IDs. 
    dataframe_results_api_reactive <- reactive({ build_results_df_api(cleanse_data_reactive(), cleanse_names_96_reactive(), threshold_reactive(), experiment_id_reactive(), sample_id_column_reactive()) })
    
    output$sample_api_response <- renderPrint({ return() })
    output$results_api_response <- renderPrint({ return() })
  })
  
  
  #--------------------------------------------#
  #           Benchling File Download          #
  #--------------------------------------------#
  
  output$benchling <- renderUI({
    req(input$acsins_file)
    req(input$acsins_names)
    downloadButton('download_for_benchling', "Benchling Result File", icon=icon("arrow-circle-right"))
  })
  
  acq_date <- reactive({
    return(capture_date_for_benchling(input$acsins_file$datapath))
  })
  
  uploaded_file_name <- reactive({
    req(input$acsins_file)
    name <- input$acsins_file$name
    name <- strsplit(name, "[.]")[[1]][1]
    return(name)
  })
  
  benchling_table <- reactive({
    return(create_benchling_table(cleanse_data_reactive_num(), cleanse_names_384_reactive(), acq_date()))
  })
  
  output$download_for_benchling <- downloadHandler(
    filename = function() {
      paste0("AC-SINS_Benchling_File_", uploaded_file_name(), "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write.xlsx2(benchling_table(), file, sheetName = "Benchling", append = FALSE, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  
}

shinyApp(ui,server)
