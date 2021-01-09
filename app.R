#-------------------------------------------#
# Microdrop Assay Tool: R Shiny Application
# Auralee Walmer
# 2019-2020
#-------------------------------------------#

library(shiny)
library(readxl)
library(ggplot2)
library(plotly)
library(xtable) 
library(rmarkdown)
library(gridExtra)
library(xlsx)
library(rlist)
library(scales)
library(shinythemes)
library(data.table)
library(openxlsx)
library(shinyjs)
library(lubridate)

rm(list = ls())
gc()

source("functions_aw.R")
source("uploading_microdrop.R")

ui = fluidPage(
  
  titlePanel("Microdrop Assay Analysis Tool"),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  navbarPage(
    "Upload data and select type.",
    
    tabPanel("Upload File",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   fileInput('file1', 'Upload File (.xlsx)', 
                             accept = c(".xlsx")),
                   radioButtons("plateSize1", NULL, 
                                choices = list("24hr Full Plate" = 1, "24hr Half Plate" = 2,
                                               "3hr Full Plate" = 3, "3hr Half Plate" = 4), 
                                selected = character(0))
                 ),
                 mainPanel(
                   tableOutput('contents2'),
                   br(),
                   br(),
                   tableOutput('contents1'))
               )
             )
    ),
    
    tabPanel("Parsed Data",
             fluidPage(
               mainPanel( 
                 tableOutput('ParsedData')
               )
             )
    ),
    
    tabPanel("Pivot by Sample",
             fluidPage(sidebarPanel(uiOutput("sampleNames")),
                       mainPanel(
                         textOutput('s1'),
                         tableOutput('pivotTableSamp1'),
                         textOutput('s2'),
                         tableOutput('pivotTableSamp2'),
                         textOutput('s3'),
                         tableOutput('pivotTableSamp3'),
                         textOutput('s4'),
                         tableOutput('pivotTableSamp4'),
                         textOutput('s5'),
                         tableOutput('pivotTableSamp5'),
                         textOutput('s6'),
                         tableOutput('pivotTableSamp6'),
                         textOutput('s7'),
                         tableOutput('pivotTableSamp7'),
                         textOutput('s8'),
                         tableOutput('pivotTableSamp8'), 
                         width = 3))
    ),
    
    tabPanel("Pivot by Buffer",
             fluidPage(sidebarPanel(uiOutput("bufferNames")),
                       mainPanel(
                         textOutput('b1'),
                         tableOutput('pivotTableBuff1'),
                         textOutput('b2'),
                         tableOutput('pivotTableBuff2'),
                         textOutput('b3'),
                         tableOutput('pivotTableBuff3'),
                         textOutput('b4'),
                         tableOutput('pivotTableBuff4'),
                         textOutput('b5'),
                         tableOutput('pivotTableBuff5'),
                         textOutput('b6'),
                         tableOutput('pivotTableBuff6'),
                         textOutput('b7'),
                         tableOutput('pivotTableBuff7'),
                         textOutput('b8'),
                         tableOutput('pivotTableBuff8')
                         , width = 3))
    ),
    
    tabPanel("Onset Heat Table",
             mainPanel(
               tableOutput('onset_preheat'),
               br(),
               plotOutput('onset_heat',width="600px",height="500px"),
               br(),
               br()
             )
    ),
    
    
    tabPanel("Plots by Sample",
             fluidPage(sidebarPanel(uiOutput("sample_selection1")),
                       mainPanel(
                         textOutput('onset_type_text'),
                         uiOutput('Render_GroupPlots_Sample1'),
                         uiOutput('Render_GroupPlots_Sample2'),
                         uiOutput('Render_GroupPlots_Sample3'),
                         uiOutput('Render_GroupPlots_Sample4'),
                         uiOutput('Render_GroupPlots_Sample5'),
                         uiOutput('Render_GroupPlots_Sample6'),
                         uiOutput('Render_GroupPlots_Sample7'),
                         uiOutput('Render_GroupPlots_Sample8')
                       )
             )
    ),
    
    tabPanel("Plots by Buffer",
             fluidPage(sidebarPanel(uiOutput("buffer_selection1")),
                       mainPanel(
                         textOutput('onset_type_text2'),
                         uiOutput('Render_GroupPlots_Buffer1'),
                         uiOutput('Render_GroupPlots_Buffer2'),
                         uiOutput('Render_GroupPlots_Buffer3'),
                         uiOutput('Render_GroupPlots_Buffer4'),
                         uiOutput('Render_GroupPlots_Buffer5'),
                         uiOutput('Render_GroupPlots_Buffer6'),
                         uiOutput('Render_GroupPlots_Buffer7'),
                         uiOutput('Render_GroupPlots_Buffer8')
                       )
             )
    ),
    
    tabPanel("Individual Curve Fit",
             fluidPage(sidebarPanel(
               uiOutput("sample_selection2"),
               uiOutput("buffer_selection2")
             ),
             mainPanel(
               plotOutput('plot_curve_fit', width = "600", height = "600"),
               tableOutput('rmse_report'),
               uiOutput('download_individual'),
               br(),
               br()
             )
             )
    ),
    
    tabPanel("Curve Fit by Sample",
             fluidPage(sidebarPanel(
               uiOutput("sample_selection3")
             ),
             mainPanel(
               textOutput('onset_type_text3'),
               uiOutput('Render_GroupFits_Sample1'),
               uiOutput('Render_GroupFits_Sample2'),
               uiOutput('Render_GroupFits_Sample3'),
               uiOutput('Render_GroupFits_Sample4'),
               uiOutput('Render_GroupFits_Sample5'),
               uiOutput('Render_GroupFits_Sample6'),
               uiOutput('Render_GroupFits_Sample7'),
               uiOutput('Render_GroupFits_Sample8')
             )
             )
    ),
    
    tabPanel("Curve Fit by Buffer",
             fluidPage(sidebarPanel(
               uiOutput("buffer_selection3")
             ),
             mainPanel(
               textOutput('onset_type_text4'),
               uiOutput('Render_GroupFits_Buffer1'),
               uiOutput('Render_GroupFits_Buffer2'),
               uiOutput('Render_GroupFits_Buffer3'),
               uiOutput('Render_GroupFits_Buffer4'),
               uiOutput('Render_GroupFits_Buffer5'),
               uiOutput('Render_GroupFits_Buffer6'),
               uiOutput('Render_GroupFits_Buffer7'),
               uiOutput('Render_GroupFits_Buffer8')
             )
             )
    ),
    
    tabPanel("Validation",
             mainPanel(
               tableOutput('invalid_table'),
               DT::DTOutput("validation")
             )
    ),
    
    tabPanel("Report",
             mainPanel(
               tableOutput("report_table_onset"),
               tableOutput("report_table_ic50"),
               uiOutput("downloadData_type"),
               uiOutput("downloadPDF_type")
             )
    ),
    
    tabPanel("Methodology",
             mainPanel( 
               h4("Curve Fit and Onset Calculation Method"),
               h5("Model"),
               uiOutput("formula"),
               p("The log-logistic model above is used to fit the data for both the 24-hour and 3-hour onset types."),
               h5("Onset"),
               p("The onset value is calculated by finding the point where the IC-5 (or IC-95, the in case of 3-hour onset data)
               intersects the fitted curve."),
               uiOutput("ic_type"),
               h5("Quasi IC-50"),
               p("The IC-50 used in this analysis method is not a true IC-50 as given by the model used. 
                 It is more of a 'quasi' IC-50 for the following reason: "),
               p("Since most of the data we see for MicroDrop does not have a tail, the lower asymptote (or upper asymptote 
                 in the case of the 3-hour data) is not fulfilled by the fitted curve. Thus, the true IC-50 may not even be 
                 visible, since the data is cut off before the final asymptote manifests.
                 "),
               uiOutput("quasi_ic50"),
               h5("Onset Validation Conditions"),
               uiOutput("onset_validation_condition"),
               h5("Developer"),
               p("For any questions regarding this app, or for any bug-fix requests, please reach out to Auralee Walmer at walmer_auralee@network.lilly.com."),
               br(),
               br()
             )
             
    ),
    
    tabPanel("Benchling",
             mainPanel(
               h5("Download File for Benchling Upload"),
               uiOutput("benchling"),
               br(),
               br()
             )
    )
    
  )
  
)


server = function(input, output){
  
  #--------------------------------------------#
  #           READ INPUT DATA FILE             #
  #--------------------------------------------#
  
  reactExcel <- reactive({
    req(input$file1)
    return(input$file1)
  })
  
  plateSize <- reactive({
    req(input$plateSize1)
    return(input$plateSize1) # returns number 1-4; 1 & 3 mean full plate, 2 & 4 mean half plate
  })
  
  contents1 <- reactive({
    req(input$file1)
    req(input$plateSize1)
    inFile <- reactExcel()
    if (plateSize()==1 | plateSize()==2) {
      excel <- read_excel(inFile$datapath, 1, col_names = letters[1:25])
    } else {
      excel <- read_excel(inFile$datapath, 1, col_names = letters[1:24])
    }
    return(excel)
  })
  
  output$contents1 <- renderTable({
    contents1()
  })
  
  contents2 <- reactive({
    req(input$file1)
    req(input$plateSize1)
    inFile <- reactExcel()
    return(read_excel(inFile$datapath, 2))
  })
  
  output$contents2 <- renderTable({
    contents2()
  })
  
  ## Reactive rename:
  PlateInfo <- reactive({ contents2() })
  FirstTab <- reactive({ contents1() })
  
  ## Onset type:
  onset_type_reactive <- reactive({
    if (plateSize() == 1 | plateSize() == 2) {
      return('24hr')
    } else {
      return('3hr')
    }
  })
  
  ## Plate type
  plate_type <- reactive({
    if (plateSize()==1 | plateSize()==3) {
      return('F')
    } else {
      return('H')
    }
  })
  
  ## Isolated plate data
  plate <- reactive({
    if (onset_type_reactive()=='24hr') {
      capture_third_input_set(contents1())
    } else {
      contents1()[1:16,1:24]
    }
  })
  
  ## Parsed Table, reactive:
  ParsedTable <- reactive({
    if (plateSize() == 1){
      df <- parse_fplate_4quads(plate(), PlateInfo())
      df <- merge_fplate_plateinfo(df, PlateInfo())
      df <- transform_parsedtable_to24hr(df)
    } else if (plateSize() == 2){
      df <- parse_hplate_8quads(plate(), PlateInfo())
      df <- merge_hplate_plateinfo(df, PlateInfo()) 
      df <- transform_parsedtable_to24hr(df)
    } else if (plateSize() == 3){
      df <- parse_fplate_4quads(plate(), PlateInfo())
      df <- merge_fplate_plateinfo(df, PlateInfo())
    } else {
      df <- parse_hplate_8quads(plate(), PlateInfo())
      df <- merge_hplate_plateinfo(df, PlateInfo())
    }
    df <- rename_redundant_samples(df)
    return(df)
  })
  
  ## Parsed Table Output
  output$ParsedData <- renderTable({
    df <- ParsedTable()
    df$Raw <- formatC(df$Raw, digits = 4, format = "f")
    df$`PEG %` <- formatC(df$`PEG %`, digits=1, format = "f")
    df$Quadrant <- formatC(df$Quadrant, digits = 1)
    return(df)
  })
  
  
  #--------------------------------------------#
  #           SAMPLE & BUFFER NAMES            #
  #--------------------------------------------#
  
  samNam <- reactive({
    df <- ParsedTable()
    if (plateSize() == 1||plateSize() == 3){
      d1 <- as.character(unique(df$Sample))[1]
      d2 <- as.character(unique(df$Sample))[2]
      d3 <- as.character(unique(df$Sample))[3]
      d4 <- as.character(unique(df$Sample))[4]
      return(c(d1, d2, d3, d4))
    } else {
      d1 <- as.character(unique(df$Sample))[1]
      d2 <- as.character(unique(df$Sample))[2]
      d3 <- as.character(unique(df$Sample))[3]
      d4 <- as.character(unique(df$Sample))[4]
      d5 <- as.character(unique(df$Sample))[5]
      d6 <- as.character(unique(df$Sample))[6]
      d7 <- as.character(unique(df$Sample))[7]
      d8 <- as.character(unique(df$Sample))[8]
      return(c(d1, d2, d3, d4, d5, d6, d7, d8))
    }
  })
  
  bufNam <- reactive({
    df <- ParsedTable()
    if (plateSize() == 1||plateSize() == 3){
      d1 <- as.character(unique(df$Buffer))[1]
      d2 <- as.character(unique(df$Buffer))[2]
      d3 <- as.character(unique(df$Buffer))[3]
      d4 <- as.character(unique(df$Buffer))[4]
      d5 <- as.character(unique(df$Buffer))[5]
      d6 <- as.character(unique(df$Buffer))[6]
      d7 <- as.character(unique(df$Buffer))[7]
      d8 <- as.character(unique(df$Buffer))[8]
      return(c(d1, d2, d3, d4, d5, d6, d7, d8))
    } else {
      d1 <- as.character(unique(df$Buffer))[1]
      d2 <- as.character(unique(df$Buffer))[2]
      d3 <- as.character(unique(df$Buffer))[3]
      d4 <- as.character(unique(df$Buffer))[4]
      return(c(d1, d2, d3, d4))
    }
  })
  
  
  #--------------------------------------------#
  #              SAMPLE TABLES                 #
  #--------------------------------------------#
  
  output$pivotTableSamp1 <- renderTable({
    req(input$varia)
    if ("sample 1" %in% input$varia){
      pivot_sample_table(samNam()[1], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableSamp2 <- renderTable({
    req(input$varia)
    if ("sample 2" %in% input$varia){
      pivot_sample_table(samNam()[2], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableSamp3 <- renderTable({
    req(input$varia)
    if ("sample 3" %in% input$varia){
      pivot_sample_table(samNam()[3], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableSamp4 <- renderTable({
    req(input$varia)
    if ("sample 4" %in% input$varia){
      pivot_sample_table(samNam()[4], ParsedTable()) 
    } else {
      return(NULL)
    }})
  output$pivotTableSamp5 <- renderTable({
    req(input$varia)
    if ("sample 5" %in% input$varia){
      pivot_sample_table(samNam()[5], ParsedTable()) 
    } else {
      return(NULL)
    }})
  output$pivotTableSamp6 <- renderTable({
    req(input$varia)
    if ("sample 6" %in% input$varia){
      pivot_sample_table(samNam()[6], ParsedTable()) 
    } else {
      return(NULL)
    }})
  output$pivotTableSamp7 <- renderTable({
    req(input$varia)
    if ("sample 7" %in% input$varia){
      pivot_sample_table(samNam()[7], ParsedTable()) 
    } else {
      return(NULL)
    }})
  output$pivotTableSamp8 <- renderTable({
    req(input$varia)
    if ("sample 8" %in% input$varia){
      pivot_sample_table(samNam()[8], ParsedTable()) 
    } else {
      return(NULL)
    }})
  
  ## Sample Titles ## 
  output$s1 <- renderText({
    req(input$varia)
    if ("sample 1" %in% input$varia){
      paste(samNam()[1], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$s2 <- renderText({
    req(input$varia)
    if ("sample 2" %in% input$varia){
      paste(samNam()[2], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$s3 <- renderText({
    req(input$varia)
    if ("sample 3" %in% input$varia){
      paste(samNam()[3], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$s4 <- renderText({
    req(input$varia)
    if ("sample 4" %in% input$varia){
      paste(samNam()[4], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$s5 <- renderText({
    req(input$varia)
    if ("sample 5" %in% input$varia){
      paste(samNam()[5], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$s6 <- renderText({
    req(input$varia)
    if ("sample 6" %in% input$varia){
      paste(samNam()[6], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$s7 <- renderText({
    req(input$varia)
    if ("sample 7" %in% input$varia){
      paste(samNam()[7], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$s8 <- renderText({
    req(input$varia)
    if ("sample 8" %in% input$varia){
      paste(samNam()[8], "Pivot", " ") 
    } else {
      return(NULL)}})
  
  output$sampleNames <- renderUI({
    checkboxGroupInput("varia", "Select sample:", generate_sample_checkboxes(ParsedTable()), selected=generate_sample_checkboxes(ParsedTable())) 
  })
  
  #--------------------------------------------#
  #              BUFFER TABLES                 #
  #--------------------------------------------#
  
  output$pivotTableBuff1 <- renderTable({
    req(input$var)
    if ("buffer 1" %in% input$var){
      pivot_buffer_table(bufNam()[1], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableBuff2 <- renderTable({
    req(input$var)
    if ("buffer 2" %in% input$var){
      pivot_buffer_table(bufNam()[2], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableBuff3 <- renderTable({
    req(input$var)
    if ("buffer 3" %in% input$var){
      pivot_buffer_table(bufNam()[3], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableBuff4 <- renderTable({
    req(input$var)
    if ("buffer 4" %in% input$var){
      pivot_buffer_table(bufNam()[4], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableBuff5 <- renderTable({
    req(input$var)
    if ("buffer 5" %in% input$var){
      pivot_buffer_table(bufNam()[5], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableBuff6 <- renderTable({
    req(input$var)
    if ("buffer 6" %in% input$var){
      pivot_buffer_table(bufNam()[6], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableBuff7 <- renderTable({
    req(input$var)
    if ("buffer 7" %in% input$var){
      pivot_buffer_table(bufNam()[7], ParsedTable()) 
    } else {
      return(NULL)}})
  output$pivotTableBuff8 <- renderTable({
    req(input$var)
    if ("buffer 8" %in% input$var){
      pivot_buffer_table(bufNam()[8], ParsedTable()) 
    } else {
      return(NULL)}})
  
  output$b1 <- renderText({
    req(input$var)
    if ("buffer 1" %in% input$var){
      paste(bufNam()[1], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$b2 <- renderText({
    req(input$var)
    if ("buffer 2" %in% input$var){
      paste(bufNam()[2], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$b3 <- renderText({
    req(input$var)
    if ("buffer 3" %in% input$var){
      paste(bufNam()[3], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$b4 <- renderText({
    req(input$var)
    if ("buffer 4" %in% input$var){
      paste(bufNam()[4], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$b5 <- renderText({
    req(input$var)
    if ("buffer 5" %in% input$var){
      paste(bufNam()[5], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$b6 <- renderText({
    req(input$var)
    if ("buffer 6" %in% input$var){
      paste(bufNam()[6], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$b7 <- renderText({
    req(input$var)
    if ("buffer 7" %in% input$var){
      paste(bufNam()[7], "Pivot", " ") 
    } else {
      return(NULL)}})
  output$b8 <- renderText({
    req(input$var)
    if ("buffer 8" %in% input$var){
      paste(bufNam()[8], "Pivot", " ") 
    } else {
      return(NULL)}})
  
  output$bufferNames <- renderUI({
    checkboxGroupInput("var", "Select buffer:",  generate_buffer_checkboxes(ParsedTable()), selected=generate_buffer_checkboxes(ParsedTable())) 
  })
  
  
  #--------------------------------------------#
  #               HEAT MAP                     #
  #--------------------------------------------#
  
  output$ic50 <- renderTable({
    ic50_table()
  }, digits = 1)
  output$onset_preheat <- renderTable({
    onset_table()
  }, digits = 1)
  
  heat_table_reactive <- reactive({
    if (onset_type_reactive()=='24hr') {
      df <- invalidated_onsets_24hr(ParsedTable())
    } else {
      df <- invalidated_onsets_3hr(ParsedTable())
    }
    df$Sample <- factor(df$Sample, levels=c(as.character(unique(df$Sample))))
    df$Buffer <- factor(df$Buffer, levels=c(rev(as.character(unique(df$Buffer)))))
    df[is.na(df)] <- 36
    return(
      ggplot(data = df, aes(x=Sample, y=Buffer, fill=Onset)) + 
        geom_tile(data=df, aes(fill = Onset)) + geom_text(data=df, aes(label = paste0(round(Onset, digits=1), " %"))) + 
        scale_fill_gradientn(colors=c("red3","red1","white","darkolivegreen3","forestgreen"), na.value="gray88", limits=c(4,36),breaks=seq(4,36,8)) + 
        scale_x_discrete(position = "top") +
        theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.x  = element_text(angle=90, vjust=0.5, size=11), axis.text.y  = element_text(angle=0, vjust=0.5, size=11))
    )
  })
  
  output$onset_heat <- renderPlot({
    heat_table_reactive()
  })
  
  
  #--------------------------------------------#
  #           Plots by Sample                  #
  #--------------------------------------------#
  
  output$sample_selection1 <- renderUI({
    checkboxGroupInput("sample", "Select sample:", generate_sample_checkboxes(ParsedTable()), selected=generate_sample_checkboxes(ParsedTable()))
  })
  
  output$onset_type_text <- renderText({
    onset_type_reactive()
  })
  
  ## List of Grouped Plots
  ListofGroupPlots_Sample <- reactive({
    req(input$sample)
    return(return_list_of_plots_sample(ParsedTable(), onset_type_reactive()))
  })
  
  ## Define each plot by extracting it from the list
  output$GroupPlots_Sample1 <- renderPlot({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    lis[[1]]
  })
  output$GroupPlots_Sample2 <- renderPlot({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    lis[[2]]
  })
  output$GroupPlots_Sample3 <- renderPlot({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    lis[[3]]
  })
  output$GroupPlots_Sample4 <- renderPlot({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    lis[[4]]
  })
  output$GroupPlots_Sample5 <- renderPlot({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    lis[[5]]
  })
  output$GroupPlots_Sample6 <- renderPlot({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    lis[[6]]
  })
  output$GroupPlots_Sample7 <- renderPlot({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    lis[[7]]
  })
  output$GroupPlots_Sample8 <- renderPlot({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    lis[[8]]
  })
  
  ## Plots in RenderUI format
  output$Render_GroupPlots_Sample1 <- renderUI({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    if ("sample 1" %in% input$sample){
      plotOutput('GroupPlots_Sample1', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Sample1', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Sample2 <- renderUI({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    if ("sample 2" %in% input$sample){
      plotOutput('GroupPlots_Sample2', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Sample2', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Sample3 <- renderUI({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    if ("sample 3" %in% input$sample){
      plotOutput('GroupPlots_Sample3', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Sample3', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Sample4 <- renderUI({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    if ("sample 4" %in% input$sample){
      plotOutput('GroupPlots_Sample4', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Sample4', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Sample5 <- renderUI({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    if ("sample 5" %in% input$sample){
      plotOutput('GroupPlots_Sample5', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Sample5', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Sample6 <- renderUI({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    if ("sample 6" %in% input$sample){
      plotOutput('GroupPlots_Sample6', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Sample6', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Sample7 <- renderUI({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    if ("sample 7" %in% input$sample){
      plotOutput('GroupPlots_Sample7', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Sample7', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Sample8 <- renderUI({
    req(input$sample)
    lis <- ListofGroupPlots_Sample()
    if ("sample 8" %in% input$sample){
      plotOutput('GroupPlots_Sample8', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Sample8', width = "0%", height = "0px")
    }
  })
  
  
  #--------------------------------------------#
  #           Plots by Buffer                  #
  #--------------------------------------------#
  
  output$buffer_selection1 <- renderUI({
    checkboxGroupInput("buffer", "Select buffer:", generate_buffer_checkboxes(ParsedTable()), selected=generate_buffer_checkboxes(ParsedTable()))
  }) 
  
  output$onset_type_text2 <- renderText({
    onset_type_reactive()
  })
  
  ## List of Grouped Plots
  ListofGroupPlots_Buffer <- reactive({
    req(input$buffer)
    return(return_list_of_plots_buffer(ParsedTable(), onset_type_reactive()))
  })
  
  ## Define each plot by extracting it from the list
  output$GroupPlots_Buffer1 <- renderPlot({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    lis[[1]]
  })
  output$GroupPlots_Buffer2 <- renderPlot({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    lis[[2]]
  })
  output$GroupPlots_Buffer3 <- renderPlot({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    lis[[3]]
  })
  output$GroupPlots_Buffer4 <- renderPlot({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    lis[[4]]
  })
  output$GroupPlots_Buffer5 <- renderPlot({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    lis[[5]]
  })
  output$GroupPlots_Buffer6 <- renderPlot({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    lis[[6]]
  })
  output$GroupPlots_Buffer7 <- renderPlot({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    lis[[7]]
  })
  output$GroupPlots_Buffer8 <- renderPlot({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    lis[[8]]
  })
  
  ## Plots in RenderUI format
  output$Render_GroupPlots_Buffer1 <- renderUI({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    if ("buffer 1" %in% input$buffer){
      plotOutput('GroupPlots_Buffer1', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Buffer1', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Buffer2 <- renderUI({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    if ("buffer 2" %in% input$buffer){
      plotOutput('GroupPlots_Buffer2', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Buffer2', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Buffer3 <- renderUI({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    if ("buffer 3" %in% input$buffer){
      plotOutput('GroupPlots_Buffer3', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Buffer3', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Buffer4 <- renderUI({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    if ("buffer 4" %in% input$buffer){
      plotOutput('GroupPlots_Buffer4', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Buffer4', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Buffer5 <- renderUI({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    if ("buffer 5" %in% input$buffer){
      plotOutput('GroupPlots_Buffer5', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Buffer5', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Buffer6 <- renderUI({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    if ("buffer 6" %in% input$buffer){
      plotOutput('GroupPlots_Buffer6', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Buffer6', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Buffer7 <- renderUI({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    if ("buffer 7" %in% input$buffer){
      plotOutput('GroupPlots_Buffer7', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Buffer7', width = "0%", height = "0px")
    }
  })
  output$Render_GroupPlots_Buffer8 <- renderUI({
    req(input$buffer)
    lis <- ListofGroupPlots_Buffer()
    if ("buffer 8" %in% input$buffer){
      plotOutput('GroupPlots_Buffer8', width = "600px", height = "500px")
    } else {
      plotOutput('GroupPlots_Buffer8', width = "0%", height = "0px")
    }
  })
  
  
  #--------------------------------------------#
  #      Individual Onset Curve Fit Tab        #
  #--------------------------------------------#
  
  output$sample_selection2 <- renderUI({
    radioButtons("select_sample2", "Select sample:", samNam(), selected = character(0))
  })
  
  output$buffer_selection2 <- renderUI({
    radioButtons("select_buffer2", "Select buffer:", bufNam(), selected = character(0))
  })
  
  basicxy_react <- reactive({
    req(input$select_sample2)
    req(input$select_buffer2)
    filter_basic_xytable(ParsedTable(), input$select_sample2, input$select_buffer2, onset_type_reactive())
  })
  
  basicxy_react_multiplied <- reactive({
    multiply_fitted_values(basicxy_react())
  })
  
  # Reactive values: 1. drm model, 2. fitted values, 3. parameters from model, 4. y-range, ic-5 and ic-95, 5. onset value
  y_fitted_reactive <- reactive({
    get_drm_fitted_output(basicxy_react())$y_fitted
  })
  
  ic5_reactive <- reactive({ # parameter 'a'
    if (onset_type_reactive()=='24hr') {
      get_ic5(y_fitted_reactive())
    } else {
      get_ic95_3hr(y_fitted_reactive())
    }
    
  })
  b_reactive <- reactive({
    get_drm_fitted_output(basicxy_react())$b
  })
  c_reactive <- reactive({
    get_drm_fitted_output(basicxy_react())$c
  })
  d_reactive <- reactive({
    get_drm_fitted_output(basicxy_react())$d
  })
  e_reactive <- reactive({
    get_drm_fitted_output(basicxy_react())$e
  })
  
  ic50_y_react <- reactive({
    get_ic50_y(y_fitted_reactive(), b_reactive(), c_reactive(), d_reactive(), e_reactive(), onset_type_reactive())
  })
  ic50_x_react <- reactive({
    get_ic50_x(ic50_y_react(), b_reactive(), c_reactive(), d_reactive(), e_reactive())
  })
  
  onset_reactive <- reactive({
    solve_onset(y=ic5_reactive(), b=b_reactive(), c=c_reactive(), d=d_reactive(), e=e_reactive())
  })
  
  # plot with onset and without ic-50
  plot_curve_fit_reactive <- reactive ({
    p <- plot_drm_fit_onset(basicxy_react(), y_fitted_reactive(), ic5_reactive(), onset_reactive())
    q <- add_ic50_to_plot(p, ic50_x = ic50_x_react(), ic50_y = ic50_y_react())
    if (onset_type_reactive()=='24hr' & abs(y_fitted_reactive()[1]-y_fitted_reactive()[length(y_fitted_reactive())]) > 0.05) {
      return(q)
    } else {
      return(p)
    }
  })
  
  # final plot with ic-50
  output$plot_curve_fit <- renderPlot({
    plot_curve_fit_reactive()
  })
  
  rmse_table <- reactive({
    req(input$select_sample2)
    req(input$select_buffer2)
    df <- build_rmse_table(ParsedTable(), input$select_sample2, input$select_buffer2, onset_type_reactive())
    nums <- unlist(lapply(df, is.numeric))
    df[,nums] %<>% lapply(function(x) as.character(round(x, digits=4)))
    return(df)
  })
  output$rmse_report <- renderTable({
    r <- rmse_table()
    if (onset_type_reactive()=="24hr") { colnames(r) <- c('Sample','Buffer', paste0(onset_type_reactive(), ' Onset'),'Onset.Q', 'IC-50','Slope at IC-50', 'RMSE', 'R-sq', 'Upper Bound','Lower Bound','Fixed Bound') }
    else { colnames(r) <- c('Sample','Buffer', paste0(onset_type_reactive(), ' Onset'),'Onset.Q', 'Midway Slope', 'RMSE', 'R-sq', 'Upper Bound','Lower Bound','Fixed Bound') }
    return(r)
  })
  
  
  output$download_individual <- renderUI({
    req(input$select_sample2)
    req(input$select_buffer2)
    downloadButton('download_indiv', 'Download PDF of Plot')
  })
  
  output$download_indiv <- downloadHandler(
    filename = function() {
      paste0(uploaded_file_name(), "_microdrop_", onset_type_reactive(), "_individual_plot_", input$select_sample2, "_", input$select_buffer2, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file)
      grid.arrange(
        top=paste0("Onset Prediction Plot: ", onset_type_reactive(), ", ", input$select_sample2, " & ", input$select_buffer2),
        plot_curve_fit_reactive(),
        nrow=1)
      dev.off()
    }
  )
  
  
  #--------------------------------------------#
  #           Curve Fit by Sample              #
  #--------------------------------------------#
  
  output$sample_selection3 <- renderUI({
    checkboxGroupInput("sample_select3", "Select sample:", generate_sample_checkboxes(ParsedTable()), selected=generate_sample_checkboxes(ParsedTable()))
  }) 
  
  output$onset_type_text3 <- renderText({
    onset_type_reactive()
  })
  
  ## List of Grouped Plots
  ListofGroupFits_Sample <- reactive({
    req(input$sample_select3)
    return(return_list_of_curvefits_sample(ParsedTable(), onset_type_reactive()))
  })
  
  ## Define each plot by extracting it from the list
  output$GroupFits_Sample1 <- renderPlot({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    lis[[1]]
  })
  output$GroupFits_Sample2 <- renderPlot({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    lis[[2]]
  })
  output$GroupFits_Sample3 <- renderPlot({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    lis[[3]]
  })
  output$GroupFits_Sample4 <- renderPlot({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    lis[[4]]
  })
  output$GroupFits_Sample5 <- renderPlot({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    lis[[5]]
  })
  output$GroupFits_Sample6 <- renderPlot({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    lis[[6]]
  })
  output$GroupFits_Sample7 <- renderPlot({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    lis[[7]]
  })
  output$GroupFits_Sample8 <- renderPlot({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    lis[[8]]
  })
  
  ## Plots in RenderUI format
  output$Render_GroupFits_Sample1 <- renderUI({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    if ("sample 1" %in% input$sample_select3){
      plotOutput('GroupFits_Sample1', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Sample1', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Sample2 <- renderUI({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    if ("sample 2" %in% input$sample_select3){
      plotOutput('GroupFits_Sample2', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Sample2', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Sample3 <- renderUI({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    if ("sample 3" %in% input$sample_select3){
      plotOutput('GroupFits_Sample3', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Sample3', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Sample4 <- renderUI({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    if ("sample 4" %in% input$sample_select3){
      plotOutput('GroupFits_Sample4', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Sample4', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Sample5 <- renderUI({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    if ("sample 5" %in% input$sample_select3){
      plotOutput('GroupFits_Sample5', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Sample5', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Sample6 <- renderUI({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    if ("sample 6" %in% input$sample_select3){
      plotOutput('GroupFits_Sample6', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Sample6', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Sample7 <- renderUI({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    if ("sample 7" %in% input$sample_select3){
      plotOutput('GroupFits_Sample7', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Sample7', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Sample8 <- renderUI({
    req(input$sample_select3)
    lis <- ListofGroupFits_Sample()
    if ("sample 8" %in% input$sample_select3){
      plotOutput('GroupFits_Sample8', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Sample8', width = "0%", height = "0px")
    }
  })
  
  
  #--------------------------------------------#
  #           Curve Fit by Buffer              #
  #--------------------------------------------#
  
  output$buffer_selection3 <- renderUI({
    checkboxGroupInput("buffer_select3", "Select buffer:", generate_buffer_checkboxes(ParsedTable()), selected=generate_buffer_checkboxes(ParsedTable()))
  }) 
  
  output$onset_type_text4 <- renderText({
    onset_type_reactive()
  })
  
  ## List of Grouped Plots
  ListofGroupFits_Buffer <- reactive({
    req(input$buffer_select3)
    return(return_list_of_curvefits_buffer(ParsedTable(), onset_type_reactive()))
  })
  
  ## Define each plot by extracting it from the list
  output$GroupFits_Buffer1 <- renderPlot({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    lis[[1]]
  })
  output$GroupFits_Buffer2 <- renderPlot({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    lis[[2]]
  })
  output$GroupFits_Buffer3 <- renderPlot({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    lis[[3]]
  })
  output$GroupFits_Buffer4 <- renderPlot({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    lis[[4]]
  })
  output$GroupFits_Buffer5 <- renderPlot({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    lis[[5]]
  })
  output$GroupFits_Buffer6 <- renderPlot({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    lis[[6]]
  })
  output$GroupFits_Buffer7 <- renderPlot({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    lis[[7]]
  })
  output$GroupFits_Buffer8 <- renderPlot({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    lis[[8]]
  })
  
  ## Plots in RenderUI format
  output$Render_GroupFits_Buffer1 <- renderUI({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    if ("buffer 1" %in% input$buffer_select3){
      plotOutput('GroupFits_Buffer1', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Buffer1', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Buffer2 <- renderUI({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    if ("buffer 2" %in% input$buffer_select3){
      plotOutput('GroupFits_Buffer2', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Buffer2', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Buffer3 <- renderUI({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    if ("buffer 3" %in% input$buffer_select3){
      plotOutput('GroupFits_Buffer3', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Buffer3', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Buffer4 <- renderUI({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    if ("buffer 4" %in% input$buffer_select3){
      plotOutput('GroupFits_Buffer4', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Buffer4', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Buffer5 <- renderUI({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    if ("buffer 5" %in% input$buffer_select3){
      plotOutput('GroupFits_Buffer5', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Buffer5', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Buffer6 <- renderUI({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    if ("buffer 6" %in% input$buffer_select3){
      plotOutput('GroupFits_Buffer6', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Buffer6', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Buffer7 <- renderUI({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    if ("buffer 7" %in% input$buffer_select3){
      plotOutput('GroupFits_Buffer7', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Buffer7', width = "0%", height = "0px")
    }
  })
  output$Render_GroupFits_Buffer8 <- renderUI({
    req(input$buffer_select3)
    lis <- ListofGroupFits_Buffer()
    if ("buffer 8" %in% input$buffer_select3){
      plotOutput('GroupFits_Buffer8', width = "600px", height = "500px")
    } else {
      plotOutput('GroupFits_Buffer8', width = "0%", height = "0px")
    }
  })
  
  #--------------------------------------------#
  #         REVIEW/validation Tab              #
  #--------------------------------------------#
  
  validation_table <- reactive({
    df <- ParsedTable()
    df$Raw <- round(df$Raw, digits = 2)
    if (onset_type_reactive()=='24hr') {
      df[c(7)] <- round(df[c(7)], digits = 2)
    }
    return(df)
  })
  
  invalid_table_reactive <- reactive({
    if (is.null(input$validation_rows_selected)) {
      df <- ParsedTable()
      df$Validation <- 'valid'
    } else {
      df <- ParsedTable()[input$validation_rows_selected,]
      df$Validation <- 'invalid'
    }
    return(df)
  })
  
  output$invalid_table <- renderTable({
    if (!is.null(input$validation_rows_selected)) {
      invalid_table_reactive()
    } else {
      NULL
    }
  })
  
  final_valid_results_raw <- reactive({
    if (is.null(input$validation_rows_selected)) {
      df <- ParsedTable()
      df$Validation <- 'valid'
      return(df)
    } else {
      df <- join(ParsedTable(), invalid_table_reactive())
      df$Validation[is.na(df$Validation)] <- 'valid'
      return(df)
    }
  })
  
  output$validation <- DT::renderDataTable(validation_table(), options = list(pageLength = 384, dom = 't'),rownames = FALSE)
  
  
  
  #--------------------------------------------#
  #               Report Tab                   #
  #--------------------------------------------#
  
  onset_table <- reactive({
    if (onset_type_reactive()=='24hr') {
      df <- invalidated_onsets_24hr(ParsedTable())
      df$Sample <- factor(df$Sample, levels=c(as.character(unique(df$Sample))))
      df <- dcast_onset_df_24hr(df)
    } else {
      df <- invalidated_onsets_3hr(ParsedTable())
      df$Sample <- factor(df$Sample, levels=c(as.character(unique(df$Sample))))
      df <- dcast_onset_df_3hr(df)
    }
    df[is.na(df)] <- 36
    # change number of decimal places to 1
    df[c(2:ncol(df))] <- round(df[c(2:ncol(df))], 1)
    return(df)
  })
  
  ic50_table <- reactive({
    if (onset_type_reactive()=='24hr') {
      df <- invalidated_onsets_24hr(ParsedTable())
      df$Sample <- factor(df$Sample, levels=c(as.character(unique(df$Sample))))
      df <- dcast_ic50_df_24hr(df)
    } else {
      df <- invalidated_onsets_3hr(ParsedTable())
      df$Sample <- factor(df$Sample, levels=c(as.character(unique(df$Sample))))
      df <- dcast_ic50_df_3hr(df)
    }
    df[is.na(df)] <- 36
    # change number of decimal places to 1
    df[c(2:ncol(df))] <- round(df[c(2:ncol(df))], 1)
    return(df)
  })
  
  output$report_table_onset <- renderTable({onset_table()}, digits = 1)
  
  output$report_table_ic50 <- renderTable({
    if (onset_type_reactive()=='24hr') {
      ic50_table()
    } else {
      NULL
    }
    
  }, digits = 1)
  
  stacked_tables_sample <- reactive({
    # set up 1:
    df <- Reduce(rbind,c(list(pivot_sample_table(samNam()[1], ParsedTable())),rep(list(NA),3)))
    peg_values <- as.character(unique(colnames(df)))
    df[nrow(df),] <- colnames(df)
    df[nrow(df)-1,] <- c(samNam()[2],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    # loop through remainder:
    for (s in 2:length(samNam())) {
      df <- Reduce(rbind,c(list(df), list(pivot_sample_table(samNam()[s], ParsedTable())), rep(list(NA),3)))
      if (s != length(samNam())) {
        df[nrow(df),] <- colnames(df)
        df[nrow(df)-1,] <- c(samNam()[s+1],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
      }
    }
    df[2:nrow(df),] <- df[1:nrow(df)-1,]
    df[1,] <- peg_values
    colnames(df) <- samNam()[1]
    return(df)
  })
  
  stacked_tables_buffer <- reactive({
    # set up 1:
    df <- Reduce(rbind,c(list(pivot_buffer_table(bufNam()[1], ParsedTable())),rep(list(NA),3)))
    peg_values <- as.character(unique(colnames(df)))
    df[nrow(df),] <- colnames(df)
    df[nrow(df)-1,] <- c(bufNam()[2],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    # loop through remainder:
    for (s in 2:length(bufNam())) {
      df <- Reduce(rbind,c(list(df), list(pivot_buffer_table(bufNam()[s], ParsedTable())), rep(list(NA),3)))
      if (s != length(bufNam())) {
        df[nrow(df),] <- colnames(df)
        df[nrow(df)-1,] <- c(bufNam()[s+1],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
      }
    }
    df[2:nrow(df),] <- df[1:nrow(df)-1,]
    df[1,] <- peg_values
    colnames(df) <- bufNam()[1]
    return(df)
  })
  
  rmse_table_report <- reactive({
    df <- build_rmse_table_all(ParsedTable(), onset_type_reactive())
    return(df)
  })
  
  ## acquisition date:
  acq_date <- reactive({
    req(input$file1)
    req(input$plateSize1)
    sheet1 <- FirstTab()
    if (onset_type_reactive()=='24hr') {
      row <- which(sheet1[,1]=="Date:")
      if (length(row)!=0) {
        #date <- paste0(as.character(sheet1[row,2]),"T:00:00:00")
        capture <- as.numeric(sheet1[row,2])
        date <- as.Date(capture, origin = "1899-12-30")
        date <- format(date, format="%Y-%m-%d")
      } else {
        date <- format(Sys.Date(), format="%Y-%m-%d")
      }
    } else {
      row <- which(sheet1[,1]=="Acquisition Date:")
      if (length(row)!=0) {
        capture <- as.numeric(sheet1[row,2])
        date <- as.Date(capture, origin = "1899-12-30")
        date <- format(date, format="%Y-%m-%d")
      } else {
        date <- format(Sys.Date(), format="%Y-%m-%d")
      }
    }
    return(date)
  })
  
  ##info tab table in downloadable xlsx report
  info_report_tab <- reactive({
    generate_info_table(title = uploaded_file_name(), onset_type = onset_type_reactive(), acq_date = acq_date())
  })
  
  ## Reactive value to capture file name ##
  uploaded_file_name <- reactive({
    name <- input$file1$name
    name <- gsub(".xlsx","",name)
    return(name)
  })
  
  output$downloadData_type <- renderUI({
    req(input$file1)
    req(input$plateSize1)
    downloadButton('downloadData', 'Download XLSX: Tables')
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(uploaded_file_name(),"_", onset_type_reactive(), "_Analysis_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write.xlsx2(ParsedTable(), file, sheetName = "Parsed Data", append = FALSE, row.names = FALSE)
      write.xlsx2(round_digits4(stacked_tables_sample()), file, sheetName = "Pivot by Sample", append=TRUE, row.names = FALSE)
      write.xlsx2(round_digits4(stacked_tables_buffer()), file, sheetName = "Pivot by Buffer", append=TRUE, row.names = FALSE)
      write.xlsx2(round_digits4(onset_table()), file, sheetName = paste0(onset_type_reactive(), " Onset Report"), append = TRUE, row.names = FALSE)
      if (onset_type_reactive()=='24hr') {
        write.xlsx2(round_digits4(ic50_table()), file, sheetName = "IC-50 Report", append = TRUE, row.names = FALSE)
      }
      write.xlsx2(round_digits4(rmse_table_report()), file, sheetName = "Goodness of Fit", append = TRUE, row.names = FALSE, check.names = FALSE)
      write.xlsx2(info_report_tab(), file, sheetName = "Info", append = TRUE, row.names = FALSE, check.names = FALSE)
    }
  )
  
  report_table_onset <- reactive({
    if (length(samNam()) > 4) {
      return(t(onset_table()))
    } else {
      return(onset_table())
    }
    return(df)
  })
  report_table_ic50 <- reactive({
    if (length(samNam()) > 4) {
      return(t(ic50_table()))
    } else {
      return(ic50_table())
    }
    return(df)
  })
  
  output$downloadPDF_type <- renderUI({
    if (plateSize()==1 | plateSize()==3) {
      downloadButton('downloadPDF_fp', 'Download PDF: Plots')
    } else {
      downloadButton('downloadPDF_hp', 'Download PDF: Plots')
    }
  })
  
  output$downloadPDF_fp <- downloadHandler(
    filename = function() {
      paste0(uploaded_file_name(),"_", onset_type_reactive(), "_Analysis_","Plots", ".pdf")
    },
    content = function(file) {
      pdf(file)
      if (onset_type_reactive()=='24hr') {
        grid.arrange(
          top="Onset and IC-50 Calculations (24-hour)",
          tableGrob(onset_table()),
          tableGrob(ic50_table()),
          nrow=2)
      } else {
        grid.arrange(
          top="Onset Calculations (3-hour)",
          tableGrob(onset_table()),
          nrow=1)
      }
      grid.arrange(
        top="Heat Map",
        heat_table_reactive(),
        nrow=1)
      grid.arrange(
        top="Grouped Plots by Sample",
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[1], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[2], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Sample",
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[3], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[4], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Buffer",
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[1], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[2], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Buffer",
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[3], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[4], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Buffer",
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[5], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[6], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Buffer",
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[7], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[8], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      dev.off()
    }
  )
  
  output$downloadPDF_hp <- downloadHandler(
    filename = function() {
      paste0(uploaded_file_name(),"_", onset_type_reactive(), "_Analysis_","Plots", ".pdf")
    },
    content = function(file) {
      pdf(file)
      if (onset_type_reactive()=='24hr') {
        grid.arrange(
          top="Onset and IC-50 Calculations (24-hour)",
          tableGrob(t(onset_table())),
          tableGrob(t(ic50_table())),
          nrow=2)
      } else {
        grid.arrange(
          top="Onset Calculations (3-hour)",
          tableGrob(t(onset_table())),
          nrow=1)
      }
      grid.arrange(
        top="Heat Map",
        heat_table_reactive(),
        nrow=1)
      grid.arrange(
        top="Grouped Plots by Sample",
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[1], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[2], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Sample",
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[3], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[4], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Sample",
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[5], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[6], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Sample",
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[7], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_sample(ParsedTable(), samNam()[8], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Buffer",
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[1], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[2], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      grid.arrange(
        top="Grouped Plots by Buffer",
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[3], onset_type = onset_type_reactive())),
        addSmallLegend(curvefit_plot_by_buffer(ParsedTable(), bufNam()[4], onset_type = onset_type_reactive())),
        nrow=2, ncol=1,
        widths = 2)
      dev.off()
    }
  )
  
  #--------------------------------------------#
  #               Methodology                  #
  #--------------------------------------------#
  
  output$formula <- renderUI({
    withMathJax(
      helpText("Four-parameter log-logistic function"),
      helpText('$$ f(x) = c + \\frac{d - c}{ 1 + \\left(exp
               \\left(b \\left(\\log x - log e\\right)\\right)\\right)}\\!$$'),
      helpText("where"),
      helpText("b = the Hill coefficient;"),
      helpText("c = the lower bound;"),
      helpText("d = the upper bound;"),
      helpText("e = the IC-50.")
    )
  })
  
  output$ic_type <- renderUI({
    withMathJax(
      helpText("IC-5 (24-hour data) = horizontal line 5% below the upper asymptote of the model."),
      helpText("IC-95 (3-hour data) = horizontal line 5% above the lower asymptote of the model.")
    )
  })
  
  output$quasi_ic50 <- renderUI({
    withMathJax(
      helpText("Quasi IC-50 = the 50% mark between the onset and the data point 
               associated with the maximum PEG % value."),
      helpText("For the 24-hour data, parameter c is replaced with the minimum fitted value."),
      helpText("For the 3-hour data, parameter d is replaced with the maximum fitted value.")
    )
  })
  
  output$onset_validation_condition <- renderUI({
    withMathJax(
      helpText("For the 24-hour data, if all fitted data points fall within a vertical range of 0.1, the onset is invalidated. (Updated from 0.05 to 0.1 in April 2020.)"),
      helpText("For the 3-hour data, if all fitted data points fall between the vertical interval of 0 to 0.2, the onset is invalidated.")
    )
  })
  
  
  #--------------------------------------------#
  #           Upload to Biologica              #
  #--------------------------------------------#
  
  instance <- "test"
  
  if (instance=="test") {
    ## URLS IN TEST INSTANCE
    url_token_test <- ""
    url_protocol_test <- ""
    url_project_test <- ""
    url_experiment_test <- ""
    url_sample_biophysical_test <- ""
    url_results_protocol_def_test <- ""
    url_results_protocol_test <- ""
  } else {
    ## URLS IN TEST INSTANCE
    url_token_test <- ""
    url_protocol_test <- ""
    url_project_test <- ""
    url_experiment_test <- ""
    url_sample_biophysical_test <- ""
    url_results_protocol_def_test <- ""
    url_results_protocol_test <- ""
  }
  
  
  authentication_reactive <- eventReactive(input$Authenticate, {
    t <- call_token_api(input$username_authenticate, input$password_authenticate, url_token_test)
    return(t)
  })
  
  token_reactive <- reactive({
    return(authentication_reactive()$cookies$value[authentication_reactive()$cookies$name=='biologica_jwt'])
  })
  
  output$authentication_response <- renderText({
    if (authentication_reactive()$status_code==200) {
      return(paste0("Authentication successful! (Status Code = ",authentication_reactive()$status_code,") Token = ", token_reactive()))
    } else {
      return(paste0("Authentication not successful. (Status Code = ",authentication_reactive(),")"))
    }
  })
  
  ## once authentication_reactive() status code becomes 200, then allow rest of UI to emerge
  
  output$breakline1 <- renderUI({
    if (authentication_reactive()$status_code==200) { hr() }
  })
  
  output$protocol <- renderUI({
    if (authentication_reactive()$status_code==200) {
      tagList(tags$h4("Protocol"), tags$p("Title = Microdrop, Version = 2"))
    } else {
      NULL
    }
  })
  
  output$breakline2 <- renderUI({
    if (authentication_reactive()$status_code==200) { hr() }
  })
  
  projects_reactive <- reactive({
    return(get_projects(url_project_test))
  })
  
  list_projects <- reactive({
    get_list_projects(projects_reactive())
  })
  
  output$project <- renderUI({
    if (authentication_reactive()$status_code==200) {
      fluidRow(
        column(7, tagList(tags$h4("Select Project"), selectInput("project_list","",choices=list_projects())) )
      )
    } else {
      NULL
    }
  })
  
  output$project_confirmation <- renderText({
    if (authentication_reactive()$status_code==200) { paste0("Project Selected: ", input$project_list) }
  })
  
  output$breakline3 <- renderUI({
    if (authentication_reactive()$status_code==200) { hr() }
  })
  
  output$experiment <- renderUI({
    if (authentication_reactive()$status_code==200) {
      fluidRow(
        column(8, tagList(tags$h4("Submit Experiment"), textInput("experiment_name","Experiment Name", value = uploaded_file_name() ) ) )
      )
    }
  })
  
  check_experiment_exists_reactive <- reactive({
    req(input$experiment_name)
    id <- get_experiment_id(experiment_name = input$experiment_name, url = url_experiment_test)
    return(id)
  })
  
  output$check_experiment_exists <- renderUI({
    req(input$experiment_name)
    if (check_experiment_exists_reactive()==0) {
      tagList(tags$p("Experiment does not yet exist. Enter experiment details below."))
    } else {
      tagList(tags$p("Experiment already exists. Results will be added to this experiment."))
    }
    
  })
  
  output$submit_experiment2 <- renderUI({
    if (check_experiment_exists_reactive()==0) {
      actionButton("confirm_experiment_final", "Submit New Experiment", icon=icon("arrow-circle-right"))
    } else {
      actionButton("confirm_experiment_final", "Continue", icon=icon("arrow-circle-right"))
    }
  })
  
  output$experiment_details <- renderUI({
    req(input$experiment_name)
    if (check_experiment_exists_reactive()==0) {
      div(
        fluidRow(
          column(10, tagList(textInput("experiment_description","Description")))
        ),
        fluidRow(
          tagList(tags$h5("Run From Date")),
          tagList(tags$p(paste("Acqusition Date =", acq_date() )))
        ),
        fluidRow(
          column(3, tagList(textInput("run_from_date_year","Year", value="2020" ))),
          column(3, tagList(selectInput("run_from_date_month","Month", choices=c("01","02","03","04","05","06","07","08","09","10","11","12") ))),
          column(3, tagList(selectInput("run_from_date_day","Day", choices=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31') )))
        ),
        fluidRow(
          tagList(tags$h5("Run To Date")),
        ),
        fluidRow(
          column(3, tagList(textInput("run_to_date_year","Year", value="2020" ))),
          column(3, tagList(selectInput("run_to_date_month","Month", choices=c("01","02","03","04","05","06","07","08","09","10","11","12") ))),
          column(3, tagList(selectInput("run_to_date_day","Day", choices=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31') )))
        ),
      )
      
    }
  })
  
  run_from_date_formatted <- reactive({
    runfrom <- paste0(input$run_from_date_year, "-", input$run_from_date_month, "-", input$run_from_date_day,  "T00:00:00")
    return(runfrom)
  })
  
  run_to_date_formatted <- reactive({
    runto <- paste0(input$run_to_date_year, "-", input$run_to_date_month, "-", input$run_to_date_day,  "T00:00:00")
    return(runto)
  })
  
  
  output$breakline4 <- renderUI({
    if (authentication_reactive()$status_code==200) { hr() }
  })
  
  ## Next, once user enters the experiment name, do: GET protocol and GET or CREATE experiment. Return response message.
  
  experiment_reactive <- eventReactive(input$confirm_experiment_final, {
    req(input$experiment_description)
    
    p <- get_protocol_id(protocol_title = "Microdrop", protocol_version = "2", url = url_protocol_test)
    pr <- get_project_id(project_name = input$project_list, url = url_project_test)
    
    e_exists <- get_experiment_id(experiment_name = input$submit_experiment, url = url_experiment_test)
    
    
    experiment_id <- create_experiment(protocol_id = p, experiment_name = input$experiment_name, status="Completed", 
                                       run_from_date = run_from_date_formatted(), run_to_date = run_to_date_formatted(), description=input$experiment_description, 
                                       funding_project_id = pr, owning_team_name = "Biophysical Assay Group",
                                       url = url_experiment_test)
    return(experiment_id) 
    ## ultimately, if experiment already exists, it will return the existing ID
    ## if experiment does not yet exist, it will create a new experiment
  })
  
  # [1] "{\"run_from_date\":[\"Datetime has wrong format. Use one of these formats instead: YYYY-MM-DDThh:mm[:ss[.uuuuuu]][+HH:MM|-HH:MM|Z].\"],\"run_to_date\":[\"Datetime has wrong format. Use one of these formats instead: YYYY-MM-DDThh:mm[:ss[.uuuuuu]][+HH:MM|-HH:MM|Z].\"]}"
  
  output$experiment_confirmation <- renderUI({
    if (!is.null(experiment_reactive())) { 
      tagList(tags$p( paste0("Success. Experiment ID = ", experiment_reactive() ) ))
    } else {
      tagList(tags$p( paste0("Failure. ", experiment_reactive() ) ))
    }
  })
  
  
  ## Sample Resolving
  
  output$sample <- renderUI({
    req(input$confirm_experiment_final)
    req(experiment_reactive())
    fluidRow(
      column(8, tagList(tags$h4("Check Sample Names")) )
    )
  })
  
  output$run_sample_check <- renderUI({
    req(input$confirm_experiment_final)
    actionButton("run_sample_check","Run Sample Check")
  })
  
  sample_api_call_event_reactive <- eventReactive(input$run_sample_check, {
    df <- PlateInfo()['Sample Name']
    df <- df[which(!is.na(df$`Sample Name`)),]
    df$`Sample ID` <- NA
    df$`Sample Resolved?` <- ''
    df$`API Response` <- ''
    api_call <- get_or_create_biophysical_samples(experiment_name = input$experiment_name,
                                                  sample_df = df, url_sample_biophysical = url_sample_biophysical_test)
    return(api_call)
  })
  
  sample_resolve_table <- reactive({
    df <- PlateInfo()['Sample Name']
    df <- df[which(!is.na(df$`Sample Name`)),]
    df$`Sample ID` <- NA
    df$`Sample Resolved?` <- ''
    df$`API Response` <- ''
    if (input$run_sample_check==TRUE) {
      ## New implementation of Sample Biophysical API call.
      api_call <- sample_api_call_event_reactive()
      
      if (api_call$status_code==201) {
        for (n in 1:nrow(df)) {
          name <- as.character(df$`Sample Name`[n])
          df$`Sample ID`[n] <- content(api_call)$sample_ids[name]
          
          if (!is.na(df$`Sample ID`[[n]])) {
            df$`Sample Resolved?`[n] <- 'Yes'
            #df$`API Response`[n] <- api_call$status
          } else {
            df$`Sample Resolved?`[n] <- 'No'
            #df$`API Response`[n] <- api_call$status
          }
        }
      } else {
        df$`Sample Resolved?` <- 'No'
        df$`API Response` <- api_call$status_code
      }
      
      df$`Sample ID` <- as.integer(df$`Sample ID`)
    }
    colnames(df) <- c('Sample','Sample ID', 'Sample Resolved?', 'API Response')
    return(df)
  })
  
  
  output$sample_api_call <- renderText({
    req(input$run_sample_check)
    sample_api_call_event_reactive()
  })
  
  sample_resolve_table_only_resolved <- reactive({
    df <- sample_resolve_table()
    df <- df[df$`Sample Resolved?`=='Yes',]
    return(df)
  })
  
  output$resolve_samples <- renderTable({
    req(input$confirm_experiment_final)
    sample_resolve_table()
  })
  
  
  output$breakline5 <- renderUI({
    req(input$confirm_experiment_final)
    hr()
  })
  
  output$raw_results <- renderUI({
    req(input$run_sample_check)
    fluidRow(
      column(8, tagList(tags$h4("Upload Raw Results")) )
    )
  })
  
  output$result_table_upload1 <- renderTable({
    req(input$run_sample_check)
    if (onset_type_reactive()=='3hr') {
      df <- build_raw_results_3hr_dataframe(experiment_name = input$experiment_name, sample_id_table = sample_resolve_table_only_resolved(), 
                                            parsed_table_raw = final_valid_results_raw(), acq_date=acq_date())
      df$`absorbance OD` <- format(df$`absorbance OD`, digits = 3)
      
    } else {
      df <- build_raw_results_24hr_dataframe(experiment_name = input$experiment_name, sample_id_table = sample_resolve_table_only_resolved(), 
                                             parsed_table_raw = final_valid_results_raw(), acq_date=acq_date())
      
      df$`protein concentration [W/V]` <- format(df$`protein concentration [W/V]`, digits = 3)
      df$`calculated result` <- format(df$`calculated result`, digits = 3)
    }
    ## formatting:
    df$experiment_name <- NULL
    df$quadrant <- as.integer(df$quadrant)
    return(df)
  })
  
  raw_JSON_reactive <- reactive({
    if (onset_type_reactive()=='3hr') {
      transform_raw_results_3hr_toJSON(
        build_raw_results_3hr_dataframe(
          experiment_name = input$experiment_name, sample_id_table = sample_resolve_table_only_resolved(), 
          parsed_table_raw = final_valid_results_raw(), acq_date=acq_date()
        )
      )
    } else {
      transform_raw_results_24hr_toJSON(
        build_raw_results_24hr_dataframe(
          experiment_name = input$experiment_name, sample_id_table = sample_resolve_table_only_resolved(), 
          parsed_table_raw = final_valid_results_raw(), acq_date=acq_date()
        )
      )
    }
  })
  
  output$JSON_text_raw <- renderPrint({
    req(input$run_sample_check)
    raw_JSON_reactive()
  })
  
  
  output$upload_raw <- renderUI({
    req(input$run_sample_check)
    actionButton("upload_raw","Upload Raw Results")
  })
  
  call_api_raw_results <- eventReactive(input$upload_raw, {
    payload <- raw_JSON_reactive()
    p <- post_result(payload, url = url_results_protocol_test)
    return(p)
  })
  
  output$upload_raw_outcome <- renderText({
    req(input$upload_raw)
    r <- call_api_raw_results()
    if (class(r)=="list") {
      return(paste("Success! Number of results added to the database:", r$Result))
    } else if (class(r)=="response") {
      return(content(r, as="text"))
    } else {
      return(NULL)
    }
    #if (call_api_raw_results()$Result > 0) {
    #  paste(call_api_raw_results(), "Raw Results have now been uploaded to the Biologica database.")
    #} else {
    #  paste(call_api_raw_results(), "Raw Results not uploaded.")
    #}
  })
  
  output$breakline6 <- renderUI({
    req(input$run_sample_check)
    hr()
  })
  
  output$analyzed_results <- renderUI({
    req(input$run_sample_check)
    fluidRow(
      column(8, tagList(tags$h4("Upload Analyzed Results")) )
    )
  })
  
  output$result_table_upload2 <- renderTable({
    req(input$run_sample_check)
    if (onset_type_reactive()=='3hr') {
      df <- build_analyzed_results_3hr_dataframe(experiment_name = input$experiment_name, sample_id_table = sample_resolve_table_only_resolved(), 
                                                 analyzed_results_table = rmse_table_report(), acq_date=acq_date())
      
    } else {
      df <- build_analyzed_results_24hr_dataframe(experiment_name = input$experiment_name, sample_id_table = sample_resolve_table_only_resolved(), 
                                                  analyzed_results_table = rmse_table_report(), acq_date=acq_date())
    }
    ## formatting:
    df$experiment_name <- NULL
    return(df)
  })
  
  
  analyzed_JSON_reactive <- reactive({
    req(input$run_sample_check)
    if (onset_type_reactive()=='3hr') {
      transform_analyzed_results_3hr_toJSON(
        build_analyzed_results_3hr_dataframe(
          experiment_name = input$experiment_name, sample_id_table = sample_resolve_table_only_resolved(), 
          analyzed_results_table = rmse_table_report(), acq_date=acq_date()
        )
      )
    } else {
      transform_analyzed_results_24hr_toJSON(
        build_analyzed_results_24hr_dataframe(
          experiment_name = input$experiment_name, sample_id_table = sample_resolve_table_only_resolved(), 
          analyzed_results_table = rmse_table_report(), acq_date=acq_date()
        )
      )
    }
  })
  
  
  output$JSON_text_analyzed <- renderPrint({
    req(input$run_sample_check)
    analyzed_JSON_reactive()
  })
  
  output$upload_analyzed <- renderUI({
    req(input$run_sample_check)
    actionButton("upload_analyzed","Upload Analyzed Results")
  })
  
  call_api_analyzed_results <- eventReactive(input$upload_analyzed, {
    payload <- analyzed_JSON_reactive()
    p <- post_result(payload, url = url_results_protocol_test)
    return(p)
  })
  
  output$upload_analyzed_outcome <- renderText({
    req(input$upload_analyzed)
    r <- call_api_analyzed_results()
    if (class(r)=="list") {
      return(paste("Success! Number of results added to the database:", r$Result))
    } else if (class(r)=="response") {
      return(content(r, as="text"))
    } else {
      return(NULL)
    }
  })
  
  
  
  observeEvent(input$upload_raw, {
    shinyjs::disable("upload_raw")
  })
  observeEvent(input$upload_analyzed, {
    shinyjs::disable("upload_analyzed")
  })
  
  
  #--------------------------------------------#
  #           Benchling File Download          #
  #--------------------------------------------#
  
  # commit for testing on Aug 11
  
  output$benchling <- renderUI({
    req(input$file1)
    req(input$plateSize1)
    downloadButton('download_for_benchling', "Benchling Result File", icon=icon("arrow-circle-right"))
  })
  
  benchling_table <- reactive({
    return(build_benchling_upload_file(rmse_table_report(), info_report_tab(), onset_type_reactive()))
  })
  
  output$download_for_benchling <- downloadHandler(
    filename = function() {
      paste0(uploaded_file_name(),"_", onset_type_reactive(), "_BENCHLING_RESULTS_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write.xlsx2(benchling_table(), file, sheetName = "Benchling", append = FALSE, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui, server)