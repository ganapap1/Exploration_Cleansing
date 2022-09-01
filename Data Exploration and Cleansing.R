

##############################################################################
# Loading Required Liabitlies
##############################################################################
require(shiny)
require(shinydashboard)    # for Dashboard
require(shinyWidgets)      # for radio button widgets
# require(shinydashboardPlus)
require(shinyjs)           # to perform common useful JavaScript operations in Shiny apps
require(shinyBS)           # for bsTooltip function
require(shinyalert)        # for alert message very nice format
require(dplyr)             # select functions are covered in the require
require(plyr)              # empty() function is from this package
require(DT)                # for using %>% which works as a pipe in R code
require(ggplot2)
require(plotly)
require(scales) ## used to format date like only month or month and year
library(colorspace) # to generate Rainbow coloring function

if("package:shinydashboardPlus" %in% search()==TRUE){
  detach("package:shinydashboardPlus", unload = TRUE) 
}


if("package:psych" %in% search()==TRUE){
  detach("package:psych", unload = TRUE) 
}

##############################################################################
#Actionbutton style function default 30px and width 100px
##############################################################################
styleButtonBlue<- function(xheight="50px",xwidth="180px",xcolor='#4682B4',mfontsize='11px'){
  paste(
    "white-space: normal;
                        text-align:center;
                        color: #ffffff; 
                        background-color:",xcolor,";",
    "border-color: #ffffff;
                        border-width:2px;
                        height:",xheight,";
                        width:",xwidth,";
                        font-size:", mfontsize)
}


##############################################################################
#function for "NOT IN" for example column name not in the given list
##############################################################################
'%ni%' <- Negate('%in%')



##############################################################################
#Percentage formatting of numbers
##############################################################################
fnpercentZeroDigits <- function(x, digits = 0, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

fnpercent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}


##############################################################################
#functions to get right string and left string - these are facilitating functions
##############################################################################
fnRightString = function(x,n){
  substring(text = x,first = nchar(x)-n+1)
}
fnLeftString = function(x,n){
  substring(text = x,first = 1,last = n)
}


##########################################################################
#max frequency plus 8% to ylim in histogram
##########################################################################
fngetMaxFreq <-function(x){
  H <-hist(x, plot = FALSE)
  return(round(max(H$counts)*1.08,0))
}


##########################################################################
#function to calcuate Minimum and Maximum value
##########################################################################
custom_min <- function(x) {if (length(x)>0) min(x) else Inf}
custom_max <- function(x) {if (length(x)>0) max(x) else Inf}



####################################
# UI Header
####################################

header<- shinydashboardPlus::dashboardHeader(
  title = "Data Exploration and Cleansing",
  titleWidth = '1200px'
)



sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(id = "tabs"
  ) #sidebar menu
)

#https://www.javatpoint.com/r-classification#:~:text=In%20R%2C%20classification%20algorithms%20are%20broadly%20classified%20in,characteristics%20for%20finding%20to%20which%20class%20it%20belongs.

##############################################################################
# UI - DashboardBody
##############################################################################
body <- dashboardBody(
  
  ##############################################################
  # standard codes relating to side menu bar and hiding the same
  ##############################################################
  shinyjs::useShinyjs(),
  setShadow(class = "dropdown-menu"),
  #Remove icon which displays or hides the left sidebar in shinydashboard for a certain tabPanel
  tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),   #where you got: https://stackoverflow.com/questions/60895511/remove-icon-which-displays-or-hides-the-left-sidebar-in-shinydashboard-for-a-cer
  #So, adding this line to the ui seems to fix the misaligned header row, while still keeping the fixed column widths.
  tags$head(tags$style(".datatables .display {margin-left: 2;{margin-right: 2;}")),
  
  #scroll bar in box
  tags$head(
    tags$style(HTML("
                      .box { height: 90vh; overflow-y: auto; font-size:12px;font-style:normal;font-family: 
                      'Lucida Sans Unicode';text-font }" )
    )
  ),
  
  tags$head(
    tags$style(
      HTML(
        ".form-group {
            margin-bottom: 0 !important;
          }
        "
      )
    )
  ),
  
  
  fluidRow(
    uiOutput(outputId = 'mDataExplorationUI')
  )#column closure
)



ui <- shinydashboardPlus::dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)


server <- function(input, output, session) {
  ##############################################################################
  # These are initial codes to be part of Server
  ##############################################################################
  #to the top of server.R would increase the limit to 30MB.
  options(shiny.maxRequestSize=30*1024^2) 
  ## this option code is to Avoid the Message – `summarise()` has grouped output by ‘gr1’. You can override using the `.groups` argument.
  options(dplyr.summarise.inform = FALSE)
  
  # prevents printing scientific notation
  options(scipen=999)
  
  # #this is to hide right side bar
  shinyjs::addCssClass(selector = "body", class = "sidebar-collapse")
  onevent("mouseenter", "sidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "sidebar-collapse"))
  onevent("mouseleave", "sidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "sidebar-collapse"))
  
  
  
  ##########################################################################
  # Overview - Data Cleansing
  ##########################################################################
  
  observeEvent(input$mOverviewBtn,{
    showModal(
      mymodal(easyClose = T,
              size = 'l',
              column(style = "border: 4px double red;height: 550px;overflow-y: none;",
                     width = 12,
                     align = "center",
                     HTML(paste('<h4><b>',"Overview & Citations",'</b><h6>')),
                     tags$head(
                       tags$style(
                         paste0("#myboxOverView{color:black; font-size:10px; font-style:bold;overflow:auto;text-align: justify;margin: 5px 5px 5px 5px; 
                                            width: '100%';height: 475px;max-height: 475px; background: #ffffff;}")
                       )
                     ),
                     uiOutput("myboxOverView")
                     
              ), #column
              idcss = "test"
      )
    )
    
    
  })
  

  output$myboxOverView <- renderUI({
    column(
      width = 12,
      align='left',     
      tags$div(
        tags$p(
          useShinyjs(),
          HTML(paste('<h5><b>',"Overview:",'</b><h5>',
                     "Data Cleansing or Data Wrangling is an important early step in the data analytics process.", 
                     "This process is to fix incorrect, incomplete, duplicate or otherwise erroneous data in a data set.",
                     "It is very important to carry out this process. If you don’t, they’ll impact the results of your analysis.")), 
          HTML(paste('<h5>',"In this Shiny App demonstrats how to", '<br><h5>',
                     "1.  Remove irrelevant data eg url, HTML tag etc.",'<br>',
                     "2.  Fix structural errors eg improper column names, improper capitalization",'<br>',
                     "3.  Change data type eg to factor or to date or to numeric",'<br>',
                     "4.  Review and remove outliers using plotly box plot",'<br>',
                     "5.  Replace NA values and missing values in the data set",'<br>',
                     "6.  Review summary statistics using base R and also pastecs package",'<br>',
                     "7.  Correlation plot - multi variable",'<br>',
                     "8.  Review data using Box Plot, Histogram,QQ Plot and Density Plot ",'<br>',
                     "9.  Review Linearity using Linearity test and plot and also using Residuals",'<br>',
                     "10. Delete Duplicate rows and Delete rows having one or more NAs",'<br>',
                     "11. Create Dummy variables, Data Grouping, Normalization or Standardization",'<br>',
                     "12. Review Data Structure any stage",'<br>',
                     "13. Download Data as CSV or RDS files"
          ))
          
          
        )
      )
    ) #fluidrow closure
  })
  
  
  ###################################################################
  #  Reactive values are declared here 
  ###################################################################
  vmy <- reactiveValues(mydata=NULL,data_1=NULL)
  
  ###################################################################
  # Data Upload
  ###################################################################
  
  output$mDataExplorationUI <- renderUI({
    enable("mgetfileclick")
    column(
      width = 12,
      align='center',
      column(style = "border: 4px double red;height: 550px;overflow-y: none;background-color:#add8e6;color:black;",
             width = 4,
             align='center',
             HTML(paste('<h4><b>',"Data Upload / Structure / Rename",'</b><h6>')),
             box(
               width=12,
               height = '480px',
               tabsetPanel(
                 id = 'tabs',
                 tabPanel(
                   title = 'Data Upload',
                   br(),
                   br(),
                   fileInput("file",
                             label = "Select: csv,txt, xls, xlsx, rds ",
                             multiple = FALSE,
                             accept = c("text/csv/txt/Excel",
                                        "text/comma-separated-values,text/plain/excel",
                                        ".csv",".txt",".xls",".xlsx",".rds")),
                   
                   # Horizontal line ----
                   #tags$hr(),
                   column(
                     width = 5,
                     offset = 1,
                     align = "left",
                     fluidRow(
                       # Input: Checkbox if file has header ----
                       checkboxInput("header", "Header", TRUE),
                       
                       # Input: Select separator ----
                       radioButtons("sep", "Separator",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                    selected = ",")
                     )
                   ),
                   column(
                     width = 5,
                     offset = 0,
                     align = "left",
                     fluidRow(
                       br(),
                       br(),
                       # Input: Select quotes ----
                       radioButtons("quote", "Quote",
                                    choices = c(None = "",
                                                "Double Qot." = '"',
                                                "Single Qot." = "'"),
                                    selected = '"')
                     )
                   ),
                   
                   tags$hr(),
                   column(
                     width = 12,
                     align="center",
                     useShinyjs(),
                     br(),
                     actionButton(inputId = "mgetfileclick",label = "Get Data!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
                     textOutput(outputId = 'mfileimportmsg'),
                     br(),
                     actionButton(inputId = 'mOverviewBtn',label = "Overview!",style = styleButtonBlue(xheight = '45px',xwidth = '130px'))
                     
                   )
                 ), #tabpanel closure
                 tabPanel(
                   title = 'Cleansing',
                   DT::dataTableOutput("dt",height = '370px'),
                   tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}')),
                   useShinyjs(),
                   extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('dt_cell_clicked', null); }"),functions = c('foo','bar')),
                   br(),
                   prettyCheckbox(
                     inputId = "mMultiYN",
                     label = "Click to Select Multi Rows!",
                     value = FALSE,
                     status = "danger",
                     shape = "curve",
                     outline = TRUE
                   ),
                   textOutput("mselectedvariable")
                 ), #tabpanel closure
                 tabPanel(
                   title = 'Rename',
                   DT::dataTableOutput("mColRenameTbl",height = '370px'),
                   useShinyjs(),
                   extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('mColRenameTbl_cell_clicked', null); }"),functions = c('foo','bar')),
                   
                   actionButton(inputId = "mColRenameBtn",label = "Commit Rename",style = styleButtonBlue(xheight = '50px',xwidth = '100px'))
                 ) #tabpanel closure
               ) #tabSetpanel closure
             ) # box closure
      ),
      
      column(style = "border: 4px double red;height: 550px;overflow-y: none;background:#add8e6;color:black;",
             width = 8,
             align='center',
             # br(),br(),
             HTML(paste('<h4><b>',"Data Exploration & Cleansing",'</b><h6>')),
             box(
               width=6,
               height = '480px',
               
               # HTML(paste('<h4><b>',"Data Exploration & Cleansing",'</b><h6>')),
               
               HTML(paste('<h5><b>',"Select Vaiable by Type / Nature",'</b><h5>')),
               div(style = "margin-top:-10px"),
               actionButton(inputId = 'mConfirmTargetVarBtn',label = "Choose Target Variable",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#15997A')),
               actionButton(inputId = 'mbtndelete',label = "Delete Selected Variable(s)",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               actionButton(inputId = "mConfirmDateVarBtn", label = "Confirm Date Variable",style=styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               actionButton(inputId = "mConfirmYearVarBtn", label = "Confirm YEAR Variable, if any",style=styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#15997A')),
               
               
               HTML(paste('<h5><b>',"Modify Data Type",'</b><h5>')),
               div(style = "margin-top:-10px"),
               actionButton(inputId = 'mbtntoDate',label = "Convert as Date",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#15997A')),
               actionButton(inputId = 'mbtntoNumeric',label = "Convert to Numeric",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               actionButton(inputId = 'mbtntoFactor',label = "Convert to Factor",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               actionButton(inputId = 'mbtntoCharacter',label = "Convert to Character",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#15997A')),
               HTML(paste('<h5><b>',"Handling NAs / Missing Values",'</b><h5>')),
               div(style = "margin-top:-10px"),
               actionButton(inputId = 'mbtntoNAs',label = "Make Blanks to NAs",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#15997A')),
               actionButton(inputId = 'mbtnEmptyRows',label = "Remove Empty Rows",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               actionButton(inputId = 'mbtnNaOmit',label = "Delete All NA Rows",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               actionButton(inputId = "mRemoveDuplicateBtn",label = "Remove Duplicate Rows",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#15997A')),
               br(),br(),
               radioGroupButtons(
                 inputId = "mRadioNABtn",
                 label = "Replace NAs with",
                 choices = c("ZERO", "MEAN", "MEDIAN", "MODE"),
                 selected = character(0),
                 status = "primary",
                 checkIcon = list(
                   yes = icon("ok",
                              lib = "glyphicon"),
                   no = icon("xmark",
                             lib = "glyphicon"))
               ),
               
             ), #box closure
             box(
               width=6,
               height = '480px',
               
               HTML(paste('<h5><b>',"Create Dummies / Wide to Long",'<br>' ,"Normalization / Data Grouping",'</b><h5>')),
               div(style = "margin-top:-10px"),
               actionButton(inputId = 'mbtntoDummy',label = "Create Dummies",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               actionButton(inputId = "btnWidetoLong",label = "Wide to Long Transpose",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#016160')),
               actionButton(inputId = 'mNormalizeBtn',label = "Normalization",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#016160')),
               actionButton(inputId = "mGroupmyDataBtn",label = "Data Grouping",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               
               HTML(paste('<h5><b>',"Summary Statistics  & Visualization",'</b><h5>')),
               div(style = "margin-top:-10px"),
               actionButton(inputId = 'mCorrelationBtn',label = "Correlation Plot",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#016160')),
               actionButton(inputId = 'mMultiPlotBtn',label = "Multiple Plots",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               actionButton(inputId = 'mFixOutliersBtn',label = "Box Plot & Handling Outliers",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#6A6F37')),
               # actionButton(inputId = 'mBoxPlotBtn',label = "Box Plot",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#f78a77')),
               actionButton(inputId = 'mExploreStatBtn',label = "Summary Statistics Base Pkg",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#016160')),
               actionButton(inputId = 'mpastecsbtn',label = "Summary with pastecs Pkg",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#016160')),
               
               
               HTML(paste('<h5><b>',"Other Actions",'</b><h5>')),
               div(style = "margin-top:-10px"),
               actionButton(inputId = "mshowtableBtn",label = "Show Table",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#916db7')),
               actionButton(inputId = "mShowStrBtn",label = "Data Structure",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#542166')),
               downloadButton(outputId = "downloadCSVBtn", label = "Download Dataset CSV",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#542166')),
               downloadButton(outputId = "downloadRDSBtn", label = "Download Dataset RDS",style = styleButtonBlue(xheight = '45px',xwidth = '130px',xcolor = '#916db7'))
             ) #box closure
      )
    )
  })
  
  
  observeEvent(input$mgetfileclick,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    enable("mshowtableBtn")
    enable("mFixDependentVarBtn")
    
    
    #### file import code start
    ext <- tools::file_ext(input$file$name)
    
    if (ext != "csv" & ext !='rds' & ext != 'xlsx' & ext != 'xlsx'){
      shinyalert("Oops!", "valid files are csv, txt, excel and rds only", type = "error")
      return()
    }
    else if (ext == "rds"){
      vmy$mydata <- as.data.frame(readRDS(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "xls" || ext == 'xlsx'){
      vmy$mydata <- as.data.frame(readxl::read_excel(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "csv" || ext == 'txt'){
      tryCatch({            #avoid rscript showing error initially before execution,Skipping error in for-loop ::I got from this site; Thank you:https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
        
        vmy$mydata <- as.data.frame(
          read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote,
                   na = "***"
          )
        )
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    }
    
    #### create datatype df to use in cleansing phase
    fncreatedftype()
    fnCreateRenamedf()
    output$mfileimportmsg<-renderText({
      "Done - uploaded"
    })
    
    disable("mgetfileclick")
    
    output$mradioreplyMonthUI <- renderUI({
      if (length(input$file)==0){
        return()
      }
      options <- sort(unique(month(vmy$mydata$Report_Dt)))
      
      prettyCheckboxGroup(
        inputId = "radioreplyMonth",
        label = 'Select month to Filter Climate Chart',
        choices = options,
        selected = NULL,
        status  = "danger",
        inline = TRUE
        
      )
    })
  })
  
  
  
  ################### Upload data End #################################
  
  ########################################################
  # Selecting Target Variable
  ########################################################
  observeEvent(input$mConfirmTargetVarBtn, {
    if (length(input$file)==0){
      return()
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Confirm Target Variable as :",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(inputId = "mTargetvaryes", label = "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable first!" ),easyClose = TRUE
        )
      }
    )
  })
  
  
  ### If user say OK, then factor the selected rows
  observeEvent(input$mTargetvaryes, {
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    if (length(mselected)>1){
      shinyalert(title = 'Error',text = 'Only One can be Target Variable; you have multiple selections')
      return()
    }else{
      vmy$mdependvar <- mselected[1]
    }
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  })
  
  
  #############################################################################
  # Delete un-wanted columns or  columns that are not required, for example ID  
  #############################################################################
  
  fncreatedftype <- function(){
    df_types <- data.frame("col_types" = unlist(lapply(vmy$mydata, class)))
    df_types['Var_name'] <- rownames(df_types)
    row.names(df_types) <- seq(1,nrow(df_types))
    
    dd <- data.frame(NAs=sapply(vmy$mydata, function(x) sum(is.na(x))))
    dd['Var_name'] <- rownames(dd)
    row.names(dd) <- seq(1,nrow(dd))
    
    df_types <- plyr::join(x = df_types,y = dd,by= 'Var_name')
    
    vmy$df_types <- df_types
    vmy$df_types <-vmy$df_types %>% dplyr::select('Var_name', everything())      
    
  }
  
  
  output$dt <- DT::renderDataTable({
    DT::datatable(vmy$df_types,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = FALSE,
                  selection = list(mode = ifelse(input$mMultiYN==TRUE,'multiple',"single"), target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(scrollY = '275px',dom = 't',ordering=F, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff'});",   #::I got from this site; Thank you: JS from https://stackoverflow.com/questions/34009922/how-to-format-a-shiny-rendertable
                                   "}")
                  ) 
    )
    
  })
  
  ### delete selected column
  ### this is warning messge for deleting
  observeEvent(input$mbtndelete,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete variable:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    vmy$mydata <- dplyr::select_if(vmy$mydata,names(vmy$mydata)%ni% mselected)
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  })
  
  
  
  ############################################
  # Convert as Factor
  ############################################
  observeEvent(input$mbtntoFactor,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure Convert this variable as FActor:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("mconvertyes", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to convert to Factor!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then factor the selected rows
  observeEvent(input$mconvertyes, {
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    for (i in mselected){
      vmy$mydata[,i] <- as.factor(vmy$mydata[,i])  
    }
    
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  })
  
  
  
  ############################################
  # Convert as Character
  ############################################
  observeEvent(input$mbtntoCharacter,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure Convert this variable as Character:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("mCharacteryes", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to convert to Factor!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then Character the selected rows
  observeEvent(input$mCharacteryes, {
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    for (i in mselected){
      vmy$mydata[,i] <- as.character(vmy$mydata[,i])  
    }
    
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  })
  
  ########################################################
  # Convert to Date
  ########################################################
  observeEvent(input$mbtntoDate,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        clicked_list <- input$dt_rows_selected
        mselected <- vmy$df_types[clicked_list,1]
        n=which(colnames(vmy$mydata)== mselected[1])
        
        #where you got this sapply: https://stackoverflow.com/questions/18178451/is-there-a-way-to-check-if-a-column-is-a-date-in-r
        if (sapply(vmy$mydata[n], is.Date)== TRUE){
          n=which(colnames(vmy$mydata)== mselected[1])
          colnames(vmy$mydata)[n] <- 'Report_Dt'
          if (("Year" %in% colnames(vmy$mydata))==FALSE){
            vmy$mydata['Year']<-as.numeric(format(vmy$mydata$Report_Dt,'%Y'))
          }else{
            vmy$mydata$Year<-as.numeric(format(vmy$mydata$Report_Dt,'%Y'))
          }
          shinyalert(title = 'Info',text = 'Already in date format, hence only the name is update as Report_Dt')
          return()
        }else{
          modalDialog(
            title = "Warning",
            paste("Are you sure to Convert as Date:",vmy$df_types[input$dt_cell_clicked$row,1] ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("mDateyes", "Yes")
            ), easyClose = TRUE)
        }
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to convert to Date!" ),easyClose = TRUE
        )
      }
    )
    
  })
  
  
  ### If user say OK, then factor the selected rows
  observeEvent(input$mDateyes, {
    
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    for (i in mselected){
      vmy$mydata[,i] <- lubridate::dmy(vmy$mydata[,i])
    }
    
    if (("Year" %in% colnames(vmy$mydata))==FALSE){
      vmy$mydata['Year']<-as.numeric(format(vmy$mydata$Report_Dt,'%Y'))
    }else{
      vmy$mydata$Year<-as.numeric(format(vmy$mydata$Report_Dt,'%Y'))
    }
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
    
  })
  
  ########################################################
  # Convert to Numeric
  ########################################################
  observeEvent(input$mbtntoNumeric,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure to Convert as Numeric:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("mNumericyes", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to convert to Date!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then factor the selected rows
  observeEvent(input$mNumericyes, {
    
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    for (i in mselected){
      # vmy$mydata[,i] <- as.Date(vmy$mydata[,i], "%y/%m/%d")  
      
      # vmy$mydata[,i] <- as.Date(vmy$mydata[,i],'%d/%m/%Y')
      vmy$mydata[,i] <- as.numeric(vmy$mydata[,i])
    }
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
    
  })
  
  
  ############################################
  # Make All Blanks or missing values as NAs
  ############################################
  ##In the R programming language, missing values are usually represented by NA. For that reason, it is useful to convert all missing values to this NA format.
  ## sometimes NA values are shown within quote, meaning NA values are formatted as characters instead of real NA values which also to be converted as NA
  observeEvent(input$mbtntoNAs,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      modalDialog(
        title = "Warning",
        paste("Are you sure to update missing values or Blank cells with NAs?" ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("mBlank2NAsyes", "Yes")
        ), easyClose = TRUE)
      
    )
  })
  
  
  ### If user say OK, then Character the selected rows
  observeEvent(input$mBlank2NAsyes, {
    for (i in names(vmy$mydata)){
      vmy$mydata[vmy$mydata[[i]] == 'NA'] <- NA
      vmy$mydata[vmy$mydata[[i]] == ""]   <- NA  
    }
    
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  })
  
  
  
  ############################################
  # Delete all Empty Rows
  ############################################
  observeEvent(input$mbtnEmptyRows,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      modalDialog(
        title = "Warning",
        paste("Are you sure to delete all Empty Rows" ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("mEmptyRowsyes", "Yes")
        ), easyClose = TRUE)
      
    )
  })
  
  
  ### If user say OK, then Character the selected rows
  observeEvent(input$mEmptyRowsyes, {
    vmy$mydata <- vmy$mydata[rowSums(is.na(vmy$mydata)) != ncol(vmy$mydata), ]        # Drop empty rows
    
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  })
  
  
  
  
  ############################################
  # Delete all Empty Columns
  ############################################
  observeEvent(input$mbtnEmptyColumns,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      modalDialog(
        title = "Warning",
        paste("Are you sure to delete all Empty COLUMNS" ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("mEmptyColumnsyes", "Yes")
        ), easyClose = TRUE)
      
    )
  })
  
  
  ### If user say OK, then Character the selected rows
  observeEvent(input$mEmptyColumnsyes, {
    vmy$mydata <- vmy$mydata[ , colSums(is.na(vmy$mydata)) != nrow(vmy$mydata)]       # Drop empty columns
    
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  }) 
  
  
  
  
  ####################################################################################
  # Delete all row with one or more missing values or NA values using na.omit function
  ####################################################################################
  observeEvent(input$mbtnNaOmit,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      modalDialog(
        title = "Warning",
        paste("Are you sure to delete all Rows having one or more NAs" ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("mNAOmityes", "Yes")
        ), easyClose = TRUE)
      
    )
  })
  
  
  ### If user say OK, then Character the selected rows
  observeEvent(input$mNAOmityes, {
    vmy$mydata <- na.omit(vmy$mydata)
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  }) 
  
  
  
  
  
  
  
  ########################################################
  # Create Dummary variables from selected column variable
  ########################################################
  observeEvent(input$mbtntoDummy,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure create Dummy Variables:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("mDummyyes", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to create Dummies!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then factor the selected rows
  observeEvent(input$mDummyyes, {
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    library(fastDummies)
    #https://www.marsja.se/create-dummy-variables-in-r/
    for (i in mselected){
      vmy$mydata <- dummy_cols(
        vmy$mydata, 
        select_columns = i,
        remove_selected_columns = TRUE,
        ignore_na = TRUE)
    }
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  })
  

  
  ##############################################################################################
  # Date in Character form converted into Date form  for time series purpose with 31st Dec date
  ############################################################################################## 
  library(lubridate) #package to handle dates effectively 
  observeEvent(input$mConfirmDateVarBtn, {
    if (length(input$file)==0){
      return()
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        clicked_list <- input$dt_rows_selected
        mselected <- vmy$df_types[clicked_list,1]
        n=which(colnames(vmy$mydata)== mselected[1])
        
        if (lubridate::is.Date(vmy$mydata[n])== TRUE){
          n=which(colnames(vmy$mydata)== mselected[1])
          colnames(vmy$mydata)[n] <- 'Report_Dt'
          if (("Year" %in% colnames(vmy$mydata))==FALSE){
            vmy$mydata['Year']<-as.numeric(format(vmy$mydata$Report_Dt,'%Y'))
          }else{
            vmy$mydata$Year<-as.numeric(format(vmy$mydata$Report_Dt,'%Y'))
          }
          shinyalert(title = 'Info',text = 'Already in date class / format, hence only the name is update as Report_Dt')
          return()
        }else{
          modalDialog(
            title = "Warning",
            paste("Confirm Date Variable as :",mselected[1] ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(inputId = "mDatevaryes", label = "Yes")
            ), easyClose = TRUE)
        }
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable first!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then factor the selected rows
  observeEvent(input$mDatevaryes, {
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    if (length(mselected)>1){
      shinyalert(title = 'Error',text = 'Only One can be Target Variable; you have multiple selections')
      return()
    }else{
      n=which(colnames(vmy$mydata)== mselected[1])
      colnames(vmy$mydata)[n] <- 'Report_Dt'
      fncreatedftype()
      fnCreateRenamedf()
      disable("mConfirmDateVarBtn")
    }
    removeModal()
  })
  
  
  
  #####################################################################
  # Year converted Year into Date for time series purpose with 31st Dec date
  #####################################################################
  observeEvent(input$mConfirmYearVarBtn, {
    if (length(input$file)==0){
      return()
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Confirm Target Variable as :",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(inputId = "mYearvaryes", label = "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable first!" ),easyClose = TRUE
        )
      }
    )
  })
  
  
  ### If user say OK, then factor the selected rows
  observeEvent(input$mYearvaryes, {
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    if (length(mselected)>1){
      shinyalert(title = 'Error',text = 'Only One can be Target Variable; you have multiple selections')
      return()
    }else{
      if (("Report_Dt" %in% colnames(vmy$mydata))==FALSE){
        vmy$mydata['Report_Dt'] <- as.character()
        for (i in (1:nrow(vmy$mydata))){
          vmy$mydata$Report_Dt[i] <- paste0("31/","12/",vmy$mydata[i,mselected[1]])
        }
        vmy$mydata <-vmy$mydata %>% dplyr::select('Report_Dt', everything())
      }
      
      fncreatedftype()
      fnCreateRenamedf()
      disable("mConfirmYearVarBtn")
    }
    removeModal()
  })
  
  
  
  ########################################################
  # Data Normalization / Standardization
  #Standatdization of values in dataset
  #To standardize a dataset means to scale all of the values in the dataset such that the mean value is 0 and the standard deviation is 1.
  ########################################################
  observeEvent(input$mNormalizeBtn,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      modalDialog(
        title = "Warning",
        paste("Do you want to Normalize all Numeric Columns ?:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("mNormalizeyes", "Yes")
        ), easyClose = TRUE),
    )
    
  })
  
  
  ### If user say OK, then factor the selected rows
  library(dplyr)
  observeEvent(input$mNormalizeyes, {
    aaa <- c()
    aaa <- names(dplyr::select_if(vmy$mydata,is.factor))
    aaa <- c(aaa,names(dplyr::select_if(vmy$mydata,is.character)))
    new_df = subset(vmy$mydata, select = !(names(vmy$mydata) %in% aaa))
    set.seed(1)
    nnn <- names(dplyr::select_if(new_df,is.numeric))
    temp <- vmy$mydata %>% mutate_at(c(nnn), ~(scale(.) %>% as.vector))
    vmy$mydata <- temp
    removeModal()
    fncreatedftype()
    click(id = 'mshowtableBtn')
    
    fncreatedftype()
    fnCreateRenamedf()
    disable('mNormalizeBtn')
    
    removeModal()
  })
  
  
  
  
  
  #####################################################################################
  # Multiple Box plots in one screen
  #####################################################################################
  
  observeEvent(input$mBoxPlotBtn,{
    if (length(input$file)==0){
      return()
    }
    fnRadioButton(mlabel = 'Select Variable to Remove from Chart',mmulti = TRUE)
    showModal(
      mymodal(easyClose = T,
              size = 'l',
              column(style = "border: 4px double red;height: 550px;overflow-y: none;",
                     width = 12,
                     align = "center",
                     x <- uiOutput('radio_BtnsCorrelation'),
                     
                     HTML(paste('<h4><b>',"Review Outliers with Box Plot",'</b><h6>')),
                     plotOutput(outputId = 'mboxplot',width = '100%',height = '400px')
                     
              ), #column
              idcss = "test"
      )
    )
    
    
  })
  
  
  output$mboxplot <- renderPlot({
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    temp4 <- dplyr::select_if(vmy$mydata,is.numeric)
    mcolnames <- colnames(temp4) %ni% c(input$mradioPlotVarable)
    
    temp4 <-  temp4[,mcolnames]
    
    n <- ncol(temp4)
    r <- ifelse(n<=8,2,3)
    c <- ceiling(n/r)
    par(mfrow=c(r,c))    #get in 3 rows and 3 columns (first one is row)
    
    # #par("mar") (bottom, left, top, right) in lines
    # #par(mar=rep(0,4))
    for(i in 1:n) {
      boxplot(
        temp4[,i],
        main = colnames(temp4)[i],
        at = c(i),
        height ="100%",
        xlab = NULL,
        ylab = NULL,
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE,
        outline = TRUE,
        axes = TRUE,
        staplewex = 1
      )
    }
    
    
  }) 
  
  
  
  
  ##########################################################################
  # Codes to show the structure of the dataset for all columns variables
  ##########################################################################
  observeEvent(input$mShowStrBtn,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      modalDialog(
        title = "Info",
        verbatimTextOutput(outputId = 'mShowStrTxt'),
        easyClose = TRUE)
      
    )
  }) 
  
  output$mShowStrTxt <- renderPrint({
    str(vmy$mydata)
  })
  
  
  
  ##########################################################################
  # Code to download dataset as csv and rds
  ##########################################################################
  output$downloadCSVBtn<- downloadHandler(
    filename = function() {
      paste("mydatadf", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vmy$mydata), file, row.names = FALSE)
    }
  )
  
  ### can download the table in RDS
  output$downloadRDSBtn <- downloadHandler(
    filename = function() {
      paste("mydatadf", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(data.frame(vmy$mydatas), file)
    }
  )
  
  ##########################################################################
  # Display Dataset
  ##########################################################################
  observeEvent(input$mshowtableBtn,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      mymodal(easyClose = T,
              size = 'l',
              column(style = "border: 4px double red;height: 550px;overflow-y: none;",
                     width = 12,
                     align = "center",
                     
                     HTML(paste('<h4><b>',"Dataset Table",'</b><h6>')),
                     tags$head(
                       tags$style(
                         paste0("#mdatatable{color:black; font-size:11px; font-style:bold;overflow-y:scroll;font-family: 'Arial';margin: 5px;
                                            width: '100%';height: 475px;max-height: 475px; background: #ffffff;text-align: left;}")
                       )
                     ),
                     column(width = 12, DT::dataTableOutput('mdatatable', height = '475px',width = '100%')),
                     
                     tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}'))
                     
              ), #column
              # footer = tagList(
              #   modalButton("Cancel"),
              #   actionButton("ok", "OK")
              # ),
              idcss = "test"
      )
    )
    
  })
  
  
  
  output$mdatatable <- DT::renderDataTable({
    DT::datatable(vmy$mydata,
                  rownames = FALSE,
                  editable = FALSE,
                  selection = list(mode = "single", selected = c(1), target = 'row'),
                  #fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(
                    lengthMenu = list(c(15, 25, 50,-1), c('15', '25','50' ,'All')),
                    paging = TRUE,
                    lenthChange=TRUE,
                    searching = FALSE,
                    fixedColumns = FALSE,
                    autoWidth = FALSE,
                    ordering = FALSE,
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      paste0("$(this.api().table().container()).css({'font-size': '", "12px", "'});"),
                      "}")
                  ),
                  
                  class ='cell-border stripe compact white-space: nowrap', #where you got this multiple classes: https://rstudio.github.io/DT/
    )
  })
  
  
  
  
  
  
  
  #######################################################
  #custom Modal Function Interesting
  #######################################################
  mymodal <- function (..., title = NULL, footer = modalButton("Dismiss"), 
                       size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE, idcss = "") 
  {
    size <- match.arg(size)
    cls <- if (fade) 
      "modal fade"
    else "modal"
    div(id = "shiny-modal", class = cls, tabindex = "-1", `data-backdrop` = if (!easyClose) 
      "static", `data-keyboard` = if (!easyClose) 
        "false", div(class = paste("modal-dialog", idcss), class = switch(size, 
                                                                          s = "modal-sm", 
                                                                          m = NULL, 
                                                                          l = "modal-lg"), 
                     div(class = "modal-content", 
                         if (!is.null(title)) 
                           div(class = "modal-header", tags$h4(class = "modal-title", 
                                                               title)
                           ), 
                         div(class = "modal-body", ...), 
                         if (!is.null(footer)) 
                           div(class = "modal-footer", footer))
        ), 
      tags$script("$('#shiny-modal').modal().focus();"))
  }
  
  
  ##########################################################################
  # Statistic using Base R
  ##########################################################################
  observeEvent(input$mExploreStatBtn,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      mymodal(easyClose = T,
              size = 'l',
              column(style = "border: 4px double red;height: 550px;overflow-y: auto;",
                     width = 12,
                     align = "center",
                     HTML(paste('<h4><b>','Summary Statistics','</b><h5>')),
                     tags$head(tags$style("#mSummaryStat{color:black; font-size:13px; font-style:normal;    #formatting verbatimtextoutput: verbatimTextOutput sizing and scrollable: where you got:https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193/2
overflow-y:scroll; height: 450px; background: #ffffcd;}")),
                     br(),
                     verbatimTextOutput(outputId = "mSummaryStat")
              ), #column
              idcss = "test"
      )
    )
  })
  
  
  output$mSummaryStat <- renderPrint({
    summary(vmy$mydata)
  })
  
  
  
  ##########################################################################
  # Statistic using pastecs Package
  ##########################################################################
  library(pastecs)
  library(kableExtra)
  observeEvent(input$mpastecsbtn,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      mymodal(easyClose = T,
              size = 'l',
              column(style = "border: 4px double red;height: 550px;overflow-y: none;",
                     width = 12,
                     align = "center",
                     HTML(paste('<h4><b>','Summary Statistics using pastecs Package','</b><h5>')),
                     tags$head(tags$style("#mDescriptiveStat{color:black; font-size:14px; font-style:normal;    #formatting verbatimtextoutput: verbatimTextOutput sizing and scrollable: where you got:https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193/2
overflow-y:scroll; height: 450px; background: #ffffcd;}")),
                     br(),
                     htmlOutput("mDescriptiveStat")
              ), #column
              idcss = "test"
      )
    )
    
    temp5 <- dplyr::select_if(vmy$mydata,is.numeric)
    temp6 <- data.frame(stat.desc(temp5))
    temp6 <- temp6 %>% mutate_if(is.numeric, round, digits = 2)
    n<-ncol(temp6)
    output$mDescriptiveStat <- renderUI({
      HTML( kbl(temp6, escape = FALSE,align=c(rep('c',times=n)),   #align center all columns including header in table align=c(rep('c',times=n)) https://stackoverflow.com/questions/41365502/aligning-columns-with-knitr-kable-function
                caption =NULL,linesep ="",
                # linesep = "\\addlinespace",
                table.attr = "style='width:30%;'")%>%
              kable_styling(font_size = 14, position = "center", html_font = "Cambria",fixed_thead = TRUE) %>%
              kable_paper("striped", full_width = FALSE) %>%
              kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
              column_spec(1, color = "red")%>%  #for formatting first column in table
              row_spec(0:nrow(temp6), angle = 360,bold=TRUE, color = "red",background = '#ffffd4',font_size = 14)%>%  #for formatting table header
              column_spec(1, bold = FALSE,width = "1.3in",color='red') %>%
              column_spec((1:n+1), bold = FALSE,border_left = TRUE,border_right = TRUE,width = "1.3in",color='black') %>%
              scroll_box(width = "100%", height = "425px")
      )
      
    })
    
    
  })
  
  
  
  
  # library(DataExplorer)
  # observeEvent(input$mExploreDataBtn2,{
  #   DataExplorer::create_report(vmy$mydata)
  # })
  
  
  
  
  
  
  ##########################################################################
  # Box plot using Plotly
  ##########################################################################
  observeEvent(input$mFixOutliersBtn,{
    if (length(input$file)==0){
      return()
    }
    fnRadioButton(mlabel = 'Select Variable to Remove from Chart',mmulti = FALSE)
    showModal(
      mymodal(easyClose = T,
              size = 'l',
              column(style = "border: 4px double red;height: 550px;overflow-y: none;",
                     width = 12,
                     align = "center",
                     x <- uiOutput('radio_BtnsCorrelation'),
                     
                     HTML(paste('<h4><b>',"Review Outliers using Plotly Box Plot",'</b><h6>')),
                     plotlyOutput(outputId = 'mPlotlyOutlierPlot',height = '275px',width = '100%'),
                     
                     uiOutput(outputId = 'msliderRangeInputUI')
              ), #column
              idcss = "test"
      )
    )
    
  })
  
  output$mPlotlyOutlierPlot <- renderPlotly({
    
    fig <- plot_ly(x = vmy$mydata[,input$mradioPlotVarable],type = "box", quartilemethod="linear",
                   name=input$mradioPlotVarable,
                   notched = TRUE,
                   jitter = 0.3, 
                   pointpos = -1.8, 
                   boxpoints = 'outliers',
                   notched = TRUE,
                   color = I('orange'),
                   marker = list(color = 'red'),
                   line = list(color = 'red')
                   
    ) %>% layout(showlegend = FALSE, plot_bgcolor='#e5ecf6',
                 yaxis = list(tickangle=-90, tickfont = list(family='Rockwell', color='crimson', size=14)),
                 xaxis = list(tickangle=0, tickfont = list(family='Rockwell', color='crimson', size=14)))
    fig 
    
  })
  
  observeEvent(input$mradioPlotVarable,{
    if (length(input$file)==0){
      return()
    }
    
    
    mMin <- quantile(vmy$mydata[,input$mradioPlotVarable],probs = 0.25,na.rm = TRUE)[[1]] 
    mMax <- quantile(vmy$mydata[,input$mradioPlotVarable],probs = 0.75,na.rm = TRUE)[[1]] 
    
    mIqr <- mMax - mMin
    Lower_fence <- mMin - (1.5*mIqr)
    Upper_fence <- mMax + (1.5*mIqr)
    
    mChoice <- c(
      Lower_fence,
      quantile(vmy$mydata[,input$mradioPlotVarable],probs = 0,na.rm = TRUE)[[1]],
      quantile(vmy$mydata[,input$mradioPlotVarable],probs = 0.25,na.rm = TRUE)[[1]],
      quantile(vmy$mydata[,input$mradioPlotVarable],probs = 0.5,na.rm = TRUE)[[1]],
      quantile(vmy$mydata[,input$mradioPlotVarable],probs = 0.75,na.rm = TRUE)[[1]],
      quantile(vmy$mydata[,input$mradioPlotVarable],probs = 1,na.rm = TRUE)[[1]],
      Upper_fence)
    mChoice <- sort(mChoice)
    output$msliderRangeInputUI <- renderUI({
      column(
        br(),
        width = 12,
        tags$head(tags$style("#msliderRangeInput{color:black; font-size:12px; font-style:normal;    #formatting verbatimtextoutput: verbatimTextOutput sizing and scrollable: where you got:https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193/2
overflow-y:none; height: 75px;width:600px; background: #ffffcd;}")),
        HTML(paste('<h5>',"Select Less than and More than to replace Ourliers",'<h5>')),
        sliderIn <- sliderTextInput(inputId = 'msliderRangeInput',label = NULL,
                                    choices = mChoice,selected = c(min(mChoice),max(mChoice)),
                                    grid=TRUE,width = '600px' ),
        
        actionButton(inputId = 'mReplceOutlierBtn',label = "Click to Go!",style = styleButtonBlue(xheight = '45px',xwidth = '130px'))
      )
    })
    
    
  })
  
  
  
  observeEvent(input$mReplceOutlierBtn,{
    if (length(input$file)==0){
      return()
    }
    vmy$mydata[,input$mradioPlotVarable] <-ifelse( vmy$mydata[,input$mradioPlotVarable] >= input$msliderRangeInput[2],input$msliderRangeInput[2], vmy$mydata[,input$mradioPlotVarable] )
    vmy$mydata[,input$mradioPlotVarable] <-ifelse( vmy$mydata[,input$mradioPlotVarable] <= input$msliderRangeInput[1],input$msliderRangeInput[1], vmy$mydata[,input$mradioPlotVarable] )
  })
  
  
  # output$mOutlierVarFreq <- renderPrint({
  #   as.data.frame(table(vmy$mydata[input$mradioPlotVarable]))
  # })
  
  
  
  #####################################################
  # Radio Button and also options to pass parameters
  #####################################################
  fnRadioButton <- function(mlabel="Select Variable",mmulti=TRUE,mOnlyNumeric=TRUE){
    output$radio_BtnsCorrelation <- renderUI({
      if (length(input$file)==0){
        return()
      }
      if (mOnlyNumeric==TRUE){
        options <- colnames(dplyr::select_if(vmy$mydata,is.numeric))
      }else{
        options <- colnames(vmy$mydata)
      }
      

      
      pickerInput(
        inputId = "mradioPlotVarable",
        label = mlabel,
        choices = options,
        multiple = mmulti,
        width = '300px',
        selected = options[1],
        options = list('actions-box' = TRUE)
      )
    })
    
  }
  
  
  ##########################################################################
  # Correlation Pair-Panel Plot
  ##########################################################################
  observeEvent(input$mCorrelationBtn,{
    if (length(input$file)==0){
      return()
    }
    fnRadioButton(mlabel = 'Select Variable to Remove from Chart',mmulti = TRUE)
    showModal(
      mymodal(easyClose = T,
              size = 'l',
              column(style = "border: 4px double red;height: 550px;overflow-y: none;",
                     width = 12,
                     align = "center",
                     x <- uiOutput('radio_BtnsCorrelation'),
                     
                     HTML(paste('<h4><b>',"Correlation Pair-Panel Plot",'</b><h6>')),
                     plotOutput('mcorrelationpairpanel',height = '425px',width = '100%')
                     
              ), #column
              idcss = "test"
      )
    )
    
    
  })
  
  
  
  output$mcorrelationpairpanel <- renderPlot({
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse and select dataset ...!", type = "error")
      return()
    }
    
    temp <- dplyr::select_if(vmy$mydata,is.numeric)
    # library(psych)
    mcolnames <- colnames(temp) %ni% c(input$mradioPlotVarable)
      psych::pairs.panels(temp[,mcolnames],
                          smooth = TRUE,      # If TRUE, draws loess smooths
                          scale = FALSE,      # If TRUE, scales the correlation text font
                          density = FALSE,    # If TRUE, adds density plots and histograms
                          ellipses = TRUE,    # If TRUE, draws ellipses
                          method = "pearson", # Correlation method (also "spearman" or "kendall")
                          pch = 21,           # pch symbol
                          lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
                          cor = TRUE,         # If TRUE, reports correlations
                          jiggle = FALSE,     # If TRUE, data points are jittered
                          factor = 2,         # Jittering factor
                          smoother = FALSE,    # If TRUE, then smooth.scatter the data points -- slow but pretty with lots of subjects
                          digits = 2,         # the number of digits to show
                          cex.cor = 0.8,       # If this is specified, this will change the size of the text in the correlations. 
                          hist.col = 4,       # Histograms color
                          gap = 0,
                          stars = TRUE)       # If TRUE, adds significance level with stars

  })
  
  
  
  
  
  
  
  
  ##########################################################################
  # Code to download dataset as csv and rds
  ##########################################################################
  output$downloadCSVBtn<- downloadHandler(
    filename = function() {
      paste("mydatadf", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vmy$mydata[input[["mdatatable_rows_all"]], ]), file, row.names = FALSE)
    }
  )
  
  ### can download the table in RDS
  output$downloadRDSBtn <- downloadHandler(
    filename = function() {
      paste("mydatadf", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(data.frame(vmy$mydata[input[["mdatatable_rows_all"]], ]), file)
    }
  )
  
  
  observeEvent(input$mRadioNABtn,{
    if (length(input$file)==0){
      return()
    }
    
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure You want to update NA values in :",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("mNAsok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
    
  })
  
  
  
  library(tidyverse)
  ### If user say OK, then delete the selected rows
  # Create the function.
  getmode <- function(v) {
    uniqv <- na.omit(unique(v))
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  observeEvent(input$mNAsok, {
    nn <- nrow(vmy$mydata)
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    
    if (input$mRadioNABtn == "ZERO"){
      for (i in mselected){
        mreplaceval <- 0
        for (r in (1:nn)){
          vmy$mydata[r,i][is.na(vmy$mydata[r,i])] <- mreplaceval
        }
      }
    }else if (input$mRadioNABtn == "MEAN"){
      for (i in mselected){
        mreplaceval <- mean(vmy$mydata[,i],na.rm	= TRUE )
        for (r in (1:nn)){
          vmy$mydata[r,i][is.na(vmy$mydata[r,i])] <- mreplaceval
        }
      }
    }else if (input$mRadioNABtn == "MEDIAN"){
      for (i in mselected){
        mreplaceval <- median(vmy$mydata[,i],na.rm	= TRUE )
        for (r in (1:nn)){
          vmy$mydata[r,i][is.na(vmy$mydata[r,i])] <- mreplaceval
        }
      }
    }else if (input$mRadioNABtn == "MODE"){
      for (i in mselected){
        mreplaceval <- getmode(vmy$mydata[,i] )
        for (r in (1:nn)){
          vmy$mydata[r,i][is.na(vmy$mydata[r,i])] <- mreplaceval
        }
      }
    }
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
    updateRadioGroupButtons(session,inputId = 'mRadioNABtn', selected = character(0))
  })
  
  ######################################################################
  # Box plot , Histogram QQ plot and Density Plot
  #######################################################################
  
  
  observeEvent(input$mMultiPlotBtn,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      mymodal(easyClose = T,
              size = 'l',
              column(style = "border: 4px double red;height: 550px;overflow-y: none;",
                     width = 12,
                     align = "center",
                     column(width = 4,
                            x <- uiOutput('mpickerMultiPlotUI')
                            ),
                     column(width =4,
                            HTML(paste('<h5><b>',"Multiple Plots",'<br>','You get only Bar Plot if Character column is selected','</b><h5>')),
                            
                     ),
                     column(width = 4,
                            selectInput(
                              inputId = "mplottype",
                              label = 'Select Plot Type',
                              choices = c("Box Plot", "Hist & Box","Bar Plot","QQ Plot", "Density Plot","Linearity Test","All in one Linearity Test"),
                              selected = "Box Plot")
                     ),
                     br(),
                     column(
                       width = 12,
                       align='center',
                       plotOutput('mmultiplot',height = '400px',width = '100%')
                     )
              ), #column
              idcss = "test"
      )
    )
    
    
  })
  
  
  output$mpickerMultiPlotUI <- renderUI({
    mchoiceNumeric <-  names(dplyr::select_if(vmy$mydata,is.numeric))
    mchoiceNOTNumeric <-  names(dplyr::select_if(vmy$mydata,negate(is.numeric)))
    
    pickerInput(
      inputId = "mpickerMultiPlot",
      label = "Select Variable",
      choices = list(
        Numeric = c(mchoiceNumeric),
        Non_Numeric = c(mchoiceNOTNumeric)
        ),
      selected = NULL,
      multiple = FALSE,
      width = '100%'
      
    )
  })
  
  
  
  library(car)
  output$mmultiplot <-renderPlot({
    
    if (length(input$mpickerMultiPlot)==0){
      return()
    }
    
    
    df2 <- data.frame(vmy$mydata[,input$mpickerMultiPlot])
    df2<-data.frame(df2[!is.na(df2[[1]]),])
    names(df2)<- c(input$mpickerMultiPlot)

    par(mfrow = c(1, 1))
    if (is.numeric(df2[ ,1])  == FALSE){
      updatePickerInput(session,inputId = 'mplottype',selected = "Bar Plot")
      # Outside bars
      df<- data.frame(table(df2[ ,1]))
      ggplot(data=df, aes(x=Var1, y=Freq)) +
        geom_bar(stat="identity", fill="#87CEFA")+
        xlab(input$mpickerMultiPlot) +
        ylab("Frequency") +
        geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
        theme_minimal()
    }else if (input$mplottype=="Box Plot"){
             boxplot(x =  df2[ ,1],
                                   main = NULL,
                                   horizontal=TRUE,
                                   labels = TRUE,
                                   col = "#87CEFA",
                                   border = "black",
                                   # col = "orange",
                                   # border = "brown",
                                   notch = TRUE)
             points(pch = 16,cex = 1.5,mean(df2[ ,1],na.rm = TRUE),col="red")
            
    }else if(input$mplottype=="Hist & Box"){
      layout(p = matrix(c(1,2),nrow=2,ncol = 1, byrow=TRUE),  height = c(8,3))
      par(mar=c(0, 3.1, 1.1, 2.1)) # #par("mar") (bottom, left, top, right) in lines
      
      hist(df2[ ,1],
           main = NULL,
           labels = TRUE,
           col = "#87CEFA",
           border = "white",
           breaks = 10,
           ylim = c(0,fngetMaxFreq(df2[ ,1])),
           xlab = paste("Bin of ",input$mpickerMultiPlot))+geom_point()+geom_smooth()+coord_cartesian()%>%
        abline(v = mean(df2[ ,1], na.rm = T),
               col = "red",
               lwd = 2)%>%
        abline(v = median(df2[ ,1], na.rm = T),
               col = "black",
               lwd = 2)
      
      boxplot(df2[ ,1],
              main = NULL,
              xlab = NULL,
              ylab = NULL,
              col = "#CDE2B8",
              border = "black",
              horizontal = TRUE,
              notch = FALSE,
              outline = TRUE,
              axes = TRUE,
              staplewex = 1
      )+coord_cartesian()+
        text(x=quantile(df2[ ,1]),labels=quantile(df2[ ,1]),y=1.25,cex = 0.90)
        points(mean(df2[ ,1],na.rm = TRUE), col = "red", pch = 19, cex = 1.5)
    }else if(input$mplottype=="QQ Plot"){
      qqPlot(unlist(vmy$mydata[input$mpickerMultiPlot]))
    }else if(input$mplottype=="Density Plot"){
      ggplot(vmy$mydata, aes(x=df2[ ,1])) +
        geom_density(fill = "#87CEFA")+xlab(input$mpickerMultiPlot)+
        geom_vline(data=vmy$mydata, aes(xintercept = mean(df2[ ,1])), colour='red') +
        geom_vline(data=vmy$mydata, aes(xintercept = median(df2[ ,1])), colour='black')
    }else if(input$mplottype=="Linearity Test"){
      fnSimpleLineartyTest()
    }else if(input$mplottype=="All in one Linearity Test"){
      fnmAllInOneLinearityPlot()
    }else if(input$mplottype=="Box Plot"){
      
    }
             
  
  }) 
  
  
  
  ############################
  #Linearty test using ggplot 
  ############################
  fnSimpleLineartyTest <- function(){
    if (length(vmy$mdependvar)==0){
      shinyalert(title = 'Error',text = 'Target Variable not identified or selected')
      return()
    }
    mcortest <- cor.test(x = vmy$mydata[,input$mpickerMultiPlot],y = vmy$mydata[,vmy$mdependvar],method='pearson')
    mcorcoff <- round(mcortest$estimate,2)
    mtstat   <- round(mcortest$statistic,2)
    mtstatp  <- round(mcortest$p.value,3)
    mtext    <- paste("Corr Coeff:",mcorcoff,";p-value of t-stat:",mtstatp,". ",'\n',
                      "If p-value is less than 0.05, Null Hypothesis will be rejected and there exists Linearity;",
                      "Otherwise no Linearity")
    ggplot(vmy$mydata, aes(x = vmy$mydata[,input$mpickerMultiPlot],y = vmy$mydata[,vmy$mdependvar])) +
      geom_point(color='black',size=.4,method = 'loess',formula = y ~ x) +
      geom_smooth(method = lm, se = TRUE,color='red') +
      labs(title=mtext, y=vmy$mdependvar,
           x=names(vmy$mydata[input$mpickerMultiPlot]))+
      theme_gray(base_size = 12)+
      theme(
        legend.position  = 'none',
        text = element_text(color = "black", size = 12,face = "italic"),complete = TRUE
      )
  }
  
  
  
  ############################################################
  #Testing of assumptions ALL in One using grid and autoplot
  ############################################################
  library(gridExtra)
  library(ggfortify)
  fnmAllInOneLinearityPlot <- function(){
    if (length(vmy$mdependvar)==0){
      shinyalert(title = 'Error',text = 'Target Variable not identified or selected')
      return()
    }
    theme_set(
      theme_grey(12)
    )
    
    par(mfrow=c(2,3)) #get in 2 rows and 3 columns (first one is row)
    f2 <- as.formula(paste(paste( text=vmy$mdependvar,"~"), input$mpickerMultiPlot))
    model <- lm(f2,  data=vmy$mydata)
    
    p<- autoplot(model, which = 1:6, ncol = 3) +
      theme(
        legend.position  = 'none',
        text = element_text(color = "black", size = 11,face = "italic"),complete = TRUE
        # panel.background = element_rect(fill='#363636'), plot.background = element_rect(fill='#363636')
      )
    p <- p %>% gridExtra::grid.arrange(grobs = p@plots, top = "All In one Test of Assumptions of Linear Regression")
    p  
  }
  
  
  
  
  
  
  
  # library(car)
  #   output$mmultiplot <-renderPlot({
  #     
  #     if (length(input$mpickerMultiPlot)==0){
  #       return()
  #     }
  #     par(mfrow = c(1, 1))
  #     tryCatch({
  #       
  #       switch(input$mplottype,
  #              "Box Plot" =  boxplot(x = vmy$mydata[ ,input$mpickerMultiPlot],
  #                                    main = NULL,
  #                                    horizontal=TRUE,
  #                                    labels = TRUE,
  #                                    col = "#87CEFA",
  #                                    border = "black",
  #                                    # col = "orange",
  #                                    # border = "brown",
  #                                    notch = TRUE)+
  #                points(pch = 16,cex = 1.5,mean(vmy$mydata[ ,input$mpickerMultiPlot]),col="red"),
  #              
  #              "Hist & Box / Bar"=
  #                if (is.numeric(vmy$mydata[,input$mpickerMultiPlot])  == FALSE){
  #                  # Outside bars
  #                  df<- data.frame(table(vmy$mydata[,input$mpickerMultiPlot]))
  #                  ggplot(data=df, aes(x=Var1, y=Freq)) +
  #                    geom_bar(stat="identity", fill="#87CEFA")+
  #                    xlab(input$mpickerMultiPlot) +
  #                    ylab("Frequency") +
  #                    geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  #                    theme_minimal()
  #                }else
  #                {
  #                  # # Layout to split the screen
  #                  
  #                  layout(p = matrix(c(1,2),nrow=2,ncol = 1, byrow=TRUE),  height = c(8,3))
  #                  par(mar=c(0, 3.1, 1.1, 2.1)) # #par("mar") (bottom, left, top, right) in lines
  #                  
  #                  hist(vmy$mydata[,input$mpickerMultiPlot],
  #                       main = NULL,
  #                       labels = TRUE,
  #                       col = "#87CEFA",
  #                       border = "white",
  #                       breaks = 10,
  #                       ylim = c(0,fngetMaxFreq(vmy$mydata[[input$mpickerMultiPlot]])),
  #                       xlab = paste("Bin of ",input$mpickerMultiPlot))+geom_point()+geom_smooth()+coord_cartesian()%>%
  #                    abline(v = mean(vmy$mydata[,input$mpickerMultiPlot], na.rm = T),
  #                           col = "red",
  #                           lwd = 2)%>%
  #                    abline(v = median(vmy$mydata[,input$mpickerMultiPlot], na.rm = T),
  #                           col = "black",
  #                           lwd = 2)
  #                  
  #                  boxplot(vmy$mydata[,input$mpickerMultiPlot],
  #                          main = NULL,
  #                          xlab = NULL,
  #                          ylab = NULL,
  #                          col = "#CDE2B8",
  #                          border = "black",
  #                          horizontal = TRUE,
  #                          notch = FALSE,
  #                          outline = TRUE,
  #                          axes = TRUE,
  #                          staplewex = 1
  #                  )+coord_cartesian()+
  #                    text(x=quantile(vmy$mydata[,input$mpickerMultiPlot]),labels=quantile(vmy$mydata[,input$mpickerMultiPlot]),y=1.25,cex = 0.90)
  #                  points(mean(vmy$mydata[,input$mpickerMultiPlot]), col = "red", pch = 19, cex = 1.5)
  #                },
  #              
  #              "QQ Plot" = qqPlot(unlist(vmy$mydata[input$mpickerMultiPlot])),
  #              
  #              "Density Plot" =ggplot(vmy$mydata, aes(x=vmy$mydata[,input$mpickerMultiPlot])) +
  #                geom_density(fill = "#87CEFA")+xlab(input$mpickerMultiPlot)+
  #                geom_vline(data=vmy$mydata, aes(xintercept = mean(vmy$mydata[,input$mpickerMultiPlot])), colour='red') +
  #                geom_vline(data=vmy$mydata, aes(xintercept = median(vmy$mydata[,input$mpickerMultiPlot])), colour='black'),
  #              
  #              
  #              "Linearity Test" =  fnSimpleLineartyTest(),
  #              
  #              "All in one Linearity Test" =  fnmAllInOneLinearityPlot()
  #              
  #       )}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  #     
  #   }) 

  
  

  
  
  
  
  ######################################################################
  # Data Grouping
  #######################################################################
  observeEvent(input$mGroupmyDataBtn,{
    if (length(input$file)==0){
      return()
    }
    fnRadioButton(mlabel = 'Select Variable',mmulti = FALSE)
    showModal(
      mymodal(easyClose = T,
              size = 'l',
              column(style = "border: 4px double red;height: 550px;overflow-y: none;",
                     width = 12,
                     align = "center",
                     column(
                       width = 4,
                       x <- uiOutput('radio_BtnsCorrelation')
                     ),
                     column(
                       width = 4,   
                       numericInput(inputId = 'mGrpRangeVal',label = "Enter Range",min = 1,max = 10000,value = 10,width = '150px')
                       
                     ),
                     column(
                       width = 4,
                       div(style = "margin-top:-5px"),
                       HTML(paste('<h5><b>',"Click to Commit Grouping",'</b><h5>')),
                       div(style = "margin-top:-5px"),
                       actionButton(inputId = "mGrpCommitBtn",label = "Commit Grouping",style = styleButtonBlue(xheight = '40px',xwidth = '150px'))
                       
                     ),
                     # splitLayout(cellWidths = c('20%','30%','20%'),
                     # # actionButton(inputId = "mGrpReviewBtn",label = "Review",style = styleButtonBlue(xheight = '50px',xwidth = '100px'))
                     # ),
                     column(
                       width = 8,
                       offset = 2,
                       tags$head(tags$style("#mSurviceFreq{color:black; font-size:13px; font-style:normal;    #formatting verbatimtextoutput: verbatimTextOutput sizing and scrollable: where you got:https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193/2
overflow-x: auto;overflow-y: auto; height: 350px; background: #ffffd4;}")),
                       tags$head(tags$style("#mSurviceGroupedFreq{color:black; font-size:13px; font-style:normal;    #formatting verbatimtextoutput: verbatimTextOutput sizing and scrollable: where you got:https://community.rstudio.com/t/verbatimtextoutput-sizing-and-scrollable/1193/2
overflow-x: auto;overflow-y: auto; height: 350px; background: #ffffd4;}")),
                       
                       splitLayout(cellWidths = c("50%","50%"),
                                   HTML(paste("<h5><b>","Before Grouping ","</b><h5>")),
                                   HTML(paste("<h5><b>","Grouping will look like","</b><h5>"))
                       ),
                       splitLayout(cellWidths = c("50%","50%"),
                                   verbatimTextOutput('mSurviceFreq'),
                                   verbatimTextOutput('mSurviceGroupedFreq')
                       ),
                     )
              ), #column
              idcss = "test"
      )
    )
    
    
  })
  
  
  
  observeEvent(input$mradioPlotVarable,{
    mMin <- floor(custom_min(vmy$mydata[input$mradioPlotVarable]))
    mMax <- ceiling(custom_max(vmy$mydata[input$mradioPlotVarable]))

    updateNumericInput(session,inputId = 'mGrpRangeVal',min = mMin,max = mMax,value = round((mMax-mMin)/10,2))
    s <-  input$mradioPlotVarable
    if (length(s)) {
      output$mSurviceFreq <- renderPrint({
        as.data.frame(table(vmy$mydata[input$mradioPlotVarable]))
      })
    }
  })
  
  
  vmy$data_1 <- reactive({
    req(input$mGrpRangeVal)
    tempdf <- vmy$mydata
    mMin <- floor(custom_min(vmy$mydata[input$mradioPlotVarable]))
    mMax <- ceiling(custom_max(vmy$mydata[input$mradioPlotVarable]))
    c <- input$mGrpRangeVal
    mlabels <- c(paste(seq(mMin, mMax-c, by = c), seq(mMin+c, mMax, by = c),
                       sep = "-"))
    breaks = c(seq(mMin, mMax-c, by = c), Inf)
    tempdf[[input$mradioPlotVarable]] <- cut(tempdf[[input$mradioPlotVarable]], breaks = c(seq(mMin, mMax-c, by = c), Inf), labels = mlabels, right = FALSE)
    return(tempdf)
  })
  
  output$mSurviceGroupedFreq <- renderPrint({
    as.data.frame(table(vmy$data_1()[input$mradioPlotVarable]))
    
  })
  
  
  # observeEvent(
  #   c(input$mGrpReviewBtn,
  #     input$mGrpRangeVal),{
  
  
  ##########################################################################
  # Code to Commit Grouping
  ##########################################################################
  observeEvent(input$mGrpCommitBtn,{
    if (length(input$file)==0){
      return()
    }
    mMin <- floor(custom_min(vmy$mydata[input$mradioPlotVarable]))
    mMax <- ceiling(custom_max(vmy$mydata[input$mradioPlotVarable]))
    
    c <- input$mGrpRangeVal
    mlabels <- c(paste(seq(mMin, mMax-c, by = c), seq(mMin+c, mMax, by = c),
                       sep = "-"))
    breaks = c(seq(mMin, mMax-c, by = c), Inf)
    vmy$mydata[[input$mradioPlotVarable]] <- cut(vmy$mydata[[input$mradioPlotVarable]], breaks = c(seq(mMin, mMax-c, by = c), Inf), labels = mlabels, right = FALSE) 
    output$mSurviceGroupedFreq <- renderPrint({
      as.data.frame(table(vmy$mydata[input$mradioPlotVarable]))
    }) 
    removeModal()
    click(id = 'mshowtableBtn')
  })
  
  
  ######################################################################
  # Remove all Duplicate rows in the entire Data Frame
  #######################################################################
  library(dplyr)
  
  observeEvent(input$mRemoveDuplicateBtn,{
    if (length(input$file)==0){
      return()
    }
    #remove duplicate rows from data frame using dplyr
    vmy$mydata <- vmy$mydata %>%
      distinct(.keep_all = TRUE)
  })
  
  
  #############################################################################
  # Rename Data Frame columns  
  #############################################################################
  
  fnCreateRenamedf <- function(){
    df_rename <- data.frame("New_Name" = unlist(lapply(vmy$mydata, class)))
    df_rename['Var_name'] <- rownames(df_rename)
    df_rename['New_Name'] <- rownames(df_rename)
    row.names(df_rename) <- seq(1,nrow(df_rename))
    
    vmy$df_rename <- df_rename
    vmy$df_rename <-vmy$df_rename %>% dplyr::select('Var_name', everything())      
    
  }
  
  output$mColRenameTbl <- DT::renderDataTable({
    DT::datatable(vmy$df_rename,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = TRUE,
                  selection = list(mode = "single", selected = c(1), target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(scrollY = '275px',dom = 't',ordering=F, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff'});",   #::I got from this site; Thank you: JS from https://stackoverflow.com/questions/34009922/how-to-format-a-shiny-rendertable
                                   "}")
                  ) 
    )
  })
  
  # table edit and save script where you got: https://github.com/rstudio/DT/pull/480
  # there is link the name of example in this blog  that takes to github.com https://blog.rstudio.com/2018/03/29/dt-0-4/   
  
  proxy = dataTableProxy('mColRenameTbl')
  
  observeEvent(input$mColRenameTbl_cell_edit, {
    info = input$mColRenameTbl_cell_edit
    str(info)
    i = info$row
    j = info$col+1
    v = info$value
    vmy$df_rename[i, j] <- DT::coerceValue(v, vmy$df_rename[i, j])
    replaceData(proxy, vmy$df_rename, resetPaging = FALSE)  # important
  })
  
  
  observeEvent(input$mColRenameBtn,{
    if (length(input$file)==0){
      return()
    }
    n <- nrow(vmy$df_rename)
    for (i in 1:n){
      n=which(colnames(vmy$mydata)== vmy$df_rename[i,1])
      colnames(vmy$mydata)[n] <- vmy$df_rename[i,2]
    }
    fncreatedftype()
    fnCreateRenamedf()
  })
  
  
  

  
  ############################################
  # Transpose a data set from wide to long
  ############################################
  observeEvent(input$btnWidetoLong,{
    if (length(input$file)==0){
      return()
    }
    if(length(vmy$df_types[input$dt_cell_clicked$row,1])==0 ){
      shinyalert(title = 'Instruction',text = paste('First you need to select the variable which should be the key for Wide to Long',
                                                    "For example in iris data set key variable is Species"))
    }
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure Convert this variable as FActor:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("mWideLongyes", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please Convert data from Wide to Long!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then factor the selected rows
  observeEvent(input$mWideLongyes, {
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    
    library(reshape2)
    vmy$mydata<- melt(data = vmy$mydata,id.vars = mselected[1],variable.name = "Var_Name",value.name = "Value",na.rm = TRUE,factorsAsStrings = TRUE)
    
    
    removeModal()
    fncreatedftype()
    fnCreateRenamedf()
  })
  
  
} #server closure
shinyApp(ui, server)