library(tidyverse)
library(shiny)
library(shinydashboard)
library(DBI)
library(rJava)
library(RJDBC)
library(DT)
library(data.table)
library(rhandsontable)
library(zoo)
library(shinyjs)
library(readxl)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

# options(java.parameters = "-Xmx16g")
# 
# if (grepl('^awsrshiny',Sys.info()['nodename'])){
#   drv <- JDBC(driverClass="com.microsoft.sqlserver.jdbc.SQLServerDriver",classPath = "/usr/share/java-1.8.0/mssql-jdbc-6.4.0.jre8.jar")
#   jdbc_str <- "jdbc:sqlserver://10.120.11.44;databasename=GCM"
# } else {
#   options(java.parameters = "-Xmx4g")
#   drv <- JDBC(driverClass="com.microsoft.sqlserver.jdbc.SQLServerDriver",classPath = "C:/Users/lin68913/Downloads/sqljdbc_6.4.0.0_enu.tar/sqljdbc_6.4/enu/mssql-jdbc-6.4.0.jre8.jar")
#   jdbc_str <- "jdbc:sqlserver://10.121.1.133;databasename=GCM"
# }
#  conn <- dbConnect(drv,jdbc_str,"admin","gcmsqldb1",data)
# 



# conn <- dbConnect(drv,jdbc_str,"cimsql","roadtrip",data)

# tbl_material <- dbGetQuery(conn,'Select distinct [Commodity Manager] from tbl_material')
# com <- tbl_material$`Commodity Manager` %>% as.list() %>% unique()
# com2 <- paste("'", com, "'", sep="", collapse=", ") 
# com2 <-  as.list(strsplit(com2,",")[[1]])

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "GCM",
                  tags$li(a(href = 'https://helpdesk.lumentum.com',
                            img(src = 'logo.svg',
                                title = "Lumentum", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown")
  ),
  dashboardSidebar(
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    uiOutput("userpanel"),
    sidebarMenu(
      menuItem("Main",tabName="main"),
      menuItem("ExcelUpload",tabName="excelupload"),
      # menuItem("Reports",tabName="report"),
      menuItem("Reports",tabName="report2"),
      NULL
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("main",
              box(title="Material",status = "info",solidHeader = FALSE,collapsible = TRUE, collapsed = FALSE, width=NULL, 
                  box(width=8, 
                      DT::dataTableOutput("main_mtrl"),
                      DT::dataTableOutput(""),
                      actionButton("new_material", "New"),
                      useShinyjs(),                                           # Include shinyjs in the UI
                      extendShinyjs(text = jsResetCode),                      # Add the js code to the page
                      #actionButton("edit_material", "Edit"),
                      NULL
                  ),
                  
                  box(width=4,rHandsontableOutput('main_mtrl_dtl2'),
                      actionButton("update_material", "update")
                  ),
                  
                  #    box(width=4,
                  # 	   DT::dataTableOutput("main_mtrl_dtl")
                  #    ),
                  NULL
              ),
              box(title="Supplier",status = "info",solidHeader = FALSE,collapsible = TRUE, collapsed = FALSE, width=NULL, 
                  # DT::dataTableOutput("main_supplier"),
                  rHandsontableOutput('main_supplier2'),
                  br(),
                  actionButton("update_supplier", "update"),
                  br(),
                  actionButton("new_supplier", "New"),
                  
                  
                  
                  
                  NULL     
              ),
              
              
              
              box(title="Demand Pricing Allocation",status = "info",solidHeader = FALSE,collapsible = TRUE, collapsed = FALSE,width = NULL,
                  
                  box(width=4,title = "Demand", 
                      rHandsontableOutput("main_demand2"),
                      column(1,actionButton(inputId = "update_demand", label = "Update")
                      )),
                  
                  box(width=8,title = "Pricing & Allocation", 
                      rHandsontableOutput("main_pricing2"),
                      column(1,actionButton(inputId = "update_pricing&allocation", label = "Update")
                      )),
                  
                  NULL     
              ),  
              NULL
      ),
      
      tabItem("excelupload",
              fluidPage(
                title = 'ExcelUpload',
                fileInput("file1", "Choose EXCEL File",
                          multiple = FALSE,
                          accept = c(".xls",".XLS", ".xlsx")),
                radioButtons("disp", "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head"),
                
                tableOutput("contents"),
                actionButton("Upload_excel_demand","Comfirm and Upload")
                
              )),
      
      tabItem("report2",
              fluidPage(
                title = 'Report2',
                uiOutput('commoditymanager2'),
                selectInput('sdate','Begin quarter',choices=gsub(" ", "", as.yearqtr(2018+seq(0, 51)/4) , fixed = TRUE)),
                selectInput('edate','End quarter',choices=gsub(" ", "", as.yearqtr(2018+seq(0, 51)/4) , fixed = TRUE)),
                uiOutput('reportcolumns'),
                actionButton(inputId='generatereport',label = 'Generate Report'),
                
                fluidRow(
                  DT::dataTableOutput('testreport')
                ),
                fluidRow(downloadButton("filteredreport", 'Download Report'))
                
              ))
      
    )
    
  )
)

server <- function(input, output, session) { 
  
   
  # destruction
  onStop(function() {
    dbDisconnect(conn)
  })
  
  
  options(java.parameters = "-Xmx16g")
  
  if (grepl('^awsrshiny',Sys.info()['nodename'])){
    drv <- JDBC(driverClass="com.microsoft.sqlserver.jdbc.SQLServerDriver",classPath = "/usr/share/java-1.8.0/mssql-jdbc-6.4.0.jre8.jar")
    jdbc_str <- "jdbc:sqlserver://10.120.11.44;databasename=GCM"
    conn <- dbConnect(drv,jdbc_str,"admin","gcmsqldb1",data)
  } else {
    options(java.parameters = "-Xmx4g")
    drv <- JDBC(driverClass="com.microsoft.sqlserver.jdbc.SQLServerDriver",classPath = "C:/Users/lin68913/Downloads/sqljdbc_6.4.0.0_enu.tar/sqljdbc_6.4/enu/mssql-jdbc-6.4.0.jre8.jar")
    jdbc_str <- "jdbc:sqlserver://10.121.1.133;databasename=GCM"
    conn <- dbConnect(drv,jdbc_str,"cimsql","roadtrip",data)
    
  }
  
  
  
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
  
  
  mtrl_tbl <- dbGetQuery(conn, "SELECT  * FROM [dbo].[view_material_supplier]") %>% as_tibble
  RV <- reactiveValues(data = mtrl_tbl) 
  # rv_data <- reactive({RV$data})
  output$main_mtrl <- DT::renderDataTable({RV$data} %>% select(`Part Number`,`Commodity Manager`,`CM`,`Supplier Name`), 
                                          selection=list(mode='single'), filter='top')
  
  observeEvent(input$main_mtrl_row_last_clicked, {
    chosen_mtrl <- mtrl_tbl[input$main_mtrl_row_last_clicked,] %>% as.list
    
    output$chosen_mtrl<-DT::renderDataTable(chosen_mtrl)
    
    # Update Material detail panel
    sql1 <- paste0("SELECT * FROM dbo.tbl_material 
                   WHERE [Part Number]='", chosen_mtrl[["Part Number"]],"' AND CM='",chosen_mtrl[["CM"]],
                   "'")
    tbl_material1 <- dbGetQuery(conn, sql1) %>% as_tibble
    tbl_material2 <- transpose(tbl_material1)
    rownames(tbl_material2) <- colnames(tbl_material1)
    colnames(tbl_material2) <- rownames(tbl_material1)
    
    output$main_mtrl_dtl2 <- renderRHandsontable({
      rhandsontable(tbl_material2, rowHeaderWidth =150,selectCallback = TRUE) %>%
        hot_row(2, readOnly = TRUE) %>%
        hot_row(3, readOnly = TRUE)})
    
    
    
    # Update Supplier panel
    sql2 <- paste0("SELECT * FROM dbo.tbl_supplier 
                   WHERE [Part Number]='", chosen_mtrl[["Part Number"]],"' AND CM='",chosen_mtrl[["CM"]],
                   "'")
    supplier_tbl <- dbGetQuery(conn, sql2) %>% as_tibble
    RV$data2 <- supplier_tbl
    # output$main_supplier <- DT::renderDataTable(supplier_tbl, editable=TRUE,
    #                                             selection=list(mode='single'))
    ###
    output$main_supplier2 <- renderRHandsontable({
      rhandsontable(RV$data2,selectCallback = TRUE)%>%
        hot_col(c("Part Number","CM"), readOnly = TRUE)})
    
    sql3 <- paste0("SELECT [Date],[Demand] FROM dbo.tbl_Demand 
                   WHERE valid =1 AND [CM]='", chosen_mtrl[["CM"]],"' AND [Part Number]='",chosen_mtrl[["Part Number"]],
                   "'")
    demand_tbl <- dbGetQuery(conn, sql3) %>% as_tibble %>% arrange(desc(Date))
    output$main_demand2 <- renderRHandsontable(
      rhandsontable(demand_tbl,selectCallback = TRUE) 
      # %>%
      #   hot_col(c("Part Number","Date"), readOnly = TRUE) 
    )
    # output$main_demand <- DT::renderDataTable(demand_tbl,editable=TRUE,options = list(order = list(2,'asc'),pageLength=20))
    
    
    observeEvent(
      input$main_supplier2_select$select$r,
      # input$main_supplier_rows_selected,
      {
        
        choose_supplier<-RV$data2[input$main_supplier2_select$select$r,] 
        
        sql4 <- paste0("SELECT p.[Part Number],p.[Manufacturer Name],p.[Supplier Name], p.Date,pricing,allocation FROM dbo.tbl_pricing p
                       LEFT JOIN dbo.tbl_allocation a on p.[CM]=a.[CM] and p.[Manufacturer Name]=a.[Manufacturer Name] and p.[Supplier Name]=a.[Supplier Name] and p.Date=a.Date  and p.[Part number]=a.[Part number]
                       WHERE p.[CM]='", choose_supplier[["CM"]],"' AND p.[Manufacturer Name]='",choose_supplier[["Manufacturer Name"]],
                       "' AND p.[Part Number]='",choose_supplier[["Part Number"]],
                       "' AND p.[Supplier Name]='",choose_supplier[["Supplier Name"]],
                       "'")
        pricing_tbl <- dbGetQuery(conn, sql4) %>% as_tibble  
        # %>% formatPercentage('allocation',0)
        # output$main_pricing <- DT::renderDataTable(pricing_tbl,editable=TRUE,options = list(order = list(4,'asc'),pageLength=20))
        output$main_pricing2 <- renderRHandsontable(
          rhandsontable(pricing_tbl,selectCallback = TRUE) %>%
            hot_col(c("Part Number","Manufacturer Name","Supplier Name"), readOnly = TRUE))
      })
    
  })
  
  
  
  #New_material
  
  materialModel <- function() {
    tbl_material <- dbGetQuery(conn,'Select * from tbl_material')
    commodity <- tbl_material$Commodity %>% unique()
    cm <- tbl_material$CM %>% unique()
    subcommodity1<- tbl_material$'Sub-Commodity (GCM Reporting)' %>% unique()
    subcommodity2<- tbl_material$'Sub-Commodity(Agile)' %>% unique()
    
    modalDialog(
      renderText(paste("User:",if (!is.null(session$user)){print(session$user)}else{'manual'})),
      textInput(inputId='n_partnumber',label='Part Number'),
      selectizeInput(inputId='n_CM',label='CM',choices=cm),
      textInput(inputId='n_itemdesc',label='Item Description'),
      selectizeInput(inputId='n_commodity',label='Commodity',choices=commodity),
      textInput(inputId='n_commoditymanager',label='Commodity Manager'),
      selectizeInput(inputId='n_subcommodity1',label='Sub Commodity',choices=subcommodity1),
      selectizeInput(inputId='n_subcommodity2',label='Sub Commodity(Agile)',choices=subcommodity2),
      textInput(inputId='n_L4',label='L4 Product Line'),
      textInput(inputId='n_program',label='Program'),
      textInput(inputId='n_comments',label='Component Comments'),
      textInput(inputId='n_time1',label='Recordtime:',value=Sys.time()),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("new_material_upload", "OK")
      ),
      
      NULL
    )}
  
  observeEvent(input$new_material,{
    showModal(materialModel())
  }
  )
  
  observeEvent(input$new_material_upload,{   
    sql_new_material <- 
      paste0("INSERT INTO GCM.dbo.tbl_material  VALUES ('",if (!is.null(session$user)){print(session$user)}else{'manual'},"','",
             input$n_partnumber,"','",input$n_CM,"','",input$n_itemdesc,"','",
             input$n_commodity,"','",input$n_commoditymanager,"','",input$n_subcommodity1,"','",
             input$n_subcommodity2,"','",input$n_L4,"','",input$n_program,"','",input$n_comments,"','1')"
      )
    dbSendUpdate(conn,sql_new_material)
    RV$data <- dbGetQuery(conn, "SELECT  * FROM [dbo].[view_material_supplier]") %>% as_tibble
    js$reset()
    removeModal()
    
  })
  
  # Update Material
  observeEvent(input$update_material,{
    sql_upload_material <-
      paste0("UPDATE GCM.dbo.tbl_material
             SET [Source.Name]='",if (!is.null(session$user)){print(session$user)}else{'manual'},
             "',[Item Description]='",hot_to_r(input$main_mtrl_dtl2)[4,],
             "',[Commodity]='",hot_to_r(input$main_mtrl_dtl2)[5,],
             "',[Commodity Manager]='",hot_to_r(input$main_mtrl_dtl2)[6,],
             "',[Sub-Commodity (GCM Reporting)]='",hot_to_r(input$main_mtrl_dtl2)[7,],
             "',[Sub-Commodity(Agile)]='",hot_to_r(input$main_mtrl_dtl2)[8,],
             "',[L4 Product Line]='",hot_to_r(input$main_mtrl_dtl2)[9,],
             "',[Program]='",hot_to_r(input$main_mtrl_dtl2)[10,],
             "',[Component comments]='",hot_to_r(input$main_mtrl_dtl2)[11,],
             "',[Valid]='",hot_to_r(input$main_mtrl_dtl2)[12,],
             "' WHERE [Part Number] ='",hot_to_r(input$main_mtrl_dtl2)[2,],
             "' AND CM = '",hot_to_r(input$main_mtrl_dtl2)[3,],"'")
    
    dbSendUpdate(conn,sql_upload_material)
    
    
  }
  )
  
  
  # New Supplier
  supplierModel <- function() {
    
    modalDialog(
      renderText(paste("User:",if (!is.null(session$user)){print(session$user)}else{'manual'})),
      renderText(paste("Part Number:",mtrl_tbl[["Part Number"]][input$main_mtrl_rows_selected])),
      renderText(paste("CM:",mtrl_tbl[["CM"]][input$main_mtrl_rows_selected])),
      radioButtons(inputId='n_litecontrolled',label="LITE Controlled:",c("CM"="CM Controlled","LITE"="LITE Controlled")),
      textInput(inputId='n_manufacturername',label='Manufacturer Name'),
      textInput(inputId='n_suppliername',label='Supplier Name','Supplier Name'),
      textInput(inputId='n_mpn',label='MPN'),
      numericInput(inputId='n_leadtime',label='Lead Time [Days]',value=1,min=0,max=NA),
      textInput(inputId='n_inventoryprogrampricingchange',label='Inventory Program/Pricing Changes'),
      numericInput(inputId='n_moq',label='MOQ',value=1,min=0,max=NA),
      textInput(inputId='n_uom',label='UOM'),
      selectInput(inputId='n_rank',label='Rank',choices = c('1','2','3'),selected='1'),
      # textInput(inputId='n_comments',label='Component Comments'),
      Sys.time(),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("new_supplier_upload", "OK")
      ),
      
      NULL
    )}
  
  observeEvent(input$new_supplier,{
    showModal(supplierModel())
  }
  )
  
  observeEvent(input$new_supplier_upload,{
    sql_new_supplier <-
      paste0("INSERT INTO GCM.dbo.tbl_supplier  VALUES ( '",if (!is.null(session$user)){print(session$user)}else{'manual'},"','",
             mtrl_tbl[["Part Number"]][input$main_mtrl_rows_selected],"','",
             mtrl_tbl[["CM"]][input$main_mtrl_rows_selected],"','",
             input$n_litecontrolled,"','",
             input$n_manufacturername,"','",
             input$n_suppliername,"','",
             input$n_mpn,"','",
             input$n_leadtime,"','",
             input$n_inventoryprogrampricingchange,"','",
             input$n_moq,"','",
             input$n_uom,"','",
             input$n_rank,"','1'",")"
      )
    dbSendUpdate(conn,sql_new_supplier)
    RV$data <- dbGetQuery(conn, "SELECT  * FROM [dbo].[view_material_supplier]") %>% as_tibble
    sql2 <- paste0("SELECT * FROM dbo.tbl_supplier 
                   WHERE [Part Number]='", mtrl_tbl[["Part Number"]][input$main_mtrl_rows_selected],"' AND CM='",mtrl_tbl[["CM"]][input$main_mtrl_rows_selected],
                   "'")
    supplier_tbl <- dbGetQuery(conn, sql2) %>% as_tibble
    RV$data2 <- supplier_tbl
    removeModal()
  })
  
  # Upload Supplier 
  observeEvent(input$update_supplier,{
    sql_upload_material <-
      paste0("UPDATE GCM.dbo.tbl_supplier
             SET [LITE Controlled]='",hot_to_r(input$main_supplier2)[,4],
             "',[Manufacturer Name]='",hot_to_r(input$main_supplier2)[,5],
             "',[Supplier Name]='",hot_to_r(input$main_supplier2)[,6],
             "',[MPN]='",hot_to_r(input$main_supplier2)[,7],
             "',[Lead Time [Days]]]='",hot_to_r(input$main_supplier2)[,8],
             "',[Inventory Program/Pricing Changes]='",hot_to_r(input$main_supplier2)[input$main_supplier2_select$select$r,9],
             "',[MOQ]='",hot_to_r(input$main_supplier2)[,10],
             "',[UOM]='",hot_to_r(input$main_supplier2)[,11],
             "',[Rank]='",hot_to_r(input$main_supplier2)[,12],
             "',[Valid]='",hot_to_r(input$main_supplier2)[,13],
             "',[Source.Name] = '",if (!is.null(session$user)){print(session$user)}else{'manual'},
             "' WHERE [Part Number] ='",hot_to_r(input$main_supplier2)[,2],
             "' AND CM = '",hot_to_r(input$main_supplier2)[,3],
             "' AND RANK = '",hot_to_r(input$main_supplier2)[,12],
             "'")
    
    dbSendUpdate(conn,sql_upload_material)
  }
  )
  
  # Update_Demand
  observeEvent(input$update_demand,{
    chosen_mtrl <- mtrl_tbl[input$main_mtrl_row_last_clicked,] %>% as.list
    sql_deletedemand <-
      paste0("delete from tbl_demand where [part number] ='",chosen_mtrl[["Part Number"]],
             "' AND CM='",chosen_mtrl[["CM"]],"'")
    dbSendUpdate(conn,sql_deletedemand)
    demand <- hot_to_r(input$main_demand2) %>% filter(!is.na(Demand))
    demand$'Source.Name'<- if (!is.null(session$user)){print(session$user)}else{'manual'}
    demand$CM <- chosen_mtrl[["CM"]]
    demand$'Part Number' <- chosen_mtrl[["Part Number"]]
    demand$'Manufacturer Name' <- ""
    demand$'Supplier Name' <- ""
    setcolorder(demand, c("Source.Name","CM","Part Number","Manufacturer Name","Supplier Name","Date","Demand"))
    colnames(demand) <- paste0('"',colnames(demand),'"') 
    dbWriteTable(conn, "tbl_Demand", demand,append = TRUE,overwrite = FALSE)
    
    
    
  })
  
  # Update_Pricing & Allocation
  observeEvent(input$"update_pricing&allocation",{
    choose_supplier<-RV$data2[input$main_supplier2_select$select$r,] %>% as.list
    sql_deletepricing <-
      paste0("delete from tbl_pricing where [part number] ='",choose_supplier[["Part Number"]],
             "' AND CM='",choose_supplier[["CM"]],"' AND [Manufacturer Name]='",choose_supplier[["Manufacturer Name"]]
             ,"' AND [Supplier Name]='",choose_supplier[["Supplier Name"]],"'")
    sql_deleteallocation <-
      paste0("delete from tbl_allocation where [part number] ='",choose_supplier[["Part Number"]],
             "' AND CM='",choose_supplier[["CM"]],"' AND [Manufacturer Name]='",choose_supplier[["Manufacturer Name"]]
             ,"' AND [Supplier Name]='",choose_supplier[["Supplier Name"]],"'")
    
    dbSendUpdate(conn,sql_deletepricing)
    dbSendUpdate(conn,sql_deleteallocation)
    
    
    pricing <- hot_to_r(input$main_pricing2) %>% filter(!is.na(pricing)) %>% select(-allocation)
    pricing$'Source.Name'<- if (!is.null(session$user)){print(session$user)}else{'manual'}
    pricing$CM <- choose_supplier[["CM"]] %>% as.character()
    pricing$'Part Number' <- choose_supplier[["Part Number"]] %>% as.character()
    pricing$'Manufacturer Name' <-  choose_supplier[["Manufacturer Name"]] %>% as.character()
    pricing$'Supplier Name' <-  choose_supplier[["Supplier Name"]] %>% as.character()
    setcolorder(pricing, c("Source.Name","CM","Part Number","Manufacturer Name","Supplier Name","Date","pricing"))
    colnames(pricing) <- paste0('"',colnames(pricing),'"')
    dbWriteTable(conn, "tbl_pricing", pricing,append = TRUE,overwrite = FALSE)
    
    allocation <- hot_to_r(input$main_pricing2) %>% filter(!is.na(allocation)) %>% select(-pricing)
    allocation$'Source.Name'<- if (!is.null(session$user)){print(session$user)}else{'manual'}
    allocation$CM <- choose_supplier[["CM"]]
    allocation$'Part Number' <- choose_supplier[["Part Number"]]
    allocation$'Manufacturer Name' <-  choose_supplier[["Manufacturer Name"]]
    allocation$'Supplier Name' <- choose_supplier[["Supplier Name"]]
    setcolorder(allocation, c("Source.Name","CM","Part Number","Manufacturer Name","Supplier Name","Date","allocation"))
    colnames(allocation) <- paste0('"',colnames(allocation),'"')
    dbWriteTable(conn, "tbl_allocation", allocation,append = TRUE,overwrite = FALSE)
    
    
    
  })
  
  
  # # Massive upload
  # dataModal <- function(failed = FALSE) {
  #   modalDialog(size = c("m"),
  #               textAreaInput("dataset", "Choose data set",
  #                             placeholder = 'Paste the data from Excel template'
  #               ),
  #               span(''),
  #               if (failed)
  #                 div(tags$b("Invalid Data", style = "color: red;")),
  #               
  #               footer = tagList(
  #                 modalButton("Cancel"),
  #                 actionButton("ok", "OK")
  #               ),
  #               verbatimTextOutput("datareview")
  #   )
  # }
  # 
  # observeEvent(input$submit,{
  #   showModal(dataModal())
  # })
  # 
  # output$datareview <- renderText({input$dataset})
  
  
  #select input
  output$commoditymanager2 = renderUI({
    tbl_material <- dbGetQuery(conn,'Select distinct [Commodity Manager] from tbl_material') %>% arrange(`Commodity Manager`)
    com <- tbl_material$`Commodity Manager` %>% as.list() %>% unique()
    selectInput('commoditymanager', 'Commodity Manager',choices = com ,selectize=TRUE,multiple=TRUE)
  })
  
  #select reportcolumns input
  output$reportcolumns = renderUI({
    reportcolumns <- colnames(mtrl_tbl)
    reportcolumns <- reportcolumns[!grepl("Part Number|CM|Supplier Name|Manufacturer Name|Rank|[Lead Time [Days]]",unlist(reportcolumns))]
    selectInput('reportcolumns2', 'Report Columns',choices = reportcolumns ,selectize=TRUE,selected= reportcolumns,multiple=TRUE,width=600)
  })
  #Report
  
  # df1 <- dbGetQuery(conn,'SELECT top 10 S.*,
  #                   ISNULL(d.date,p.date) as Date,
  #                   d.demand,
  #                   p.pricing,
  #                   a.allocation
  #                   FROM tbl_supplier s
  #                   LEFT JOIN tbl_material m on s.CM=m.CM and s.[part number]=m.[part number]
  #                   LEFT JOIN
  #                   tbl_demand d on d.[Part Number]=s.[Part Number] and d.[CM]=s.[CM]
  #                   FULL JOIN tbl_pricing p on s.[Part Number]=p.[Part Number] and s.[CM]=p.[CM] and s.[Manufacturer Name]=p.[Manufacturer Name] and s.[supplier name]=p.[supplier name] and d.date=p.date
  #                   FULL JOIN tbl_allocation a on s.[Part Number]=a.[Part Number] and s.[CM]=a.[CM] and s.[Manufacturer Name]=a.[Manufacturer Name] and s.[supplier name]=a.[supplier name] and d.date=a.date
  #                   WHERE S.CM IS NOT NULL
  #                   ')
  # 
  # # render the table (with row names)
  # 
  # output$x1 = DT::renderDataTable(df1,filter='top',options =
  #                                   list(
  #                                     pageLength = 5,
  #                                     autoWidth = TRUE,
  #                                     scrollX=TRUE)
  #                                 ,rownames= FALSE)
  # 
  # 
  # 
  # # download the filtered data
  # output$filteredreport = downloadHandler(filename = 'filtered.csv',
  #                             content = function(file){
  #                               df2 <- input$x1_rows_all
  #                               write.csv(df1[df2, ,drop = FALSE],file,row.names=FALSE)
  #                             })
  # 
  #Generate Report
  observeEvent(input$generatereport,{
    commodity <- input$commoditymanager 
    commodity2 <- paste0("'",commodity,"'",collapse=",")
    
    reportcolumns <- input$reportcolumns2
    reportcolumns <- paste0("[",reportcolumns,"]",collapse=",")
    sql_generatereport <- paste0("SELECT * FROM view_gcmreport WHERE [Commodity Manager] in (",commodity2,") AND DATE between '",
                                 input$sdate,"' and '",input$edate,"'")
    
    # sql_generatereport <- paste0("SELECT [Part Number],[CM],[Supplier Name],[Manufacturer Name],Rank,Date,Demand,Pricing,Allocation,"
    #                              ,reportcolumns,
    #                              " FROM view_gcmreport WHERE [Commodity Manager] in (",commodity2,") AND DATE between '",
    #                              input$sdate,"' and '",input$edate,"'")
    
    
    report_data <- dbGetQuery(conn,sql_generatereport)
    report_data <- report_data %>%
      mutate('Frozen WAPP'=report_data$Pricing*report_data$Allocation,
             'Spending'=report_data$Pricing*report_data$Allocation*report_data$Demand)
    
    
    output$testreport = DT::renderDataTable(report_data,filter='top',options =
                                              list(
                                                pageLength = 5,
                                                autoWidth = TRUE,
                                                scrollX=TRUE)
                                            ,rownames= FALSE)
    
    output_data <- report_data %>% 
      gather(key='measure', value='value', Demand, Pricing, Allocation,`Frozen WAPP`,Spending) %>% 
      group_by(`Part Number`, `CM`, `Manufacturer Name`, `Supplier Name`,`measure`,`Date`,`Rank`) %>%
      mutate(the_rank = rank(-value, ties.method="random")) %>%
      filter(the_rank==1) %>%
      select (-the_rank) %>% 
      mutate(date_name = paste(measure,Date,sep=' ')) %>%
      ungroup%>%
      arrange(`measure`,`Date`) %>%
      select(-Date,-measure) %>%
      spread(key=date_name,value) %>%
      arrange(`Part Number`)
    
    output_data2 <- output_data[,c(1:grep("^Allocation",colnames(output_data))[1]-1,
                                   grep("^Demand",colnames(output_data)),
                                   grep("^Pricing",colnames(output_data)),
                                   grep("^Allocation",colnames(output_data)),
                                   grep("^Frozen WAPP",colnames(output_data)),
                                   grep("^Spending",colnames(output_data)))]
    
    # download the filtered data
    output$filteredreport = downloadHandler(filename = 'filtered.csv',
                                            content = function(file){
                                              write.csv(output_data2,file,row.names=FALSE)
                                            })
  })
  
  output$contents <- renderTable({
    file_input_react()
  })
  
  file_input_react <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        demandexcel <- read_excel(input$file1$datapath,
                         sheet=1,
                         col_names = TRUE,
                         col_type = "list")         %>%
        mutate(Source.Name=if (!is.null(session$user)){print(session$user)}else{'manual'},input_time=format(Sys.time()))
        setcolorder(demandexcel, c("Source.Name","CM","Part Number","Manufacturer Name","Supplier Name","Date","Demand","Valid","input_time"))
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(demandexcel))
    }
    else {
      return(demandexcel)
    }
    

  })
  
  observeEvent(input$Upload_excel_demand,{
    dbWriteTable(conn,"tbl_demand",contents,append = TRUE,overwrite = FALSE)
    # dbGetQuery(conn,"")
  })

  
  
  
}



# shinyApp(ui,server)

# runApp(shinyApp(ui, server), launch.browser = TRUE)


if (grepl('^awsrshiny',Sys.info()['nodename'])){
  shinyApp(ui,server)
} else {
  options(java.parameters = "-Xmx4g")
  runApp(shinyApp(ui, server), launch.browser = TRUE)
}


