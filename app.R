


# install.packages(c("shiny",
#                    "shinydashboard",
#                    "ggrepel",
#                    "gdata"))




library(shiny)
library(shinydashboard)
library(gdata)
library(ggrepel)




#################################################################################################################################



ui <- dashboardPage(
    
    skin = "black",
        
    dashboardHeader(title = "Labeled Plots", titleWidth = 300,

                    tags$li(class = "dropdown",
                            tags$style(".main-header {max-height: 80px}"),
                            tags$style(".skin-black .main-header .logo {
                                                            height: 80px;
                                                            font-size: 32px;
                                                            font-weight: bold;
                                                            color: white;
                                                            background-color: #1e282c;
                                                            padding-top: 15px;
                                                            }"),
                            tags$style(".skin-black .main-header .logo:hover {background-color: #1e282c;"),
                            tags$style(".skin-black .sidebar-toggle {
                                                                    height: 80px;
                                                                    font-size: 20px;
                                                                    padding: 25px;
                                                                    }"),
                            tags$style(".navbar {min-height:80px}")
                    )

    ),
 
    
    dashboardSidebar(
          
        width = 300, 
        tags$style(".left-side, .main-sidebar {padding-top: 80px}"),
        
        sidebarMenu(
              
              menuItem(text = h3("Data Setup"), 
                       tabName = "plots" #, 
                       #icon = icon("line-chart")
                       ),
              
              hr(),

              fileInput(inputId = "my_data", 
                        label = "Upload Data (xlsx or csv)", 
                        accept = c(".xlsx", ".csv")),
              
              hr(),
                  
              textInput(inputId = "col_symbol", 
                        label = "Name Column",
                        value = "symbol"),
              
              textInput(inputId = "col_id", 
                        label = "ID Column (optional)",
                        value = ""),
              
              textInput(inputId = "col_xdata", 
                        label = "X - Data Column",
                        value = "xdata"),
              
              textInput(inputId = "col_ydata", 
                        label = "Y - Data Column",
                        value = "ydata"),
              
              hr(),
              
              
              column(width = 12, align = "left", offset = 0.5,
                     h3("Refresh Plot")
              ),
              
              column(width = 12, align = "center", offset = 0,
                     
                     actionButton("goButton", "Refresh", icon("redo-alt"), 
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4; margin: 10px"),
                     
                     helpText("Push the button to apply changes"),
                     
                     hr()
                     
              ),

              hr(),
              
              column(width = 12, align = "left", offset = 0.5,
                     h3("Download PDF")
              ),
              
              column(width = 12, align = "center", offset = 0,
                    downloadButton(outputId = "pdf_download", label = "Download", 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4; margin: 10px"),
                    
                    helpText("Push the button to save file")
                    ),
              
              
              column(6, 
                     textInput(inputId = "plotheight", 
                               label = "PDF Height",
                               value = "8")
                     ),
              column(6,
                     textInput(inputId = "plotwidth", 
                               label = "PDF Width",
                               value = "6")),
              
              column(12,
                     hr()
                     )

             
             
              # 
              # 
              #        
              #        
              #        
              #        )
              # 
              # #helpText("Choose smaller plot size for \\n relatively larger font/point size"),
              # 
              # 
             
            )
    ),
    
    
    dashboardBody(
        
        tags$style(".row {
                        padding-left: 25px;
                        padding-right: 25px;
                   }"),
        
        tags$style(".col-sm-4 {
                        padding-left: 25px;
                        padding-right: 25px;
                   }"),

        tags$style(".col-sm-6 {
                        padding-left: 25px;
                        padding-right: 25px;
                   }"),
        

        ##########
        
        tabItems(
            
            tabItem(tabName = "plots",
                    
                    
                    fluidRow(
                                
                              box(title = "Main Panel", solidHeader = T, status = "primary", height = 700, width = 6, 
                                  plotOutput("my_plot" #, width = "600px", height = "600px"
                                             )),
                          
                          
                              box(title = "Text Adjust Panel", solidHeader = T, status = "primary", height = 750, width = 3,           
      
                                      sliderInput("xtxt", label = "X - Text Shift",
                                                  min = -5, max = 5, value = 0, step = 0.5),
      
                                      sliderInput("ytxt", label = "Y - Text Shift",
                                                  min = -5, max = 5, value = 0, step = 0.5),
                                  
                                      sliderInput("textpadding", label = "Text Padding",
                                                  min = 0, max = 2, value = 0.5, step = 0.1),
      
                                      sliderInput("pointpadding", label = "Point Padding",
                                                  min = 0, max = 2, value = 0.3, step = 0.1),
      
                                      sliderInput("xadj", label = "X - Text Adjust",
                                                  min = 0, max = 1, value = c(0.5), step = 0.25),
      
                                      sliderInput("yadj", label = "Y - Text Adjust",
                                                  min = 0, max = 1, value = c(0.5), step = 0.25),
                                  
                                      sliderInput("textsize", label = "Text Size",
                                                   min = 1, max = 10, value = 6, step = 0.5)),
                              
                              box(title = "Selection By Name Panel", solidHeader = T, status = "primary", height = 800, width = 3,
                                  
                                  textInput(inputId = "my_selection1", 
                                            label = "Select Group #1",
                                            value = "glu Cap-D3 Cap-H2 SMC2 Cap-D2 barr Cap-G"),   
                                  
                                  textInput(inputId = "my_name_color1", 
                                            label = "Color Group #1",
                                            value = "#56B4E9"),
                                  
                                  textInput(inputId = "my_selection2", 
                                            label = "Select Group #2",
                                            value = "Hmr Lhr"),   
                                  
                                  textInput(inputId = "my_name_color2", 
                                            label = "Color Group #2",
                                            value = "#009E73"),
                                  
                                  textInput(inputId = "my_selection3", 
                                            label = "Select Group #3",
                                            value = "vtd SMC1 SMC3 SA cid"),   
                                  
                                  textInput(inputId = "my_name_color3", 
                                            label = "Color Group #3",
                                            value = "#E69F00"),
                                  
                                  textInput(inputId = "my_selection4", 
                                            label = "Select Group #4",
                                            value = "Cenp-C"),   
                                  
                                  textInput(inputId = "my_name_color4", 
                                            label = "Color Group #4",
                                            value = "#D55E00"),
                                  
                                  textInput(inputId = "my_selection5", 
                                            label = "Select Group #5",
                                            value = "Su(var)205"),   
                                  
                                  textInput(inputId = "my_name_color5", 
                                            label = "Color Group #5",
                                            value = "#CC79A7")
                                  
                                  # checkboxInput(inputId = "text_name", 
                                  #               label = "Show Text by Name", 
                                  #               value = TRUE
                                  # ),
                                  # 
                                  # selectInput("order", label = "Color order", 
                                  #             choices = list("ydata","xdata","symbol")
                                  # ),
                                  

                                  
                                  )
                              

                    ),
                    
                    fluidRow(
                          
                          column(width = 3,
                                 
                                 
                                 box(title = "Title Panel", solidHeader = T, status = "primary", width = NULL,
                                     
                                     textInput(inputId = "xlab", 
                                               label = "X - Label",
                                               value = "log2 Fold Change"),
                                     
                                     
                                     textInput(inputId = "ylab", 
                                               label = "Y - Label",
                                               value = "-log10(p-value)"),
                                     
                                     
                                     textInput(inputId = "main_title", 
                                               label = "Plot Title",
                                               value = "Plot")
                                 ),
                                 
                                 box(title = "Transform Panel", solidHeader = T, status = "primary", width = NULL,
                                     
                                     selectInput("xtrans", 
                                                 label = "X - Transform", 
                                                 choices = list("No","log2","log10","-log10"), 
                                                 selected = "No"),
                                     
                                     selectInput("ytrans", 
                                                 label = "Y - Transform", 
                                                 choices = list("No","log2","log10","-log10"), 
                                                 selected = "-log10")
                                     
                                 )
                                 
                                 ),
                          
                          column(width = 3,
                                 
                                 
                                 
                                 box(title = "Limits Panel", solidHeader = T, status = "primary", width = NULL,
                                     
                                     sliderInput("xlims", label = "X - Limits",
                                                 min = -50, max = 50, value = c(-6,6)),
                                     
                                     sliderInput("ylims", label = "Y - Limits",
                                                 min = -50, max = 50, value = c(0,8))
                                     
                                 ),
                                 
                                 
                                 box(title = "Points Panel", solidHeader = T, status = "primary", width = NULL,
                                     
                                     textInput(inputId = "main_color", 
                                               label = "Main Color",
                                               value = "grey"),
                                     
                                     sliderInput("point_size", 
                                                 label = "Point Size",
                                                 min = 0, max = 3, value = 1, step = 0.25)
                                     
                                 )
                                 
                                 
                                 ),
                          
                          box(title = "Lines Panel", solidHeader = T, status = "primary", width = 3,
                              
                              
                              checkboxInput(inputId = "horizontal", 
                                            label = "Add Horizontal Line", 
                                            value = TRUE
                                            ),
                              
                              checkboxInput(inputId = "vertical", 
                                            label = "Add Vertical Line", 
                                            value = TRUE
                              ),
                              
                              checkboxInput(inputId = "slope", 
                                            label = "Add Slope Line ", 
                                            value = FALSE
                              ),
                              
                              sliderInput(inputId = "hor_line_pos", label = "Horizontal Line Position",
                                          min = -10, max = 10, value = 0, step = 0.5),
                              
                              
                             
                              sliderInput(inputId = "ver_line_pos", label = "Vertical Line Position",
                                          min = -10, max = 10, value = 0, step = 0.5),
                              
                              sliderInput(inputId = "slop_line_pos", label = "Line Slope",
                                          min = -1, max = 1, value = 1, step = 0.25)
                              
                              
                          ),
                          
                          box(title = "Selection By Value Panel", solidHeader = T, status = "primary", width = 3,
                              
                              
                              sliderInput("xthresh", label = "Select by X - Values",
                                          min = -50, max = 50, value = c(8,50), step = 0.5),
                              
                              
                              sliderInput("ythresh", label = "Select by Y - Values",
                                          min = -50, max = 50, value = c(8,50), step = 0.5),
                              
                              textInput(inputId = "my_val_color", 
                                        label = "Color By Value",
                                        value = "black"),
                              
                              checkboxInput(inputId = "text_val", 
                                            label = "Show Text By Value", 
                                            value = TRUE)
                          )
                          
                    )
            )
        )
    )
    
)







#################################################################################################################################


server <- function(input, output) { 
    
    


################### collect inputs ################### 
    
    my_data_columns <- reactive({
        
            inFile <- input$my_data
        
            if (is.null(inFile))
                  return(NULL)
            
            if(grepl("xlsx", inFile$datapath)){
                  read.xls(inFile$datapath, sheet = 1, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, na.strings = "")
            } else if(grepl("csv", inFile$datapath)){
                  read.csv(inFile$datapath, header = TRUE, row.names = NULL, stringsAsFactors = FALSE, na.strings = "")
            }
        
    
    })
    
    
    my_columns <- reactive({
          
          if(input$col_id != ""){
                my_columns <- c(input$col_symbol, input$col_xdata, input$col_ydata, input$col_id)
          } else {
                my_columns <- c(input$col_symbol, input$col_xdata, input$col_ydata)
          }

          my_columns <- gsub("\\-|\\(|\\)|\\:\\;",".", my_columns)
        
          my_columns
    })
    
    my_selection1 <- reactive({
          
          my_selection1 <- gsub(" ", ",", input$my_selection1)
          
          my_selection1 <- gsub("\\,\\,", ",", my_selection1)
          
          my_selection1 <- as.character(strsplit(my_selection1, split = ",")[[1]])
          
          my_selection1
    })
    
    my_selection2 <- reactive({
          
          my_selection2 <- gsub(" ", ",", input$my_selection2)
          
          my_selection2 <- gsub("\\,\\,", ",", my_selection2)
          
          my_selection2 <- as.character(strsplit(my_selection2, split = ",")[[1]])
          
          my_selection2
    })
    
    my_selection3 <- reactive({
          
          my_selection3 <- gsub(" ", ",", input$my_selection3)
          
          my_selection3 <- gsub("\\,\\,", ",", my_selection3)
          
          my_selection3 <- as.character(strsplit(my_selection3, split = ",")[[1]])
          
          my_selection3
    })
    
    my_selection4 <- reactive({
          
          my_selection4 <- gsub(" ", ",", input$my_selection4)
          
          my_selection4 <- gsub("\\,\\,", ",", my_selection4)
          
          my_selection4 <- as.character(strsplit(my_selection4, split = ",")[[1]])
          
          my_selection4
    })
    
    my_selection5 <- reactive({
          
          my_selection5 <- gsub(" ", ",", input$my_selection5)
          
          my_selection5 <- gsub("\\,\\,", ",", my_selection5)
          
          my_selection5 <- as.character(strsplit(my_selection5, split = ",")[[1]])
          
          my_selection5
    })

    


################### show table ################### 
    
    # output$my_table <- renderTable({
    #       
    #       
    #       example_out <- rbind(
    #             head(my_data_columns())
    #       )
    #      
    #       example_out 
    #       
    #       
    # },include.rownames=TRUE)
    
    
    
    

################### plot function  ################### 
    
    
          plotPlot <- function(){
                
                
                
                my_data_columns <- my_data_columns()
                
                
                my_columns <- my_columns()
                
                my_selection1 <- my_selection1()
                my_selection2 <- my_selection2()
                my_selection3 <- my_selection3()
                my_selection4 <- my_selection4()
                my_selection5 <- my_selection5()
                
                
                
                if(is.null(my_data_columns)){
                      # my_data_columns <- data.frame(symbol = paste0("example", 1:100),
                      #                               xdata = seq(-10,10, length.out = 100),
                      #                               ydata = 1.25^-(seq(-10,10, length.out = 100)^2),
                      #                               stringsAsFactors = FALSE
                      # )
                      
                      my_data_columns <- read.xls("test_data.xlsx", sheet = 1, 
                                                  header = TRUE, row.names = NULL, 
                                                  stringsAsFactors = FALSE, na.strings = "")
                      
                      # my_selection <- c("example88", "example86","example87")
                      # 
                } 
                
                #my_data_columns <- my_data_columns[complete.cases(my_data_columns),]
                
                my_data_columns <- my_data_columns[,colnames(my_data_columns) %in% my_columns]
                
                my_data_columns <- my_data_columns[order(my_data_columns[,my_columns[1]]),]
                
                
                # if(input$order == "symbol"){
                #       my_data_columns <- my_data_columns[order(my_data_columns[,my_columns[1]]),]
                # } else if(input$order == "xdata"){
                #       my_data_columns <- my_data_columns[order(my_data_columns[,my_columns[2]]),]
                # } else if(input$order == "xdata"){
                #       my_data_columns <- my_data_columns[order(my_data_columns[,my_columns[3]]),]
                # }
                
                
                #################################################
                
                
                my_names <- my_data_columns[,my_columns[1]]
                
                if(length(my_columns) == 4){
                      my_ids <- my_data_columns[,my_columns[4]]
                      my_names[is.na(my_names)] <- my_ids[is.na(my_names)]
                } else {
                      my_names[is.na(my_names)] <- paste0("uid", 1:sum(is.na(my_names)))
                }
   
                xdata <- my_data_columns[,my_columns[2]]
                
                
                if(input$xtrans == "log2"){
                      xdata <- log2(xdata)
                } else if(input$xtrans == "log10"){
                      xdata <- log10(xdata)
                } else if(input$xtrans == "-log10"){
                      xdata <- -log10(xdata)
                }
                
                
                
                
                ydata <- my_data_columns[,my_columns[3]]
                
                
                if(input$ytrans == "log2"){
                      ydata <- log2(ydata)
                } else if(input$ytrans == "log10"){
                      ydata <- log10(ydata)
                } else if(input$ytrans == "-log10"){
                      ydata <- -log10(ydata)
                }
                
                
                #################################################                
                
                #input$goButton
                
                my_selection0 <- (my_names[ (xdata > input$xthresh[1] &  xdata < input$xthresh[2]) & 
                                                   (ydata > input$ythresh[1] & ydata < input$ythresh[2])] )
                
                dat2 <- data.frame(xdata, 
                                   ydata, 
                                   my_names = "", 
                                   my_name_colors = "", 
                                   stringsAsFactors = FALSE)
                
                dat2$my_names[my_names %in% my_selection0] <- my_names[my_names %in% my_selection0]
                dat2$my_names[my_names %in% my_selection1] <- my_names[my_names %in% my_selection1]
                dat2$my_names[my_names %in% my_selection2] <- my_names[my_names %in% my_selection2]
                dat2$my_names[my_names %in% my_selection3] <- my_names[my_names %in% my_selection3]
                dat2$my_names[my_names %in% my_selection4] <- my_names[my_names %in% my_selection4]
                dat2$my_names[my_names %in% my_selection5] <- my_names[my_names %in% my_selection5]

                if(input$text_val){
                      dat2$my_name_colors[my_names %in% my_selection0] <- input$my_val_color
                } else {
                      dat2$my_name_colors[my_names %in% my_selection0] <- "#00000000"
                }
                
                dat2$my_name_colors[my_names %in% my_selection1] <- input$my_name_color1
                dat2$my_name_colors[my_names %in% my_selection2] <- input$my_name_color2
                dat2$my_name_colors[my_names %in% my_selection3] <- input$my_name_color3
                dat2$my_name_colors[my_names %in% my_selection4] <- input$my_name_color4
                dat2$my_name_colors[my_names %in% my_selection5] <- input$my_name_color5

                

                ggplot(dat2, aes(xdata, ydata, label = my_names)) +
                      theme_bw(base_size = 22) +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                      ggtitle(input$main_title) +
                      theme(plot.title = element_text(hjust = 0.5)) +
                      #theme(axis.text=element_text(size=18),axis.title=element_text(size=22)) +
                      xlim(input$xlims) +
                      ylim(input$ylims) +
                      xlab(input$xlab) +
                      ylab(input$ylab) +
                      geom_point(data = dat2,  color = input$main_color, size = input$point_size) +
                      geom_point(data = dat2[my_names %in% my_selection0,],  color = input$my_val_color, size = input$point_size*1.5) +
                      geom_point(data = dat2[my_names %in% my_selection1,],  color = input$my_name_color1, size = input$point_size*1.5) +
                      geom_point(data = dat2[my_names %in% my_selection2,],  color = input$my_name_color2, size = input$point_size*1.5) +
                      geom_point(data = dat2[my_names %in% my_selection3,],  color = input$my_name_color3, size = input$point_size*1.5) +
                      geom_point(data = dat2[my_names %in% my_selection4,],  color = input$my_name_color4, size = input$point_size*1.5) +
                      geom_point(data = dat2[my_names %in% my_selection5,],  color = input$my_name_color5, size = input$point_size*1.5) +
                      geom_text_repel(box.padding = input$textpadding, 
                                      point.padding = input$pointpadding, 
                                      direction = "both", 
                                      nudge_x = input$xtxt,
                                      nudge_y = input$ytxt,
                                      hjust = input$xadj,
                                      vjust = input$yadj,
                                      #segment.color = "black",
                                      segment.size = 0.2,
                                      color = dat2$my_name_colors,
                                      size = input$textsize) +
                      geom_vline(xintercept = input$ver_line_pos, linetype=2, color =  ifelse(input$vertical, "grey22", "#00000000")) +
                      geom_hline(yintercept = input$hor_line_pos, linetype=2, color =  ifelse(input$horizontal, "grey22", "#00000000")) +
                      geom_abline(intercept = 0, slope =  input$slop_line_pos, linetype=2, color =  ifelse(input$slope, "grey22", "#00000000"))
                
                
                
                
                # par(oma=c(0,0,0,0), mar=c(5,5,3,3))
                # 
                # 
                # plot(x = xdata,
                #      y = ydata, 
                #      xlim = input$xlims,  ylim = input$ylims,
                #      pch = 19, col = input$main_color, cex= input$point_size, 
                #      cex.lab = 1.5, cex.axis = 1.25, cex.main = 1.75,
                #      main = input$main_title, xlab = input$xlab, ylab = input$ylab)
                # 
                # 
                # if(input$horizontal){
                #       abline(h = input$hor_line_pos, lty=2)
                # }
                # 
                # if(input$vertical){
                #       abline(v = input$ver_line_pos, lty=2)
                # }
                # 
                # if(input$slope){
                #       abline(coef = c(0, input$slop_line_pos), lty=2)
                # }
                # 
                # 
                # 
                # 
                # my_subset1 <- my_names %in% my_selection
                # 
                # my_subset2 <-  my_names %in% my_names[ (xdata > input$xthresh[1] & xdata < input$xthresh[2]) & (ydata > input$ythresh[1] & ydata < input$ythresh[2])]  
                # 
                #                 
                # if(sum((my_subset1 | my_subset2)) > 0){
                #       
                #       
                #       point_xcoord <- xdata[my_subset1 | my_subset2]
                #       point_ycoord <- ydata[my_subset1 | my_subset2]
                #       
                #       
                #       text_labels <- my_names[my_subset1 | my_subset2]
                #       
                # 
                #       points(point_xcoord,
                #              point_ycoord,
                #              pch = 19, cex = input$point_size * 1.5, 
                #              col = ifelse(text_labels %in% my_names[my_subset1], input$my_name_color, input$my_val_color) )
                # 
                #       
                #       
                #       text_xcoord <- xdata[my_subset1 | my_subset2]
                #       
                #       o <- order(text_xcoord, decreasing = FALSE)
                #       ro <- order(o)
                #       
                #       text_xcoord <- text_xcoord[o] 
                #                       
                #       text_xcoord <- text_xcoord + c(seq(0, input$xtxt, length.out = length(text_xcoord)/2), seq(input$xtxt,0, length.out = (length(text_xcoord)-1)/2))
                #       
                #       text_xcoord <- (text_xcoord + seq(input$xrepel[1], input$xrepel[2], length.out = length(text_xcoord)))
                #       
                #       
                #       text_xcoord <- text_xcoord[ro]
                #       
                #       text_ycoord <- ydata[my_subset1 | my_subset2]
                #       
                #       o <- order(text_ycoord, decreasing = FALSE)
                #       ro <- order(o)
                #       text_ycoord <- (text_ycoord[o] + seq(input$yrepel[1], input$yrepel[2], length.out = length(text_ycoord)))[ro]
                #       
                #       
                #       
                #       arrows(x0 = point_xcoord, y0 = point_ycoord, 
                #              x1 = text_xcoord,#+input$xtxt, 
                #              y1 = text_ycoord+input$ytxt, 
                #              length = 0, 
                #              col = ifelse(text_labels %in% my_names[my_subset1],
                #                           ifelse(input$text_name, "black", "#00000000"), 
                #                           ifelse(input$text_val, "black", "#00000000")))
                #       
                #       text(x = text_xcoord,#+input$xtxt,
                #            y = text_ycoord+input$ytxt, 
                #            labels =  text_labels, 
                #            col = ifelse(text_labels %in% my_names[my_subset1],
                #                         ifelse(input$text_name, input$my_name_color, "#00000000"), 
                #                         ifelse(input$text_val, input$my_val_color, "#00000000")), 
                #            adj = c(input$xadj,input$yadj))
                   
                      
                #}

          }
          
    
    
################### plot Plot ################### 
    
    
        output$my_plot <- renderPlot({
              
             input$goButton
              
             print(isolate(plotPlot()))
             
        }, height = 600)
    

################### pdf Plot ################### 
    
      output$pdf_download = downloadHandler(
          filename = "plot.pdf",
          content = function(file) {
                
                ggsave(filename = file, 
                       plot =  plotPlot(),
                       device = "pdf", 
                       height = as.numeric(input$plotheight), 
                       width = as.numeric(input$plotwidth)
                       )
                

          })  
        
    
}







#################################################################################################################################


shinyApp(ui = ui, server = server)

