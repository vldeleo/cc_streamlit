#

library(shiny)
library(dplyr)
library(ggplot2)
library(factoextra)
library(MetBrewer)
library(arrow)
library(lubridate)
library(maps)
library(factoextra)
library(ggrepel)
options(scipen=999)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Credit Fraud Alert: a DS4A project output"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(fluid = FALSE,
                     width = 3,
                     tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
            dateRangeInput("date1",
                      "Date Range:",
                      start = "2016-01-01",
                      end = "2016-12-31",
                      min = "2015-12-31",
                      max = "2019-12-30",
                      format = "yyyy-mm-dd"),
            sliderInput("probThresh",
                        "Detection Probability Threshold (%):",
                        min = 50,
                        max = 100,
                        value = 75),
            fluidRow(
              column(width = 5,
                     selectInput("axis1", 
                              "Select Principal Component X:", 
                               choices = c("1", "2", "3", "4", "5", "6"),
                               selected = "1")),
              column(width = 5,
                     selectInput("axis2", 
                                 "Select Principal Component Y:", 
                                 choices = c("1", "2", "3", "4", "5", "6"),
                                 selected = "2"))
            ),
        radioButtons("customergroups",
                     "Customer Demographic:",
                     choices = c("Gender" = "Gender",
                       "Age" = "Age",
                       "Income" = "Income $",
                       "Total Debt" = "Total Debt",
                       "Debt/Income" = "Debt/Income",
                       "Credit Score" = "FICO Score",
                       "Region" = "Region"),
                       selected = "Gender"
                     )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            column(width = 3,
                   h3("# Alerts: \n "),
                   textOutput("Alerts"),
                   tags$head(tags$style("#Alerts{color: black;
                                 font-size: 30px;
            font-style: bold;
            }"))),
            column(width = 3,
                   h3("# Fraudulent Transactions: "),
                   textOutput("Frauds"),
                   tags$head(tags$style("#Frauds{color: black;
                                 font-size: 30px;
            font-style: bold;
            }"))),
            column(width = 3,
                   h3("% Fraudulent Transactions: "),
                   textOutput("percTrans"),
                   tags$head(tags$style("#percTrans{color: black;
                                 font-size: 30px;
            font-style: bold;
            }"))),
            column(width=3,
                   h3("Cost of Fraud:"),
                   textOutput("moneyLost")),
            tags$head(tags$style("#moneyLost{color: black;
                                 font-size: 30px;
            font-style: bold;
            }"))
          ),
         fluidRow(
            column(width = 12,
                   h3("Timeline of Fraud Occurrences"),
                   plotOutput("ErrorLinechart"))
            ),# type I, II, and true pos/negs
            fluidRow(
              column(width = 8,
                     h3("Map of Fraud Occurrences"),
                     plotOutput("FraudMap")), # true positives and false negatives?
              column(width = 4,
                     h3("Errors in Prediction"),
                     plotOutput("ErrorPiechart"))
            )   ,
          fluidRow(
            column(width = 6,
                   h3("Anomalies in Transaction Characteristics"),
                   plotOutput("BiPlotFraud")
                   ),
            column(width = 6,
                   h3("Features Contributing to Anomalies"),
                   plotOutput("factorContrib")
                   #textOutput("test")#,
             )
          ),
         fluidRow(
           column(width = 6,
                  h3("Anomalies by Customer Demographic"),
                  plotOutput("PCAforDemo")
                  ),
           column(width = 6,
                  h3("Risk Factor by Demographic"),
                  plotOutput("segbyrisk")
                  )
         )
           
           #plotOutput("FraudAlert"), # Not sure how this will differ
           #plotOutput("ContributionFactors"),
           #plotOutput("PCAforDemo"),
           #plotOutput("ClusteringFraud")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  regions <- data.frame(
    state = c("AK", "HI", "CA", "OR", "WA", "WY", "CO", "MT","ID", "NV", "NM", "UT", "AZ", "ND", "SD", "NY", "MA", "NJ", "DE", "CT", "NH", "ME","RI", "VT", "PA","MD", "TX", "GA", "AL", "AR", "MS", "TN", "VA", "LA", "WV", "SC", "NC", "KY", "FL", "OH", "MO", "IL", "IN", "OK", "WI", "KS", "NE", "MI", "IA", "MN", "DC"),
    region = c(rep("Pacific", 5), rep("West", 10), rep("Northeast",11), rep("South", 13), rep("Midwest", 11), "DC")
  )

    cc_data <- read_parquet("C:/Users/torie/Documents/Python Scripts/Streamlit/cc_data_subset.parquet")
      # I was trying to do this on the subsetted dataframe, but no reason why I can't just pre-process this way
    cc_data$`Use Chip` <- ifelse(cc_data$`Use Chip` == "Chip Transaction", 1, 0) 
    cc_data$`Errors?` <- ifelse(cc_data$`Errors?` == "No", 0, 1 )
    cc_data$`Card Type` <- ifelse(cc_data$`Card Type` == "Credit", 0, 1)
    cc_data$`Has Chip` <- ifelse(cc_data$`Has Chip` == "YES", 1, 0)
    cc_data$Expires <- as_date(cc_data$Expires, format = "%m/%Y")
    cc_data$time_of_day <- as.numeric(as.factor(cc_data$time_of_day))
    cc_data$DatestoExp <- as.numeric(cc_data$Expires - as_date(trunc(cc_data$datetime, "months")))
    cc_data$GeoDiff <- as.numeric(cc_data$GeoDiff)

    
    #####################################################################
    #date1 <- reactive({input$date1[1]})
    
    cc_sub <- reactive({
      req(input$date1)
      cc_data %>% filter(date > input$date1[1] & date < input$date1[2]) #
      }) 
  
    output$Frauds <- renderText({
      #paste("# of Fraudulent Transactions: \n", sum(cc_data$`Is Fraud?` == "Yes" & cc_data$date > input$date1[1] & cc_data$date < input$date1[2]), sep = "")
      sum(cc_data$`Is Fraud?` == "Yes" & cc_data$date > input$date1[1] & cc_data$date < input$date1[2])
    })
    
    output$Alerts <- renderText({
      #paste("# of Alerts: \n", sum(cc_data$probFraud > input$probThresh/100 & cc_data$date > input$date1[1] & cc_data$date < input$date1[2]), sep = "")
      sum(cc_data$probFraud > input$probThresh/100 & cc_data$date > input$date1[1] & cc_data$date < input$date1[2])
    })
    
    output$percTrans <- renderText({
      #paste("% Transactions Fraudulent: \n", round(sum(cc_data$`Is Fraud?` == "Yes" & cc_data$date > input$date1[1] & cc_data$date < input$date1[2])/nrow(cc_data[which(cc_data$date > input$date1[1] & cc_data$date < input$date1[2]),])*100,2), sep = "")
      round(sum(cc_data$`Is Fraud?` == "Yes" & cc_data$date > input$date1[1] & cc_data$date < input$date1[2])/nrow(cc_data[which(cc_data$date > input$date1[1] & cc_data$date < input$date1[2]),])*100,2)
    })
    
    output$moneyLost <- renderText({
      paste("$", round(sum(cc_data[which(cc_data$`Is Fraud?` == "Yes" & cc_data$date > input$date1[1] & cc_data$date < input$date1[2]),'Amount']),0), sep = "")
    })
    
    
    output$Test <- renderText({
      #class(input$date1[1])
      #dim(cc_sub())
      #colnames(cc_sub())
      DateFraud = as.Date(cc_sub()$date, "%Y-%m-%d")
      class(DateFraud)
    })
    
    
 ###########################################################################   
    
    
    
    # geography!
    output$FraudMap <- renderPlot({
      plot(cc_sub()$Longitude, cc_sub()$Latitude, col = alpha("black",0.01), pch = 20, xlab = "", ylab = "")
      points(cc_sub()$Longitude[cc_sub()$`Is Fraud?`=="Yes"], cc_sub()$Latitude[cc_sub()$`Is Fraud?`=="Yes"], col = "orange", lwd=3)
     map(add =T)
     legend("bottomleft", legend = c("Transaction", "Fraud"), col = c("black", "orange"), pch = 19)
    })
    
    
    #now let's plot timeline/line chart!
    output$ErrorLinechart <- renderPlot({
    DateFraud = as.Date(cc_sub()$date, "%Y-%m-%d")#[cc_sub()$`Is Fraud?`=="Yes"]
    ggplot() +   
      stat_count(geom='line', aes(x = as.Date(cc_sub()$date, "%Y-%m-%d"), y=..count..), col = "gray") +
      geom_segment(
        aes(x = min(DateFraud) - 1
            , xend = max(DateFraud) + 1
            , y = 0
            , yend = 0)
        , arrow = arrow()
      ) +
      #geom_line(aes(y = count(cc_sub()), x = DateFraud)) +
      geom_linerange(
        aes(x = DateFraud[cc_sub()$`Is Fraud?`=="Yes" & cc_sub()$probFraud > input$probThresh/100]
           , ymin = 0
           , ymax = 100, col = "True Positive")
           ) + # true positives
      geom_linerange(
        aes(x = DateFraud[cc_sub()$`Is Fraud?`=="Yes" & cc_sub()$probFraud < input$probThresh/100]
            , ymin = 0
            , ymax = 100, col = "False Negative")
        ) +     # false negatives
      geom_linerange(
        aes(x = DateFraud[cc_sub()$`Is Fraud?`=="No" & cc_sub()$probFraud > input$probThresh/100]
            , ymin = 0
            , ymax = 100, 
            col = "False Positive")
        ) +       # false positives
      xlab("") +
      ylab("Transactions")+
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major.y = element_blank()#,
            #panel.grid.minor.y = element_blank()
      ) +
      scale_x_date(date_breaks = "1 month"
                   , date_labels = "%Y\n%b-%d") +
      scale_color_manual(name='Alerts',
                         breaks=c('All Transactions', 'True Positive', 'False Negative', 'False Positive'),
                         values=c('All Transactions'='gray', 'True Positive'='black', 'False Negative'='red', 'False Positive'='royalblue'),
                         drop = FALSE,
                         limits = c('True Positive', 'False Negative', 'False Positive')) +
      theme(legend.title=element_text(size=20),
            legend.text=element_text(size=14))
    })
    
    
    
    
    
    
    # and pie chart?
    output$ErrorPiechart <- renderPlot({
      TruePos = sum(cc_sub()$`Is Fraud?` == "Yes" & cc_sub()$probFraud > input$probThresh/100)
      TrueNeg = sum(cc_sub()$`Is Fraud?` == "No" & cc_sub()$probFraud < input$probThresh/100)
      FalsePos = sum(cc_sub()$`Is Fraud?` == "No" & cc_sub()$probFraud > input$probThresh/100)
      FalseNeg = sum(cc_sub()$`Is Fraud?` == "Yes" & cc_sub()$probFraud < input$probThresh/100)
      
      piedf <- data.frame("category" = c('True Positive', 'True Negative', 'False Positive', 'False Negative'),
                         "amount" = c(TruePos, TrueNeg, FalsePos, FalseNeg))
      
      ggplot(piedf, aes(x="", y=amount, fill=category)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        scale_fill_manual(values = c("red","royalblue", "lightgray", "darkgray"))+
        #geom_text(aes(label = paste0(amount/nrow(cc_sub())*100, "%")), position = position_stack(vjust=0.5)) +
        geom_label_repel(aes(label = paste0(round(amount/nrow(cc_sub())*100,2), "%")),
                        size = 4.5, nudge_x = 1.5, show.legend = FALSE)+
        labs(fill = "Classification") +
        theme_void()
    })

    
    

    #############################################################
    # "clustering"
    
      #trans.pca <- prcomp(transAnomalies[complete.cases(transAnomalies),c(1,3:5,7:10,12,13,16)], scale=TRUE)
    #PC <- as.data.frame(trans.pca$x)
    #PC$`Fraud?` <- as.vector(transAnomalies[, 'Is Fraud?'])
    
    PC.reactive <- reactive({
      #cc_sub()[,c(2,6,7,8,14,19,20,21, 22,47,49,51, 65,15, 68)]
     #as.data.frame( prcomp(cc_sub()[,c(2,7,8,14,20,21,22, 47, 51,65)])$x )
    as.data.frame(prcomp(cc_sub()[,c("Card","target", "Amount","Use Chip","Errors?","time_of_day", "Card Type", "Has Chip", "GeoDiff", "DatestoExp","years_since_pin_change" )], scale = TRUE)$x) # pin change seems more like customer feature, but whatever
      #"charge_off",
      
    })
    
    output$BiPlotFraud <- renderPlot({
      req(input$date1)
      bespokecolor <- c("black", "orange")
      
      plot(PC.reactive()[,as.numeric(input$axis1)], PC.reactive()[,as.numeric(input$axis2)], xlab = paste("PC ", input$axis1), ylab = paste("PC ", input$axis2), col = alpha(bespokecolor[as.factor(cc_sub()[,'Is Fraud?'] == "Yes")], 0.25), pch = 19)
      legend("bottomleft", legend = c("Not Fraud", "Fraud"), col = c("black", "orange"), pch = 19)
      
      
      #prcomp(cc_sub()[,c("Card","Amount","Use Chip","Errors?","time_of_day", "target","charge_off","Card Type", "Has Chip", "GeoDiff", "DatestoExp","years_since_pin_change" )], scale = TRUE) %>%
      #fviz_pca_biplot( 
      #                repel = TRUE,
      #                col.var = "gray",
      #                fill.ind = "black",
      #                geom="point") +
      #  geom_point(.[cc_sub()[,'Is Fraud?'] == "Yes",], col = "red")
      
    })
    
    
    
    # how much does each column contribute to PC?
    
    pca.load <- reactive({
      pca.load <- as.data.frame(prcomp(cc_sub()[,c("Card","target", "Amount","Use Chip","Errors?","time_of_day", "Card Type", "Has Chip", "GeoDiff", "DatestoExp","years_since_pin_change" )], scale = TRUE)$rotation) # charge off feels like cheating... "charge_off",
    })
    
    output$factorContrib <- renderPlot({
      #car.pca$rotation[, 1] * (mtcars[1,] - summary(car.pca)$center) / summary(car.pca)$scale
      
      aload <- abs(pca.load())
      readytoplot <- sweep(aload, 2, colSums(aload), "/") 
      
      readytoplot %>%
        #mutate(Xlabels = rownames(aload)) %>%
        mutate(Xlabels = c("Card","Target", "Transaction $", "Chip Used?", "Errors?", "Time of Transaction", "Type of Card", "Has Chip?", "Address Discrepancy", "Days to Expiration","Years Since Pin Change" )) %>%
        select(as.numeric(input$axis1), Xlabels) %>%
        ggplot(aes(x = reorder(Xlabels, -.[,1], sum), y = .[,1])) +
        geom_bar(stat = "identity") +
        theme_bw() +
        labs(y = paste("Proportion Contribution to PC ", input$axis1), x = "") +
        theme(axis.text.x = element_text(angle = 70, vjust = 0.85, hjust=1))
      
    })
    
    
    
    
    output$test <- renderText({
      #input$customergroups == "male"
      aload <- abs(pca.load())
      readytoplot <- sweep(aload, 2, colSums(aload), "/") 
      orderfactor <- readytoplot %>%
        select(as.numeric(input$axis1)) %>%
        arrange(desc(.[,as.numeric(input$axis1)])) %>%
        rownames()
      indexcol <- which(colnames(cc_sub()) == orderfactor[1])
      #str(indexcol)
     #length(as.data.frame(cc_sub()[,as.numeric(indexcol)])[,1])
     class(as.data.frame(cc_sub()[,which(colnames(cc_sub()) == orderfactor[1])]))
      
    })
    
  output$PCAforDemo <- renderPlot({
    
    Label = "black"
     if (input$customergroups == "Gender") {
       Label <- cc_sub()$Gender
       } 

     if (input$customergroups == "Age") {
       Label <- data.frame(cc_sub()$`Current Age`, bin=cut(cc_sub()$`Current Age`, c(18,30,40,50,60,80,110), include.lowest=TRUE))$bin
       #Label <- vector(cc_sub()$`Current Age`, bin=cut(dataset, c(18,30,40,50,60,80,110), include.lowest=TRUE))
     }
    
      if (input$customergroups == "Income $") {
       Label <- data.frame(cc_sub()$`Yearly Income - Person`, bin=cut(cc_sub()$`Yearly Income - Person`, c(0,25000,45000,65000,90000,125000,40000), include.lowest=TRUE))$bin
      }
      if (input$customergroups == "Region") {
        Label <- regions$region[match(cc_sub()$State, regions$state)]
      }
      if (input$customergroups == "FICO Score"){
        Label <- data.frame(cc_sub()$`FICO Score`, bin=cut(cc_sub()$`FICO Score`, c(0,550, 650, 750, 850), include.lowest=TRUE))$bin
      }
      if (input$customergroups == "Debt/Income"){
        Label <- data.frame(cc_sub()$total_debt_personal_income_ratio, bin=cut(cc_sub()$total_debt_personal_income_ratio, c(0,1,2,3,4), include.lowest=TRUE))$bin
      }
    if (input$customergroups == "Total Debt"){
      Label <- data.frame(cc_sub()$`Total Debt`, bin=cut(cc_sub()$`Total Debt`, c(0,25000,50000,100000,200000,500000), include.lowest=TRUE))$bin
    }
      xvar = as.character(colnames(PC.reactive())[as.numeric(input$axis1)])
      yvar = as.character(colnames(PC.reactive())[as.numeric(input$axis2)])
    
      ggplot(data = PC.reactive(),
      aes(x = PC.reactive()[,as.numeric(input$axis1)], 
          y = PC.reactive()[,as.numeric(input$axis2)]
          , color = as.factor(Label)
          )) +
      geom_point(alpha = 0.25) +
      #scale_color_manual(values=met.brewer("Lakota")) +
      stat_ellipse(show_guide = FALSE) + 
      labs(x = paste("PC ", input$axis1), y = paste("PC ", input$axis2), col = input$customergroups) +
        scale_color_manual(values=met.brewer("Austria")) +
      theme_bw()
    })
    
    
  
  
  output$segbyrisk <- renderPlot({
    if (input$customergroups == "Gender") {
      Label <- cc_sub()$Gender
    } 
    
    if (input$customergroups == "Age") {
      Label <- data.frame(cc_sub()$`Current Age`, bin=cut(cc_sub()$`Current Age`, c(18,30,40,50,60,80,110), include.lowest=TRUE))$bin
      #Label <- vector(cc_sub()$`Current Age`, bin=cut(dataset, c(18,30,40,50,60,80,110), include.lowest=TRUE))
    }
    
    if (input$customergroups == "Income $") {
      Label <- data.frame(cc_sub()$`Yearly Income - Person`, bin=cut(cc_sub()$`Yearly Income - Person`, c(0,25000,45000,65000,90000,125000,40000), include.lowest=TRUE))$bin
    }
    if (input$customergroups == "Region") {
      Label <- regions$region[match(cc_sub()$State, regions$state)]
    }
    if (input$customergroups == "FICO Score"){
      Label <- data.frame(cc_sub()$`FICO Score`, bin=cut(cc_sub()$`FICO Score`, c(0,550, 650, 750, 850), include.lowest=TRUE))$bin
    }
    if (input$customergroups == "Debt/Income"){
      Label <- data.frame(cc_sub()$total_debt_personal_income_ratio, bin=cut(cc_sub()$total_debt_personal_income_ratio, c(0,1,2,3,4), include.lowest=TRUE))$bin
    }
    if (input$customergroups == "Total Debt"){
      Label <- data.frame(cc_sub()$`Total Debt`, bin=cut(cc_sub()$`Total Debt`, c(0,25000,50000,100000,200000,500000), include.lowest=TRUE))$bin
    }
    #cc_sub() %>%
      #mutate(agebin = data.frame(cc_sub()$`Current Age`, bin=cut(cc_sub()$`Current Age`, c(18,30,40,50,60,80,110), include.lowest=TRUE))$bin) %>%
      #ggplot(aes(x = agebin, y = `years_since_pin_change`)) +
      #geom_boxplot() +
      #labs(x = "Age", y = "Years Since Pin Change") +
      #theme_bw()
    
    aload <- abs(pca.load())
    readytoplot <- sweep(aload, 2, colSums(aload), "/") 
    orderfactor <- readytoplot %>%
      select(as.numeric(input$axis1)) %>%
      arrange(desc(.[,1])) %>%
      rownames()
    indexcol <- which(colnames(cc_sub()) == orderfactor[1])
    Ydata <- as.data.frame(cc_sub()[,which(colnames(cc_sub()) == orderfactor[1])])
    
    data.frame(Ydata = as.numeric(unlist(Ydata)), 
               Label = Label) %>%
    ggplot(aes(y = Ydata, x = as.factor(Label), col = as.factor(Label))) + #cc_sub()[,indexcol]
      geom_violin(lwd = 2, alpha = 0.5) +
      geom_jitter(height = 0.02, width = 0.25, show_guide = FALSE, alpha = 0.25) +
      labs(y = orderfactor[1], col = input$customergroups, x = "") +
      scale_color_manual(values=met.brewer("Austria"))+
      theme(axis.text.x = element_blank()
            , axis.title.x = element_blank()
      ) +
      theme_bw()
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
