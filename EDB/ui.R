library(shiny)
library(ggplot2)
library(DBI)
library(RMySQL)
con = dbConnect(MySQL(), user='sesom_2015', password='sesom_2015', db='faith', host='mtekib.cyil3u7sobwf.us-west-2.rds.amazonaws.com')
query1 = 'SELECT DISTINCT county from indicators4'
query= 'SELECT county, indicator, mark from indicators4'
result1 = dbGetQuery(con, query1)
result = dbGetQuery(con, query)
county <- result1$county
county
category <- c("Infrastructure" , "Land" , "Taxation" , "Finance")
category
electricity.coverage <- subset(result, subset=(result$indicator == "electricitycoverage" ))$mark
electricity.coverage
electricity.score <- (pnorm(electricity.coverage, mean(electricity.coverage), sd(electricity.coverage), lower.tail=TRUE, log.p=FALSE)*100)
electricity.score
bitumen <- subset(result, subset=(result$indicator == "bitumen" ))$mark
bitumen
gravel <- subset(result, subset=(result$indicator == "gravel" ))$mark
gravel
earth <- subset(result, subset=(result$indicator == "earth" ))$mark
earth
road <- bitumen/(bitumen+gravel+earth)
road
road.score <- (pnorm(road, mean(road), sd(road), lower.tail=TRUE, log.p= FALSE)*100)
road.score
Infrastructure<-  round((road.score+electricity.score)/2, digits=2)
Infrastructure 
land.rates <- subset(result, subset=(result$indicator == "land rate" ))$mark
land.rates
Land <- round((pnorm(land.rates, mean(land.rates), sd(land.rates), lower.tail=FALSE, log.p=FALSE)*100), digits=2)
Land
hypermarket.rates <- subset(result, subset=(result$indicator == "hyper supermarket permit" ))$mark
hypermarket.rates
hypermarket.score <- (pnorm(hypermarket.rates, mean(hypermarket.rates), sd(hypermarket.rates), lower.tail=FALSE, log.p=FALSE)*100)
hypermarket.score
megasupermarket.rates <- subset(result, subset=(result$indicator == "mega supermarket permit" ))$mark
megasupermarket.rates
megasupermarket.score <- (pnorm(megasupermarket.rates, mean(megasupermarket.rates), sd(megasupermarket.rates), lower.tail=FALSE, log.p=FALSE)*100)
megasupermarket.score
largetradershop.rates <- subset(result, subset=(result$indicator == "large trader shop permit" ))$mark
largetradershop.rates
largetradershop.score <- (pnorm(largetradershop.rates, mean(largetradershop.rates), sd(largetradershop.rates), lower.tail=FALSE, log.p=FALSE)*100)
largetradershop.score
mediumtradershop.rates <- subset(result, subset=(result$indicator == "medium trader shop permit" ))$mark
mediumtradershop.rates
mediumtradershop.score <- (pnorm(mediumtradershop.rates, mean(mediumtradershop.rates), sd(mediumtradershop.rates), lower.tail=FALSE, log.p=FALSE)*100)
mediumtradershop.score
smalltradershop.rates <- subset(result, subset=(result$indicator == "small trader shop permit" ))$mark
smalltradershop.rates
smalltradershop.score <- (pnorm(smalltradershop.rates, mean(smalltradershop.rates), sd(smalltradershop.rates), lower.tail=FALSE, log.p=FALSE)*100)
smalltradershop.score
kiosk.rates <- subset(result, subset=(result$indicator == "kiosk permit" ))$mark
kiosk.rates
kiosk.score <- (pnorm(kiosk.rates, mean(kiosk.rates), sd(kiosk.rates), lower.tail=FALSE, log.p=FALSE)*100)
kiosk.score
largeindustrialplant.rates <- subset(result, subset=(result$indicator == "large industrial plant permit" ))$mark
largeindustrialplant.rates
largeindustrialplant.score <- (pnorm(largeindustrialplant.rates, mean(largeindustrialplant.rates), sd(largeindustrialplant.rates), lower.tail=FALSE, log.p=FALSE)*100)
largeindustrialplant.score
mediumindustrialplant.rates <- subset(result, subset=(result$indicator == "medium industrial plant permit" ))$mark
mediumindustrialplant.rates
mediumindustrialplant.score <- (pnorm(mediumindustrialplant.rates, mean(mediumindustrialplant.rates), sd(mediumindustrialplant.rates), lower.tail=FALSE, log.p=FALSE)*100)
mediumindustrialplant.score
smallindustrialplant.rates <- subset(result, subset=(result$indicator == "small industrial plant permit" ))$mark
smallindustrialplant.rates
smallindustrialplant.score <- (pnorm(smallindustrialplant.rates, mean(smallindustrialplant.rates), sd(smallindustrialplant.rates), lower.tail=FALSE, log.p=FALSE)*100)
smallindustrialplant.score
taxation.table<- data.frame(hypermarket.score,megasupermarket.score,largetradershop.score, mediumtradershop.score,smalltradershop.score, kiosk.score,largeindustrialplant.score, mediumindustrialplant.score, smallindustrialplant.score)
taxation.table
Taxation <- round(apply(taxation.table, 1, mean), digits=2)
Taxation
banks <- subset(result, subset=(result$indicator == "bankbranches" ))$mark
banks
population <-as.numeric (subset(result, subset=(result$indicator == "population" ))$mark)
population
expenditure <- as.numeric (subset(result, subset=(result$indicator == "meanhouseholdexpenditure" ))$mark)
expenditure
finance.services <- banks/(population*expenditure)
finance.services
Finance <- round((pnorm(finance.services, mean(finance.services), sd(finance.services), lower.tail=TRUE, log.p=FALSE)*100), digits=2)
Finance
mytable <- data.frame(Infrastructure, Land, Taxation, Finance)
mytable
overall <- round(rowMeans(mytable[,-1]),digits=2)
overall.table <- cbind(mytable, overall)
rownames(overall.table)<- county
table <- overall.table[order(-overall.table$overall), ]
table
shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
                    color: red;
                    }
                    "))
    ),
  
  
  titlePanel("Ease of Doing Business"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("counties", "County:", county, selected=county[1:5], inline=TRUE),
      checkboxGroupInput("vars", "Category:", category, selected= category),
      strong("Indicators:"),
      br(),
      strong("1.INFRASTRUCTURE"),
      p("  -Electricity Coverage"),
      p("  -Length of Bitumen road"),
      p("-Length of Gravel road"),
      p("-Length of Earth road"),
      br(),
      strong("2.LAND"),
      p("-Land Rate"),
      br(),
      strong("3. TAXATION"),
      div("-Single Business Permit License Fees"),
      br(),
      strong("4. FINANCE"),
      p("-Number of Bank branches"),
      p("-Population"),
      p("-Mean household expenditure")
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Main",DT::dataTableOutput("mytable")) , 
                  tabPanel("Interactive dashboard",tableOutput("mytable3"), plotOutput("myplot7")),
                  tabPanel("Categories", plotOutput("myplot4"),plotOutput("myplot8")),
                  tabPanel("Counties", plotOutput("myplot10"))
                  
                  
                  
      
      
    )
  )
)))
