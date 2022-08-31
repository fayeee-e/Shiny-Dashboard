library(shiny)
library(ggplot2)
library(DT) 
library(DBI)
library(RMySQL)
con = dbConnect(MySQL(), user='', password='', db='faith', host='mtekib.cyil3u7sobwf.us-west-2.rds.amazonaws.com')
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
Overall <- round(apply(mytable, 1, mean), digits=2)
overall.table <- cbind(mytable, Overall)
rownames(overall.table)<- county
table <- overall.table[order(-overall.table$Overall), ]
table

shinyServer(function(input, output, session){
  output$mytable <- renderDataTable({
    
    DT::datatable(table)
  })
  
  data <- reactive({
    cn <- (input$counties)
    mine <- data.frame(electricity.coverage,earth,gravel,bitumen,land.rates,hypermarket.rates,megasupermarket.rates,smalltradershop.rates,mediumtradershop.rates,largetradershop.rates,kiosk.rates,expenditure,population, banks, smallindustrialplant.rates,mediumindustrialplant.rates,largeindustrialplant.rates )
    rownames(mine) <- county
    mine[input$counties, ]
    
  }) 
  blah <- reactive ({
    electricity.score <- (pnorm(data()$electricity.coverage, mean(data()$electricity.coverage), sd(data()$electricity.coverage), lower.tail=TRUE, log.p=FALSE)*100)
    electricity.score
    road <- data()$bitumen/(data()$bitumen+data()$gravel+data()$earth)
    road
    road.score <- (pnorm(road, mean(road), sd(road), lower.tail=TRUE, log.p= FALSE)*100)
    road.score
    Infrastructure<-  round((road.score+electricity.score)/2, digits=2)
    Infrastructure 
    Land <- round((pnorm(data()$land.rates, mean(data()$land.rates), sd(data()$land.rates), lower.tail=FALSE, log.p=FALSE)*100), digits=2)
    Land
    hypermarket.score <- (pnorm(data()$hypermarket.rates, mean(data()$hypermarket.rates), sd(data()$hypermarket.rates), lower.tail=FALSE, log.p=FALSE)*100)
    hypermarket.score
    megasupermarket.score <- (pnorm(data()$megasupermarket.rates, mean(data()$megasupermarket.rates), sd(data()$megasupermarket.rates), lower.tail=FALSE, log.p=FALSE)*100)
    megasupermarket.score
    largetradershop.score <- (pnorm(data()$largetradershop.rates, mean(data()$largetradershop.rates), sd(data()$largetradershop.rates), lower.tail=FALSE, log.p=FALSE)*100)
    largetradershop.score
    mediumtradershop.score <- (pnorm(data()$mediumtradershop.rates, mean(data()$mediumtradershop.rates), sd(data()$mediumtradershop.rates), lower.tail=FALSE, log.p=FALSE)*100)
    mediumtradershop.score
    smalltradershop.score <- (pnorm(data()$smalltradershop.rates, mean(data()$smalltradershop.rates), sd(data()$smalltradershop.rates), lower.tail=FALSE, log.p=FALSE)*100)
    smalltradershop.score
    kiosk.score <- (pnorm(data()$kiosk.rates, mean(data()$kiosk.rates), sd(data()$kiosk.rates), lower.tail=FALSE, log.p=FALSE)*100)
    kiosk.score
    largeindustrialplant.score <- (pnorm(data()$largeindustrialplant.rates, mean(data()$largeindustrialplant.rates), sd(data()$largeindustrialplant.rates), lower.tail=FALSE, log.p=FALSE)*100)
    largeindustrialplant.score
    mediumindustrialplant.score <- (pnorm(data()$mediumindustrialplant.rates, mean(data()$mediumindustrialplant.rates), sd(data()$mediumindustrialplant.rates), lower.tail=FALSE, log.p=FALSE)*100)
    mediumindustrialplant.score
    smallindustrialplant.score <- (pnorm(data()$smallindustrialplant.rates, mean(data()$smallindustrialplant.rates), sd(data()$smallindustrialplant.rates), lower.tail=FALSE, log.p=FALSE)*100)
    smallindustrialplant.score
    taxation.table<- data.frame(hypermarket.score,megasupermarket.score,largetradershop.score, mediumtradershop.score,smalltradershop.score, kiosk.score,largeindustrialplant.score, mediumindustrialplant.score, smallindustrialplant.score)
    taxation.table
    Taxation <- round(apply(taxation.table, 1, mean), digits=2)
    Taxation
    finance.services <- as.numeric(data()$banks)/(as.numeric(data()$population)*as.numeric(data()$expenditure))
    finance.services
    Finance <- round((pnorm(finance.services, mean(finance.services), sd(finance.services), lower.tail=TRUE, log.p=FALSE)*100), digits=2)
    Finance
    mytable <- data.frame(Infrastructure,Land,Taxation, Finance)
    mytable
    overall <- round(apply(mytable, 1, mean), digits=2)
    overall.table <- cbind(mytable, overall)
    cn <- input$counties
    rownames(overall.table)<- cn
    overall.table[input$counties, ]
    table2 <- overall.table[order(-overall.table$overall), ]
    table2
    
  })
  
  df <- reactive({
    validate(
      need(input$vars != "", "Please select two or more categories"),
      need(input$counties != "", label="two or more counties")
      
    )
    Overall <- apply(blah()[,input$vars],1,mean)
    cbind(Overall, blah()[, input$vars])[order(-Overall), ]

  })
  
  
  output$mytable3 <- renderTable({
    print(blah())
    str(df())
    return(df())
    
    
  })
  hehe <- reactive({
    cbind(df(), County=rownames(df()))
  })
  output$myplot4 <- renderPlot({
    print(hehe())
    ggplot(hehe(), aes(County ,Overall, fill=County)) + geom_bar(stat="identity",position="dodge")+ geom_text(aes(y=Overall, ymax=Overall, label=Overall))+ labs(title="Comparing counties based on overall score",y="Overall Scores",x="Counties") + coord_flip()
    
  })
  
  output$myplot6 <- renderPlot({
    par(las=2)
    par(mar=c(5,8,4,2))
    County <- rownames(df())
    barplot(df()$Overall , main="Compare Counties",xlab="Scores", names.arg=County, xlim=c(0,100),horiz=TRUE, col="green")

    
  })
  haa <- reactive({
    cbind(df()[,input$vars], County=rownames(df()))
  })
  
  
  output$myplot7 <- renderPlot({
    print(haa())
    library(reshape2)
    haa.melt <- melt(haa(), id.vars="County")
    haa.melt
    
    ggplot(haa.melt, aes(variable, value)) +   
      geom_bar(aes(fill = County), position = "dodge", stat="identity")+labs(title="Comparing categories in different counties",y="Scores",x="Categories")
  })
  
  output$myplot8 <- renderPlot({
    library(reshape2)
    haa.melt <- melt(haa(), id.vars="County")
    haa.melt
    ggplot(haa.melt,aes(x=County, y = value)) + 
      facet_wrap(~variable) +
      geom_bar(aes(fill =County), position="dodge" ,stat="identity")+labs(title="Comparing categories in different counties",y="Scores",x="Counties")+coord_flip()
  })
  
  hee <- reactive({
    cbind(df(), County=rownames(df()))
  })
  
  output$myplot9 <- renderPlot({
    library(reshape2)
    hee.melt <- melt(hee(), id.vars="County")
    hee.melt
    ggplot(hee.melt,aes(x=County, y = value)) + 
      facet_wrap(~variable) +
      geom_bar(aes(fill =County), position="dodge" ,stat="identity")+labs(title="Comparing categories in different counties",y="Scores",x="Counties")+coord_flip()
  })
  output$myplot10 <- renderPlot({
    library(reshape2)
    hee.melt <- melt(hee(), id.vars="County")
    hee.melt
    ggplot(hee.melt,aes(x=variable, y = value)) + 
      facet_wrap(~County) +
      geom_bar(aes(fill =variable), position="dodge" ,stat="identity")+labs(title="Counties",y="Scores",x="Counties")+coord_flip()
    
})
})




