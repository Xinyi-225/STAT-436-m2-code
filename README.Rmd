---
title: "Project m2_final code"
author: "Xinyi Wang"
date: "2023-03-25"
output: html_document
---

```{r setup, include=FALSE,message=F}
library(DT)
library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(rstatix)
library(ggpubr)
#install.packages("RColorBrewer")
library(RColorBrewer)
```

```{r,r,message=F}
data=read.csv("https://github.com/yicenyang/stat436/raw/main/heart.csv")
head(data)
```


#Logistic regression
```{r}
model = glm(target~thalach+trestbps+chol,data = data, family = binomial(link = "logit"))
summary(model)
```


```{r,message=F, include=FALSE}
##clean the dataset and some prepare works
data$target=as.factor(data$target)
data$target=as.factor(data$target)
data%>%drop_na()
data$index <- 1:nrow(data)
```


#Data visualization
```{r, fig.width = 18, fig.height = 16}
p <- list()
 p[["bar1"]] <- ggplot(data,aes(x=(trestbps)))+
  geom_bar(aes(fill=target))
 
 p[["scatter"]] <- ggplot(data,aes(x=age,y=thalach))+
  geom_point(aes(col=target,shape=as.factor(sex)))+
  theme(legend.position = "none") 
 
 p[["bar2"]] <-ggplot(data,aes(x=(chol)))+
  geom_bar(aes(fill=target))+
   theme(legend.position = "none") 



(p[["bar1"]]+p[["bar2"]])/ p[["scatter"]] +
    plot_layout(guides = "collect") &
   plot_annotation(theme = theme(legend.position = "bottom", title = element_text(size = 10)))
```

#Linked brushing

```{r}
data_table <- function(data, selected_) {
  data %>%
    filter(selected_) %>%
    select(age,sex,trestbps,chol,thalach,thal,target)
}
data_mean <- function(data, selected_) {
  data %>%
    filter(selected_) %>%
    summarise(mean=mean(as.numeric(target)))%>%
    pull()
}

counts <- list(
  "trestbps" = count(data, trestbps),
  "chol" = count(data, chol),
  "age" = count(data, age)
)


bar_plot <- function(sub_flights, v, width = 5) {
  ggplot(counts[[v]], aes(.data[[v]], n)) +
    geom_col(fill = "#d3d3d3", stat = "identity", width = width) +
    geom_col(data = sub_flights, stat = "identity", width = width)
}

plot_overlay <- function(selected_, v, width = 5) {
  data %>%
    filter(selected_) %>%
    count(.data[[v]]) %>%
    bar_plot(v, width)
}

scatterplot <- function(data, selected_) {
    data %>%
      mutate(selected_ = selected_) %>%
      ggplot() +
      geom_point(aes(chol,thalach , col=target, shape=as.factor(sex),alpha = as.numeric(selected_))) +
      scale_alpha(range = c(0.05, 0.6))+
    labs(shape="sex")
}
reset_selection <- function(x, brush) {
  xvar <- str_match(brush$mapping$x, "trestbps|chol|age")[1]
  brushedPoints(x, brush, allRows = TRUE, xvar = xvar)$selected_
}
```


**main code**

```{r, fig.width = 18, fig.height = 16}
ui <- fluidPage(
   fluidRow(
     column(6, 
            plotOutput("histogram_trestbps", brush = brushOpts("plot_brush", direction = "x"), height = 200),
            plotOutput("histogram_chol", brush = brushOpts("plot_brush", direction = "x"), height = 200),
            plotOutput("histogram_age", brush = brushOpts("plot_brush", direction = "x"), height = 200),
            plotOutput("scatterplot", brush = "plot_brush")
            
            ),
    column(6, dataTableOutput("table")),
    column(6,textOutput("info")),
    column(6,textOutput("average_heart_disease_rate"))
  ),
  
)
 


server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(data)))
  
  observeEvent(
    input$plot_brush,
    selected(reset_selection(data, input$plot_brush))
  )
  
  output$histogram_trestbps <-   renderPlot(plot_overlay(selected(),"trestbps",1))
  output$histogram_chol <- renderPlot(plot_overlay(selected(),"chol",5))
  output$histogram_age <- renderPlot(plot_overlay(selected(),"age",1))
  output$scatterplot <- renderPlot(scatterplot(data, selected()))
  output$table <- renderDataTable(data_table(data, selected()))
  
  output$average_heart_disease_rate <- renderText(data_mean(data, selected()))
  output$info = renderText({" average heart disease rate by selected data"})
}
 
 
app1 =  shinyApp(ui, server)
```



#grouping age into 4 groups

```{r}
theme_set(theme_gray())
data=read.csv("https://github.com/yicenyang/stat436/raw/main/heart.csv")
min(data$age)
max(data$age)
heart4 <- data %>% 
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>% 
  mutate(target = ifelse(target == 1, "Disease", "No Disease"),age_range = case_when( 20<age& age<=40 ~ "20-40",
                                 41<=age& age<=50 ~ "40-50",
                                 51<=age& age<=60 ~ "50-60",
                                 61<=age ~ "over 60",
                                 ))
heart4
```


#stat test and boxplots

```{r}
#generate function
stat.test <- heart4 %>%
  group_by(age_range) %>%
  t_test(chol ~ target,paired = FALSE) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
stat.test
```

```{r}
theme_set(theme_bw())
stat =function(x,y){
  fm <- as.formula(paste(x, y , sep = "~"))
  heart4%>%
  group_by(age_range) %>%
  t_test(fm,paired = FALSE) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")%>%
    add_xy_position(x = "target")%>%
    mutate(custom.label = ifelse(p <= 0.1, p, "not significant"))
}
chol = stat(x = "chol", y = "target")

```

```{r}
trestbps = stat(x = "trestbps", y = "target")
```


```{r}
thalach= stat(x = "thalach", y = "target")
```



```{r}
bp1 = function(x){
  ggboxplot(heart4, x = "target", y = x,fill = "target", palette = "jco",facet.by = "age_range")+theme_bw()
}
```

```{r}
p1 = bp1("chol")+ 
  stat_pvalue_manual(chol, label = "custom.label") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))


p2 = bp1("trestbps")+ 
  stat_pvalue_manual(trestbps, label = "custom.label") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))

p3 = bp1("thalach")+ 
  stat_pvalue_manual(thalach, label = "custom.label") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10)))

```

```{r,fig.width = 12}
p1+p2+p3+plot_layout(guide = "collect")
```


#shiny usmap

```{r}
# prepare data
data = read_csv("https://uwmadison.box.com/shared/static/8cxdb08bj3jfcicda06h0n0tg5l50kn7.csv")
data1 <- data %>%
  rename(Gender = Stratification1,
         Race = Stratification2)

us_states <- map_data("state")
state = read.csv("https://uwmadison.box.com/shared/static/59noryjk6oac0zlt42uhd1ktfmh9la9l.csv")
state$City = tolower(state$City)


#function of plot the map 

plot_choropleth <- function(filtered_data) {
  
  state_data <- filtered_data %>%
    group_by(LocationAbbr) %>%
    summarize(value = round(mean(Data_Value, na.rm = TRUE),0))

    merged_data <- inner_join(state_data , state, by = c("LocationAbbr" = "State"))
    merged_data2 <- inner_join(us_states,merged_data, by = c("region" = "City"))

    ggplot(data = merged_data2, aes(x = long, y = lat, group = group, fill = value)) +
    geom_polygon(color = "black", size = 0.2) +
    coord_map() +
    scale_fill_gradientn(colors = colorRampPalette(brewer.pal(9, "Reds"))(100), na.value = "gray90",
                     guide = guide_colorbar(title = "Value"))+
    labs(title = "State Average Death Rate per 100,000 by CVD") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "right")
}


# the interactive visulization of gragh 
ui <- fluidPage(
  titlePanel("Choropleth Map by Gender and Race"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", label = "Select Gender",
                  choices = c("Overall","Male", "Female")),
      selectInput("race", label = "Select Race",
                  choices = c("Overall",  "White", "Black","Hispanic",
                              "Asian and Pacific Islander","American Indian and Alaskan Native")) 
    ),
    
    mainPanel(
      plotOutput("choroplethMap")
    )
  )
)


server <- function(input, output) {
  output$choroplethMap <- renderPlot({
    # Filter data based on user selection
    filtered_data <- data1 %>%
      filter(Gender == input$gender,
             Race == input$race) 
    
    plot_choropleth(filtered_data)
})
}

app2 =  shinyApp(ui, server)
```



