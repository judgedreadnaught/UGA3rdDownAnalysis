library(shiny)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(png)

#setwd("~Desktop/Math research work")
lsu_game <- read_csv("LSU_game.csv")
#lsu_game1 <- lsu_game[4:nrow(lsu_game),1:ncol(lsu_game)]
l_game <- lsu_game[4:nrow(lsu_game),2:3]
names(l_game)[names(l_game) == "RED ZONE EXCLUDED"] <- 'SUCCESS_RATE'
names(l_game)[names(l_game) == "...3"] <- 'FREQUENCY'
l_game <- l_game %>% mutate(SUCCESS_RATE = as.numeric(gsub("%","",SUCCESS_RATE)))
l_game$"FREQUENCY" = as.numeric(as.character(l_game$"FREQUENCY"))
l_game <- l_game %>% mutate("TRUE_RATE" = (SUCCESS_RATE/100) * FREQUENCY)
l_game$"PLAY_NAMES" <- lsu_game[4:nrow(lsu_game),1]
l_game$"PLAY_NAMES" <- unlist(l_game$"PLAY_NAMES")
l_game

# GAME DATA (CLEANED CSV)
LSU <- l_game
GT <- read_csv("gt_game.csv")[,-1]
AUBURN <- read_csv("auburn_game.csv")[,-1]
MIZZOU <- read_csv("miz_game.csv")[,-1]
KENTUCKY <- read_csv("u_game.csv")[,-1]
FLORIDA <- read_csv("f_game.csv")[,-1]
SC <- read_csv("s_game.csv")[,-1]
A_M <- read_csv("a_m_game.csv")
CONCEPTS <- read_csv("concepts.csv")[,-1]
GAME_TOTALS <-read_csv("game_totals.csv")[,-1]
BLL_PLAYS <- read_csv("all_plays1.csv")

ALL_PLAYS = subset(BLL_PLAYS,BLL_PLAYS$TRUE_RATE > mean(TRUE_RATE,na.rm = TRUE))
ALL_PLAYS_1 = subset(BLL_PLAYS,BLL_PLAYS$TRUE_RATE > 40)


# RAW GAME DATA 
LSU_D <- lsu_game
GT_D <- read_csv("gt_raw.csv")
AUBURN_D <- read_csv("auburn_raw.csv")
MIZZOU_D <- read_csv("mizzou_raw.csv")
KENTUCKY_D <- read_csv("UK_raw.csv")
FLORIDA_D <- read_csv("florida_raw.csv")
SC_D <- read_csv("SC_raw.csv")
A_M_D <- read_csv("a_m_raw.csv")
ALL_PLAYS_D <- read_csv("all_plays.csv")

# PLAYER BODY COMPOSITION DATA 
OL <- read_csv("ol_data.csv")[,-7]
OL <- OL[,-1]
names(OL)[names(OL) == "Lean vs. Total Mass Ratio"] <- 'Lean_v.s_Total_Mass_Ratio'
names(OL)[names(OL) == "Fat vs. Total Mass Ratio (% Fat)"] <- 'Fat_v.s_Total_Mass_Ratio'

TE <- read_csv("te_data.csv")[,-1]
RB <- read_csv("rb_data.csv")[,-1]
WR <- read_csv("wr_data.csv")[,-1]
QB <- read_csv("qb_data.csv")[,-1]
DL <- read_csv("dl_data.csv")[,-1]
OLB <- read_csv("olb_data.csv")[,-1]
ILB <- read_csv("ilb_data.csv")[,-1]
DB <- read_csv("db_data.csv")[,-1]
SP <- read_csv("sp_data.csv")[,-1]

OL_Lean <- read_csv("OL_Lean.csv")
OL_Fat <- read_csv("OL_Fat.csv")

TE_Lean <- read_csv("TE_Lean.csv")
TE_Fat <- read_csv("TE_Fat.csv")

RB_Lean <- read_csv("RB_Lean.csv")
RB_Fat <- read_csv("RB_Fat.csv")

WR_Lean <- read_csv("WR_Lean.csv")
WR_Fat <- read_csv("WR_Fat.csv")

QB_Lean <- read_csv("QB_Lean.csv")
QB_Fat <- read_csv("QB_Fat.csv")

DL_Lean <- read_csv("DL_Lean.csv")
DL_Fat <- read_csv("DL_Fat.csv")

OLB_Lean <- read_csv("OLB_Lean.csv")
OLB_Fat <- read_csv("OLB_Fat.csv")

ILB_Lean <- read_csv("ILB_Lean.csv")
ILB_Fat <- read_csv("ILB_Fat.csv")

DB_Lean <- read_csv("DB_Lean.csv")
DB_Fat <- read_csv("DB_Fat.csv")



team_list <- list("LSU","GT","AUBURN","MIZZOU","KENTUCKY","FLORIDA", "SC","A_M","ALL_PLAYS")
team_list2 <- list("LSU_D","GT_D","AUBURN_D","MIZZOU_D","KENTUCKY_D","ALL_PLAYS_D")
pos_list <- list("OL","TE","RB","WR","QB","DL","OLB",
                 "ILB","DB")

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title="Analysis of 3rd Down Plays in UGA Games (2019)",titleWidth= 650),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About Page",tabName="about",icon=icon("address-card")),
      menuItem("Game Dataset",tabName="data_tab",icon=icon("dashboard")),
      selectInput(inputId="var1",label="Select the Team",choices=team_list),
      menuItem("Game Graphs",tabName="graph", icon=icon("chart-line")),
      selectInput(inputId="var2",label="Select the Team",choices=team_list),
      menuItem("Body Comp Graphs",tabName="body_data2",icon=icon("chart-line")),
      selectInput(inputId="var3",label="Select the Position",choices=pos_list),
      menuItem("Body Comp Data",tabName="body_data3",icon=icon("dashboard")),
      selectInput(inputId = "var4",label="Select the Position",choices=pos_list),
      menuItem("All Plays Analysis", tabName="all_plays_1",icon=icon("dashboard"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName="about",
              fluidRow(
                column(width=10,tags$br(),
                  div(tags$img(src="ugafootballphoto1.png",height=1,width=1,alt="UGA",
                           deleteFile=FALSE), imageOutput("my_image"))),
                column(width=10,height=100,tags$br()),
                column(width=10,height=100,tags$br()),
                column(width=10,height=100,tags$br()),
                column(width=10,height=100,tags$br()),
                column(width = 10,height=10, tags$br(),tags$br(),tags$br(), 
                       tags$p("This website was created as an industry project for Math 4950 by Aakash Khanal, Chris Barroso and Tanna Rankin. The idea of building a website as the driving focus in our project budded when we realized that no one in our group had extensive football knowledge. When presented with all the football data, rigorous analysis was not a possibility, so we switched onto a utility-based approach that would allow someone that had knowledge on football to gain new insight when using our website’s tools.
                               This website contains six different pages. The first page we are on is the About Page. The Game Dataset page shows the data for the plays we used.The Game Graphs displays the data in four different types of graphs. The Body Comp Graphs page shows graphs of the Fat/Lean ratio of the players in all positions. The Body Comp Data shows the data we used to create the graphs for the previous page. The All Plays Analysis displays the top 18 plays,top 5 plays, and a cumulative list of all the plays from every game from the original dataset. 
                              "))
              )
      ), # tabItem
      
      # THIS IS THE TAB THAT DISPLAYS THE DATASETS AND SUMMARY STATISTICS
      tabItem(tabName = "data_tab", 
              tabBox(id="t1",width=12,
                     tabPanel("Raw_Dataset",icon=icon("address-card"),dataTableOutput("raw")),
                     tabPanel("Cleaned_Dataset",icon=icon("address-card"),dataTableOutput("structure")),
                     tabPanel("Summary Statistics", icon = icon("address-card"),verbatimTextOutput("summary"))
              ) #tabBox
      ), #tabItem
      
      # THIS TAB DISPLAYS THE GRAPHS FOR THE 3RD DOWN DATA
      tabItem(tabName = "graph",
              # First box that allows user to chose x-value
              box(selectInput(inputId="x_value","Select X value",choices=list("FREQUENCY","SUCCESS_RATE","TRUE_RATE","PLAY_NAMES"),selected="FREQUENCY",width=250)),
              # Second box that allows user to choose y-value
              box(selectInput(inputId="y_value","Select Y value",choices=list("FREQUENCY","SUCCESS_RATE","TRUE_RATE","PLAY_NAMES"),selected="PLAY_NAMES",width=250)),
              # Creating the different tabs for each respective graph
              tabBox(id='t2', width = 12,
                     tabPanel(title="Histogram", value="trends",plotOutput(outputId = "histPlot")),
                     tabPanel(title="Bar Graph", value="distro",plotOutput(outputId='bar')),
                     tabPanel(title="Scatterplot", value="lal",plotOutput(outputId="scatterplot")),
                     tabPanel(title="Boxplot", value = "lal2",plotOutput(outputId="lineplot"))
              ) # tabBox
      ), #tabItem2
      
      tabItem(tabName = "body_data3", 
              tabBox(id='t3',width = 12,
                     tabPanel(title = "Body Comp Dataset",icon=icon("address-card"),dataTableOutput("comp_data")),
              )
      ), # tabItem
      
      # THIS TAB GENERATES THE BODY COMPOSITION GRAPHS  
      tabItem(tabName = "body_data2",
          tabBox(id="t4",width=12,
                 tabPanel(title="Total Mass vs. Lean/Total Mass Ratio",value="trends2",plotOutput(outputId="lean")),
                 tabPanel(title="Total Mass vs. Fat/Total Mass Ratio",value="trends2",plotOutput(outputId="fat")),
                 )),
      
      tabItem(tabName="all_plays_1",
              box(selectInput(inputId="x1_value","Select X value",choices=list("FREQUENCY","SUCCESS_RATE","TRUE_RATE","PLAY_NAMES"),selected="TRUE_RATE",width=250)),
              box(selectInput(inputId="y1_value","Select Y value",choices=list("FREQUENCY","SUCCESS_RATE","TRUE_RATE","PLAY_NAMES"),selected="PLAY_NAMES",width=250)),
              
              tabBox(id="t5",width=12,
                     tabPanel(title="Top 18 Plays", value="dafas",plotOutput(outputId="all_graph")),
                     tabPanel("All Plays Analysis",icon=icon("address-card"),dataTableOutput("topplays")),
                     tabPanel(title="Top 5 Plays", value="sadf",plotOutput(outputId='topplays_1'))
                     
              )
      ) # tabItem
      
    ) #tabItems
  ) #dashboardBody
)# dashboardPage


server <- function(input,output) {
  
  # Raw_Dataset Function that returns the inputted team's raw dataset
    output$raw <- renderDataTable(
      get(gsub(" ","",paste(input$var1,"_D")))
    )
  
  # Cleaned_Dataset Function that returns the inpuuted team's cleaned dataset
    output$structure <- renderDataTable(
      get(input$var1) 
    )
    
  # Summary Function returns a summary statistic of the team
    output$summary <- renderPrint(
      get(input$var1) %>%
        summary()
    )
    
    # Histogram function that outputs the histogram with the x-values always being Success rate
    output$histPlot <- renderPlot( {                                                   
      hist(get(input$var2)$SUCCESS_RATE, main = paste("Distribution of Success Rates",input$var2) , xlab = "Success of 3rd Down Concepts") 
                                                      # input$var2 is the input selected by the user using the second dropdown menu, ex.LSU,UK,AUBURN,GT
    })
    
    # Scatter Plot
    output$scatterplot <- renderPlot( {
      ggplot(get(input$var2),aes(x=get(input$x_value),y=get(input$y_value),color=PLAY_NAMES)) + geom_point() +
        xlab(input$x_value) + ylab(input$y_value) + ggtitle(paste("Team: ",input$var2))
      # This line above reads the x and y values the user wants to see and 
      # allows the graph titles to be dynamic and change based on what team the user chooses
      
    })
    
    # Second Scatter plot 
    
    # TEMP BOXPLOT 
    output$lineplot <- renderPlot({
      ggplot(get(input$var2),aes(x=get(input$x_value),y=get(input$y_value))) + geom_boxplot() + xlab(input$x_value) + ylab(input$y_value) + ggtitle(paste("Team: ",input$var2))
    })
    
    
    
    # Bar Graph
    output$bar <- renderPlot({
      ggplot(get(input$var2), aes(x=get(input$x_value),y=get(input$y_value),color= PLAY_NAMES)) + theme(axis.title = element_text(size = 10, face = "bold"),
                                                                              axis.text.x = element_text(size = 10,face = "bold"),
                                                                              plot.title = element_text(size = 10, hjust = 0.5),
                                                                              plot.subtitle = element_text(size = 10, hjust= 0.5)) +
        geom_col() + xlab(input$x_value) + ylab(input$y_value) + ggtitle(paste("Team: ",input$var2))
      
    })
    
    # _____________BODY COMP CODE____________________________________CODE____________________________________CODE____________________________________
    
    # Getting .csv of the body composition data 
    output$comp_data <- renderDataTable(
      get(input$var4) 
    )
    
    # Bar Grap DEX
    output$scatterplot3 <- renderPlot({
      ggplot(OL, aes(x=Lean_v.s_Total_Mass_Ratio,y=Fat_v.s_Total_Mass_Ratio,color="RED")) + geom_point()
    })
    
    output$my_image <- renderImage({
      list(src="ugafootballphoto1.png",height="500px",width="700px",alt="Something went wrong")
    })

    output$lean <- renderPlot({
      ggplot(get(gsub(" ","",paste(input$var3,"_Lean"))),aes(x=`Total Mass (lbs)`,y=`Lean/Total Mass Ratio`)) + geom_point() + geom_smooth(method="lm",se=TRUE) +
        ggtitle(paste("Position: ",input$var3))
      
    })
    
    output$fat <- renderPlot({
      ggplot(get(gsub(" ","",paste(input$var3,"_Fat"))),aes(x=`Total Mass (lbs)`,y=`Fat/Total Mass Ratio`)) + geom_point() + geom_smooth(method="lm",se=TRUE) +
        ggtitle(paste("Position: ",input$var3))
      
    })
    # ALL_PLAYS_ANALYSIS 
    
    output$all_graph <- renderPlot({
      ggplot(ALL_PLAYS, aes(x=get(input$x1_value),y=get(input$y1_value),color= PLAY_NAMES)) + theme(axis.title = element_text(size = 10, face = "bold"),
                                                                                                        axis.text.x = element_text(size = 10,face = "bold"),
                                                                                                        plot.title = element_text(size = 10, hjust = 0.5),
                                                                                                        plot.subtitle = element_text(size = 10, hjust= 0.5)) +
        geom_col() + xlab(input$x1_value) + ylab(input$y1_value) + ggtitle("ALL PLAYS GRAPH")    })
    
    output$topplays <- renderDataTable(
      get("BLL_PLAYS")
    )
    
    output$topplays_1 <- renderPlot({
      ggplot(ALL_PLAYS_1, aes(x=get(input$x1_value),y=get(input$y1_value),fill= PLAY_NAMES)) + theme(axis.title = element_text(size = 10, face = "bold"),
                                                                                               axis.text.x = element_text(size = 10,face = "bold"),
                                                                                               plot.title = element_text(size = 10, hjust = 0.5),
                                                                                               plot.subtitle = element_text(size = 10, hjust= 0.5)) +
        geom_col() + xlab(input$x1_value) + ylab(input$y1_value) + ggtitle("TOP 5 PLAYS")    })
    
    #get(gsub(" ","",paste(input$var1,"_D")))
    #list(src=gsub(" ", "",paste(input$var3,"_1.png")))
    
    
    

    
}

shinyApp(ui=ui,server=server,options = list(height = 1080))