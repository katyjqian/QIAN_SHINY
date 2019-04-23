## ui.R ##

# inputs:
# catg_or_manuf: ("Category", "Manufacturer")
# pfocus: ("count", "price", "value", "popularity")
# gfocus: (""all", "girl", "boy", "neutral")
# ggroup: chr vector of user selected girl keywords
# bgroup: chr vector of user selected boy keywords


shinyUI(dashboardPage(skin = 'green',
  dashboardHeader(title = "Katys Shiny Project"),
  dashboardSidebar(
    sidebarUserPanel("Pink Tax in Toys"),
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("adn")),
      menuItem("Methodology", tabName = "method", icon = icon("code-branch")),
      menuItem("Set Keywords", tabName = "keywords", icon = icon("key")),
      menuItem("Boy Vs. Girl", tabName = "boyvsgirl", icon = icon("yin-yang")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Top20 Analysis", tabName = "top20", icon = icon("sort-amount-up")))
  ),
  
    dashboardBody(
      tabItems(
        
        # This tab just shows dataframe with nice gvis interface
        tabItem(tabName = "data", fluidRow(box(DT::dataTableOutput("table"), width = 12))),
        
        # This tab allows user to choose their own keywords for gender category evaluation
        tabItem(tabName = "keywords", 
                fluidRow(
                  box(checkboxGroupInput("ggroup", 
                     label = h4("Girl Keywords"), 
                     choices = list("princess", "glitter", " pony", " doll", " pink", "flower", "barbie", "unicorn")
                     ), hr(), verbatimTextOutput("gwords")),
                  box(checkboxGroupInput("bgroup", 
                     label = h4("Boy Keywords"), 
                     choices = list("super hero", "action figure", "dinosaur", " truck", " blue", "star wars", "zombie", " soldier")
                     ), hr(), verbatimTextOutput("bwords"))     ), 
                fluidRow(column(12, align="center", h5("Note: core keywords will always be included (girl, daughter, boy, son)"),
                                actionButton("keyupdate", label = "Update Dataset with New Gender Keywords", style="color: #fff; background-color: #337ab7;"))),
                fluidRow(hr(),column(12, align="center", verbatimTextOutput("bgcount")))
                ),
        
        # This tab will show a BOXPLOT comparing price & gender category
        tabItem(tabName = "boyvsgirl", 
                fluidRow(infoBoxOutput("boyBox"), infoBoxOutput("girlBox"),infoBoxOutput("compareBox")),
                fluidRow(plotOutput("bgbox")), hr(),hr(),hr(),hr(), h4("Categories with sufficient datapoints for girl boy comparison (out of 148):"),
                fluidRow(box(DT::dataTableOutput("cattable"), width = 12))
                #fluidRow(htmlOutput("catbubble"))
                
        ),
        
        # This tab showcases top20 category/manufacturer over 4 different metrics with BARCHARTS
        tabItem(tabName = "top20", 
                fluidRow(column(4, radioButtons("catg_or_manuf", "Category or Manufacturer", c("Category", "Manufacturer"))),
                         column(4, selectizeInput("pfocus", "Select Metric", c("count", "avg_price", "value", "popularity"))),
                         column(4, selectizeInput("gfocus", "Select Gender Focus", c("all", "girl", "boy", "neutral")))),
                plotOutput("top20graph")),

        # This tab is info only - explaining motivations 
        tabItem(tabName = "about",
                print(h1("The Pink Tax")),

                print(h4("The pink tax is form of gender-based price discrimination where products
                         catered towards females tend to cost more. The the name stems from
                         the observation that many of the affected products are pink. For children specifically,
                         it is estimated that it costs $17k more to raise a girl than to raise a boy in 18 years.", tags$br(), tags$br(), tags$br(),
                         "This project analyzes Amazon products, provides a gender rating based on keywords, and
                         determines if evidence of this phenomenon can be found in childrens goods.",
                         tags$br(), tags$br())),

                print(h3("Example Images:")),
                print(img(src = "goodnightmoon.jpg", height = "400")),
                print(img(src = "helmet.jpg", height = "400")),
                print(img(src = "rocker.jpg", height = "400")),
                print(img(src = "walker.jpg", height = "400")),
                print(img(src = "kinder.png", height = "400"))
        ),
        
      # This tab is info only - explaining methodology 
      tabItem(tabName = "method",
              print(img(src = "database.png", width = "1200")),
              print(h3("Future Considerations:", tags$br())),
              print(h5("- Refine the methodology used to determine gender category (values vs. TF, beyond keywords?)", tags$br(),
                       "- Review other datasets/categories (clothes, sports, movies, etc)",tags$br(),
                       "- Research how gender/price affects actual items sold", tags$br())),
              print(h3("Datasets & Sources used for this project:", tags$br())),
              print(h5("- https://www.kaggle.com/PromptCloudHQ/toy-products-on-amazon", tags$br(),tags$br(),
                       "- http://jmcauley.ucsd.edu/data/amazon/links.html",tags$br(),
                       "- https://www.moneytips.com/the-costs-of-raising-boys-vs-girls-infographic", tags$br())))
              
      )
      
    )
))
