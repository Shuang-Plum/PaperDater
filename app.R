# Load packages ----------------------------------------------------------------
library(DT)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(ggalluvial)
library(RISmed)



# Load data and source code -----------------------------------------------------

source('Codes/color_pick.R')
source('Codes/integer_breaks.R')
source('Codes/print_query.R')

source('Codes/keyword_search.R')
source('Codes/keyword_data.R')
#source('Codes/keyword_data_dl.R')
source('Codes/keyword_filter_plot.R')

source('Codes/journal_search.R')
source('Codes/journal_data.R')
source('Codes/journal_filter_plot.R')

source('Codes/matchMeSH.R')
source('Codes/MeSH_plot.R')


# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  
  #shinythemes::themeSelector(), # set theme selector
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
        font-weight: bold;
        font-size: 18px;
      }
    "))
  ),
  
  titlePanel(fluidRow(column(1, img(height = 30, width = 90, src = "logo3.jpg")),
                      column(10, align="left", tags$p(HTML(paste0(strong("PaperDater"), " - a digital matchmaker for manuscripts and journals."))))),
             windowTitle = "PaperDater"), # set title and tab title
  fluidRow(column(12,
                  h4('This webapp can be used to extract PubMed search terms and match them with published journal articles over time.')
  )),
  
  tabsetPanel(
    
    tabPanel(title = HTML(paste(p(strong("MeSH Term Match"),style = "font-size:18px"))),
             sidebarLayout(
               sidebarPanel(width = 3,
                            
                            tags$p("MeSH Term Match extracts ", HTML('&nbsp;'),
                                   tags$a("MeSH terms",href="https://www.nlm.nih.gov/mesh/meshhome.html",target="_blank"), 
                                   HTML('&nbsp;')," from input text and generates a tile plot."),
                            hr(),
                            
                            textAreaInput(
                              inputId = "input.text",
                              label = "Input Text :",
                              placeholder = 'Any text or abstract',
                              height = '300px'
                            ),
                            
                            
                            actionButton('mesh.match','Match',width='100%',class = "btn-success"),
                            
                            hr(),
                            tags$p('Click here to download detailed supporting information'),
                            downloadButton("mdownload_supp", "SupportInfo")
                            
                            
               ),
               
               mainPanel(width=9,
                         
                         wellPanel(id = "m.plot",style = "overflow-y:scroll; height: 500px",
                                   #textOutput(outputId = 'trial'),
                                   plotOutput(outputId = "mesh.plot")
                         ),
                         
                         
                         hr(),
                         br(),
                         
                         downloadButton("download_mesh_plot", "Download MeSH Term Match plot"),
                         
                         hr(),
                         br(),
                         downloadButton("download_mesh_data", "Download full list of matched MeSH Terms")
                         
                         
               )
               
             )
             
    ),
    
    tabPanel(title = HTML(paste(p(strong("Keyword Search"),style = "font-size:18px"))),
             sidebarLayout(
               sidebarPanel(width = 2,
                            
                            tags$p('Keyword Search extracts all publications matched to the keywords and generates an alluvial plot by journal over time.'),
                            
                            textAreaInput(
                              inputId = "keyword",
                              label = "Keywords :",
                              placeholder = 'multiomics or multiomics[MeSH Terms]',
                              height = '100px'
                            ),
                            
                            
                            textInput(
                              inputId = "k.starty",
                              label = "Year Start :",
                              placeholder = '2020'
                            ),
                            
                            textInput(
                              inputId = "k.endy",
                              label = "Year End :",
                              placeholder = '2023'
                            ),
                          
                            actionButton('k.search','Plot',width='100%',class = "btn-success"),
                            
                            hr(),
                            
                            tags$p('Show journals above a minimum total number of publications across the year range'),
                            
                            textInput(
                              inputId = "k.min.count",
                              label = "Min total count :",
                              placeholder = '3',
                              value='3'
                            ),
                         
                            tags$p('Show labels above a minimum count for a given year'),
                            
                            textInput(
                              inputId = "k.min.count.forlabel",
                              label = "Min count for label :",
                              placeholder = '3',
                              value = '3'
                            ),
                            
                            hr(),
                            tags$p("Keywords can include ", HTML('&nbsp;'),
                                   tags$a("search field tags",href="https://pubmed.ncbi.nlm.nih.gov/help/#using-search-field-tags",target="_blank"),
                                   HTML('&nbsp;')," as Advanced search in PubMed."),
                            br(),
                            tags$p('Click here to download detailed supporting information'),
                            downloadButton("kdownload_supp", "SupportInfo")
                            
                            
                            
                            #actionButton('k.replot','Re-Plot',width='100%',class = "btn-success")
                            
               ),
               
               mainPanel(width=10,
                         hr(),
                         
                         wellPanel(id="k.query",style = "overflow-y:scroll; height:300px; font-size: 20px; padding: 8px 8px",
                                   shinycssloaders::withSpinner(
                                     htmlOutput(outputId = 'keyword.query'),
                                     type = 1, color = "#408000", size = 1.0,proxy.height=250)
                                   ),

                         
                         hr(),
                         #hr(),
                         br(),
                         wellPanel(id = "k.plot",style = "overflow-y:scroll; height: 650px",
                                   #textOutput(outputId = 'trial'),
                                   plotOutput(outputId = "keyword.plot")
                         ),
                         hr(),
                         br(),
                         
                         downloadButton("download_key_plot", "Download Keyword Search plot"),
                         
                         hr(),
                         br(),
                         downloadButton("download_key_data", "Download Keyword Search full results")
                         
                         
               )
               
             )
             
             
    ),
    
    
    
    tabPanel(title = HTML(paste(p(strong("Journal Profiler"),style = "font-size:18px"))),
             sidebarLayout(
               sidebarPanel(width = 2,
                            
                            tags$p("Journal Profiler extracts ", HTML('&nbsp;'),
                                   tags$a("MeSH terms",href="https://www.nlm.nih.gov/mesh/meshhome.html",target="_blank"), 
                                   HTML('&nbsp;')," associated with all publications for a single journal and generates an alluvial plot by MeSH term over time."),
                            #hr(),
                            
                            textAreaInput(
                              inputId = "j.name",
                              label = "Journal Name :",
                              placeholder = 'Journal of Biological Chemistry',
                              height = '100px'
                            ),
                            
                            tags$p('Please enter the full journal name.'),
                            
                            textInput(
                              inputId = "j.starty",
                              label = "Year Start :",
                              placeholder = '2020'
                            ),
                            
                            textInput(
                              inputId = "j.endy",
                              label = "Year End :",
                              placeholder = '2023'
                            ),
                            
                            
                            actionButton('j.search','Plot',width='100%',class = "btn-success"),
                            
                            hr(),
                            tags$p('Show labels above a minimum count for a given year'),
                            
                            textInput(
                              inputId = "j.min.count.forlabel",
                              label = "Min count for label :",
                              placeholder = '200',
                              value = '200'
                            ),
                            hr(),
                            tags$p('Click here to download detailed supporting information'),
                            downloadButton("jdownload_supp", "SupportInfo")
                            

               ),
               
               mainPanel(width=10,
                         
                         wellPanel(id="j.notification",style = "height:45px; font-size: 18px; padding: 8px 8px",
                                   tags$p(strong('Wait time is long for Journal Profiler, approx 5-15 min.'))),
                         hr(),
                         wellPanel(id="j.query",style = "overflow-y:scroll; height:200px; font-size: 20px; padding: 8px 8px",
                                   shinycssloaders::withSpinner(
                                     htmlOutput(outputId = 'journal.query'),
                                     type = 1, color = "#408000", size = 0.7,proxy.height=150)
                                   ),
                         
                         hr(),
                         br(),
                         wellPanel(id = "j.plot",style = "overflow-y:scroll; height: 650px",
                                   #textOutput(outputId = 'trial'),
                                   plotOutput(outputId = "journal.plot")
                         ),
                         hr(),
                         br(),
                         
                         downloadButton("download_journal_plot", "Download Journal Profiler plot"),
                         
                         hr(),
                         br(),
                         downloadButton("download_journal_data", "Download Journal Profiler full results")
                         
                         
               )
               
             )
             
             
    )
    
  )
  
)


# Define server ----------------------------------------------------------------
`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}
server <- function(input, output, session) {
  
  
  updateTextAreaInput(session, 'keyword')
  updateTextInput(session, 'k.starty')
  updateTextInput(session, 'k.endy')
  updateTextInput(session, 'k.min.count')
  updateTextInput(session, 'k.min.count.forlabel')
  
  output$keyword.query<-renderUI(NULL)
  
  keyword.ris<-eventReactive(input$k.search, {
    req(input$keyword)
    req(input$k.starty)
    req(input$k.endy)
    
    keyword_search(input$keyword, input$k.starty, input$k.endy) 
    
  })
  
  
  keyword.data<-eventReactive(input$k.search, {
    
    req(input$keyword)
    req(input$k.starty)
    req(input$k.endy)
    
    keyword_data(keyword.ris(), input$k.starty, input$k.endy)
 
  })
  
  # k.data.dl<-eventReactive(input$k.search, {
  #   
  #   keyword_data_dl(keyword.data()) 
  #   
  # })
  
  
  observeEvent(input$k.search, {
    
    output$keyword.query<-renderUI({
      
      validate(
        need(input$keyword, 'Please enter at least one Keyword'),
        need(input$k.starty, 'Please enter Year Start'),
        need(input$k.endy, 'Please enter Year End')
      )
      
      HTML(print_query(keyword.ris()))
      
    })
    
    
  })

  
  observeEvent(input$k.search, {
    
    output$keyword.plot <- renderPlot({
      
      validate(
        need(input$keyword, 'Please enter at least one Keyword') %then%
        need(input$k.starty, 'Please enter Year Start') %then%
        need(input$k.endy, 'Please enter Year End') %then%
        need(try(keyword.data()), 'Too Many Requests. HTTP Error 429. Please wait 20s and then click Plot again.')
      )

      
      keyword_filter_plot(keyword.data(),input$k.min.count,input$k.min.count.forlabel,input$keyword)
      

    },
    width=1200,
    height=600)
    
  })
  
  
  output$download_key_plot <- downloadHandler(
    
    filename = function() {paste0(input$keyword, '_keyword_plot.pdf')},
    
    content = function(file) {
      ggsave(keyword_filter_plot(keyword.data(),input$k.min.count,input$k.min.count.forlabel,input$keyword),
             device='pdf',filename = file, width = 5350, height = 2450, units='px')
    }
  )
  
  output$download_key_data <- downloadHandler(
    filename = function() { paste0(input$keyword, '_keyword_data.csv') },
    content = function(file) { 
      write.csv(keyword.data(), file, row.names = F) 
    }
  )
  
  output$kdownload_supp <- downloadHandler(
    filename = 'PubAnalysis_SupportingInfo.pptx',
    content = function(file) { 
      file.copy("www/methods.pptx", file) 
    }
  )
  
  ##### journal tab
  
  updateTextAreaInput(session, 'j.name')
  updateTextInput(session, 'j.starty')
  updateTextInput(session, 'j.endy')
  updateTextInput(session, 'j.min.count.forlabel')
  
  output$journal.query<-renderUI(NULL)
  
  journal.ris<-eventReactive(input$j.search, {
    req(input$j.name)
    req(input$j.starty)
    req(input$j.endy)
    
    journal_search(input$j.name, input$j.starty, input$j.endy) 
    
  })
  
  journal.data<-eventReactive(input$j.search, {
    req(input$j.name)
    req(input$j.starty)
    req(input$j.endy)
    
    journal_data(journal.ris(),input$j.starty, input$j.endy) 
    
  })
  
  
  observeEvent(input$j.search, {
    
    output$journal.query<-renderUI({
      
      validate(
        need(input$j.name, 'Please enter Journal Name'),
        need(input$j.starty, 'Please enter Year Start'),
        need(input$j.endy, 'Please enter Year End')
      )
      
      HTML(print_query(journal.ris()))
      
    })
    
  })
  
  

  observeEvent(input$j.search, {
    
    output$journal.plot <- renderPlot({
      
      validate(
        need(input$j.name, 'Please enter Journal Name') %then%
        need(input$j.starty, 'Please enter Year Start') %then%
        need(input$j.endy, 'Please enter Year End') %then%
        need(try(journal.data()), 'Too Many Requests. HTTP Error 429. Please wait 20s and then click Plot again.')
      )
      
      journal_filter_plot(journal.data(), input$j.min.count.forlabel, input$j.name)
      
    },
    width=1200,
    height=600)
    
  })
  
  
  output$download_journal_plot <- downloadHandler(
    
    filename = function() { paste0(input$j.name, '_journal_plot.pdf') },
    content = function(file) {
      ggsave(journal_filter_plot(journal.data(), input$j.min.count.forlabel, input$j.name),
             device='pdf', filename = file, width =5350, height =2450,units='px')
    }
  )
  
  output$download_journal_data <- downloadHandler(
    filename = function() { paste0(input$j.name, '_journal_data.csv') },
    content = function(file) { 
      write.csv(journal.data(), file, row.names = F) 
    }
  )
  
  output$jdownload_supp <- downloadHandler(
    filename = 'PubAnalysis_SupportingInfo.pptx',
    content = function(file) { 
      file.copy("www/methods.pptx", file) 
    }
  )
  
  ######### mesh tab
  
  mesh.data<-eventReactive(input$mesh.match, {
    
    matchMeSH(input$input.text)
    
  })
  
  y.max<-eventReactive(input$mesh.match, {
    
    ceiling(length(mesh.data())/3)
    
  })
  
  meshmatch.plot<-eventReactive(input$mesh.match, {
    
    MeSH_plot(mesh.data())
    
  })
  
  
  observeEvent(input$mesh.match, {
    
    output$mesh.plot <- renderPlot({
      
      print(meshmatch.plot())
      
    },
    width=880,
    height=(30+y.max()*55))
    
  })
  
  output$download_mesh_plot <- downloadHandler(
    
    filename = function() { 'Matched_MeSH_plot.pdf' },
    content = function(file) {
      ggsave(meshmatch.plot(), device='pdf', 
             filename = file, width =4200, height =(120+y.max()*230),units='px')
    }
  )
  
  output$download_mesh_data <- downloadHandler(
    filename = function() { 'Matched_MeSH_data.csv' },
    content = function(file) { 
      write.csv(mesh.data(), file, row.names = F) 
    }
  )
  
  output$mdownload_supp <- downloadHandler(
    filename = 'PubAnalysis_SupportingInfo.pptx',
    content = function(file) { 
      file.copy("www/methods.pptx", file) 
    }
  )
  
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
