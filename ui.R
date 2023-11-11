
#ui.R-----
ui <- bs4DashPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = preloader),
  fullscreen = TRUE,
  help = NULL,
  dark = TRUE,
  controlbar = bs4DashControlbar(
    h4("Setting Graph"), hr(),
      id = NULL,
      disable = FALSE,
      width = 400,
      collapsed = TRUE,
      overlay = TRUE,
      skin = "dark",
      pinned = NULL,
      sliderInput("opa",
                  "Opacity (0-1):",
                  min = 0,
                  max = 1,
                  value = 1,
                  step = 0.1
      ),
      sliderInput("width",
                  "Width  (0-1):",
                  min = 0,
                  max = 1,
                  value = 0.9,
                  step = 0.1
      ),
      plotlyOutput("graph")
      
    ),
  footer = dashboardFooter(left = "R-STAT; Version: 2.0", right = "Authors: Dražan Dizdar & Darko Katović",  fixed = FALSE),
  
 # Header
  header = dashboardHeader(
    use_theme(mytheme),
    status = status,
    title = bs4DashBrand(
      color = status,
      title = "R - STAT",
      image = logo
    )
  ),
 
  # Sidebar
  sidebar = bs4DashSidebar(
    disable = FALSE,
    width = "320px",
    skin = "dark",
    status = status,
    elevation = 4,
    collapsed = F,
    minified = F,
    bs4SidebarMenu(
      bs4SidebarMenuItem("Home", tabName = "home", icon = icon("home")),
      bs4SidebarMenuItem("Select Data Tables", tabName = "SDT", icon = icon("table-cells")),
      bs4SidebarMenuItem("Data Transformation", tabName = "DT", icon = icon("recycle")),
      bs4SidebarMenuItem("Grouping Categorical Data", icon = icon("users"),
               bs4SidebarMenuSubItem("Chi-Square Goodness of Fit Test", tabName = "FT"),
               bs4SidebarMenuSubItem("Chi-Square Test of Independence", tabName = "CT"),
               bs4SidebarMenuSubItem("McNemar’s Test", tabName = "MCN")
               ),
      bs4SidebarMenuItem("Grouping Continuous Data", tabName = "GCD", icon = icon("chart-area")),
      bs4SidebarMenuItem("Basic Statistics", icon = icon("chart-pie"),
               bs4SidebarMenuSubItem("Descirptive Parameters", tabName = "DP"),
               bs4SidebarMenuSubItem("Testing for Normality", tabName = "TN"),
               bs4SidebarMenuSubItem("CI for Population Mean", tabName = "PM"),
               bs4SidebarMenuSubItem("Correlations Analysis", tabName = "COR")
      ),
      bs4SidebarMenuItem("Univariate Methods", icon = icon("share-nodes"),
               bs4SidebarMenuSubItem("Independent Samples T-Test", tabName = "ISTT"),
               bs4SidebarMenuSubItem("Dependent Samples T-Test", tabName = "DSTT"),
               bs4SidebarMenuSubItem("One-way ANOVA", tabName = "OWA"),
               bs4SidebarMenuSubItem("Repeated Measures ANOVA", tabName = "RMA")
      ),
      bs4SidebarMenuItem("Multivariate Methods", icon = icon("sitemap"), 
               bs4SidebarMenuSubItem("Regression Analysis (simple)", tabName = "SRA"),
               bs4SidebarMenuSubItem("Regression Analysis (multiple)", tabName = "RA"),
               bs4SidebarMenuSubItem("Factor Analysis", tabName = "FA"),
               bs4SidebarMenuSubItem("Canonical Analysis", tabName = "CA"),
               bs4SidebarMenuSubItem("Discriminant Analysis", tabName = "DA"),
               bs4SidebarMenuSubItem("Cluster Analysis", tabName = "CLA")
      ),
      bs4SidebarMenuItem("Reliability Analysis", tabName = "REA",icon = icon("scale-balanced")),
      bs4SidebarMenuItem("Probability Calculator", icon = icon("calculator"),
               bs4SidebarMenuSubItem("Normal Distribution", tabName = "PCN"),
               bs4SidebarMenuSubItem("T - Distribution", tabName = "PCT"),
               bs4SidebarMenuSubItem("F - Distribution", tabName = "PCF"),
               bs4SidebarMenuSubItem("Chi Square - Distribution", tabName = "PCH")
      ),
      bs4SidebarMenuItem("Export File to Database", tabName = "data", icon = icon("folder-open")),
      bs4SidebarMenuItem("About", tabName = "about", icon = icon("circle-info")),
      bs4SidebarMenuItem("Help", icon = icon("question"), href = "https://github.com/dd-km/R-STAT/wiki")
    )
  ),
  # Body
  body = bs4DashBody(includeCSS(css),
              tabItems(
                  # Home			
                  tabItem(tabName = "home",
                          fluidRow(
                                   box(title = "Home",
                                       icon = icon("home"),
                                       width = 12,
                                       status = status,
                                       headerBorder = headerBorder,
                                       solidHeader = solidHeader,
                                       elevation = elevation,
                                       collapsible = F,
                                       tags$img(src=home),
                                       br(),
                                       h4("Version: 2.0"),
                                       hr(),
                                       h6("R-STAT is a web application for statistical-graphic data analysis.
                                       R-STAT was developed using the R free software environment for statistical computing and graphics 
                                       and the R packages Shiny and bs4Dash for creating interactive web applications. 
                                       It is hosted on shinyapps.io, a self-service web-hosting platform for Shiny applications managed by RStudio."),
                                       hr(),
                                       h6("Data for statistical-graphical analysis should be prepared so that:"),
                                       h6("- first column of the data table contains unique identifiers of the entity"),
                                       h6("- other columns contain the entity data in variables"),
                                       h6("- modality labels of qualitative variables must not be numbers."),
                                       hr(),
                                       h6("Data files can be created in:"), 
                                       h6("- Comma Separated Values (.csv) format,"), 
                                       h6("- Microsoft Excel Worksheet (.xlsx) format,"),
                                       h6("- LibreOffice Open Document Spreadsheet (.ods) format,"),
                                       h6("- SPSS Statistics Data Document (.sav) format."),
                                       hr(),
                                       h5(a("Create a Data File in Only Office", href="https://personal.onlyoffice.com/"))
                                    )
                          )
                  ),
                  
                  # View Data			
                  tabItem(tabName = "SDT",
                          fluidRow(
                            box(title = "Tables in Database",
                                width = 2,
                                status = status,
                                collapsible = F,
                                maximizable = T,
                                icon = icon("table-cells"),
                                awesomeRadio(
                                  inputId = "ST",
                                  label = "Select Tables in Database",
                                  choices = file, 
                                  inline = F, 
                                  status = status
                                ),
                                htmlOutput("brbs")
                            ),
                            box(title = "View Data",
                                width = 10,
                                status = status,
                                collapsible = F,
                                maximizable = T,
                                icon = icon("table-cells"),
                                dataTableOutput("data")
                            )
                        )
                  ),
                  # Data Transformation 
                  tabItem(tabName = "DT",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                icon = icon("list"),
                                awesomeCheckboxGroup(
                                  "NS", 
                                  "Normally Scaled:",
                                  choices = "", 
                                  selected ="",
                                  status = status
                                ),
                                awesomeCheckboxGroup(
                                  "OS", 
                                  "Inversely Scaled:",
                                  choices = "", 
                                  selected ="",
                                  status = status
                                ), 
                                a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Data-Transformation/", target="_blank")
                            ),
                            tabBox(title = "",
                                   width = 8,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   status = status,
                                   type = "tabs",
                                   maximizable = T,
                                   collapsible = F,
                                   tabPanel("Z-value",
                                   icon = icon("table"),
                                            dataTableOutput("zz")
                                   ),
                                   tabPanel("T-value",
                                            icon = icon("table"),
                                            dataTableOutput("tt")
                                   ),
                                   tabPanel("L-value",
                                            icon = icon("table"),
                                            dataTableOutput("ll")
                                   ),
                                   tabPanel("Z-profile",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("zpro")
                                   ),
                                   tabPanel("T-profile",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("tpro")
                                   ),
                                   tabPanel("L-profile",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("lpro")
                                   )
                            ),
                            box(title = "Entity",
                                width = 2,
                                icon = icon("user"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                awesomeRadio(
                                  inputId = "ENTITETI",
                                  label = "",
                                  choices = "", 
                                  selected ="",
                                  inline = F, 
                                  status = status
                                )
                            )
                        )
                  ),
                  # Chi-Square Goodness of Fit Test  
                  tabItem(tabName = "FT",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                
                                awesomeRadio(
                                  "FT", 
                                  label = "",
                                  choices = "", 
                                  selected ="",
                                  inline = F, 
                                  status = status
                                ),
                                awesomeRadio(
                                  "ori", 
                                  label = "Orientation:",
                                  inline = F, 
                                  status = status,
                                  choices = c(
                                    vertical = "x",
                                    horizontal = "y"
                                  ), 
                                  selected ="x"), 
                                a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Chi-Square-Goodness-of-Fit-Test", target="_blank")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   collapsible = F,
                                   status = status,
                                   type = "tabs",
                                   
                                   tabPanel("Frequency Tables", 
                                            icon = icon("table"),
                                            dataTableOutput("tf"),
                                            htmlOutput("chi"),
                                   ),
                                   tabPanel("Bar Chart", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("bar")
                                   ),
                                   tabPanel("Pie Chart", 
                                            icon = icon("chart-pie"),
                                            plotlyOutput("pie")
                                   )
                            )
                       )
                  ),
                  # 3. Chi-Square Test of Independence      
                  tabItem(tabName = "CT",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                
                                awesomeRadio("CT1", 
                                             label = "",
                                             choices = "", 
                                             selected ="",
                                             inline = F, 
                                             status = status
                                ),
                                awesomeRadio("CT2", 
                                             label = "",
                                             choices = "", 
                                             selected ="",
                                             inline = F, 
                                             status = status
                                ),
                                awesomeRadio(
                                  "ori2", 
                                  label = "Orientation:",
                                  inline = F, 
                                  status = status,
                                  choices = c(
                                    vertical = "x",
                                    horizontal = "y"
                                  ), 
                                  selected ="x"),
                                a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Chi-Square-Test-of-Independence/", target="_blank")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   collapsible = F,
                                   status = status,
                                   type = "tabs",
                                   
                                   tabPanel("Contingency Tables",
                                            icon = icon("table"),
                                            htmlOutput("chi2"), br(),
                                            h5("Observed Frequency"), br(),
                                            dataTableOutput("ctf"), br(),
                                            h5("Observed Frequency (%)"), br(),
                                            dataTableOutput("ctfp"), br(),
                                            h5("Expected Frequency"), br(),
                                            dataTableOutput("ctfe")
                                            
                                   ),
                                   tabPanel("Stacked Bar Chart (f)",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("graf2") 
                                   ),
                                   tabPanel("Stacked Bar Chart (%)", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("graf3")
                                   )
                            )
                        )
                  ),
                  # 4.  McNemar’s Test      
                  tabItem(tabName = "MCN",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                
                                awesomeRadio("MCN1", 
                                             label = "",
                                             choices = "", 
                                             selected ="",
                                             inline = F, 
                                             status = status
                                ),
                                awesomeRadio("MCN2", 
                                             label = "",
                                             choices = "", 
                                             selected ="",
                                             inline = F, 
                                             status = status
                                ),
                                awesomeRadio(
                                  "ori3", 
                                  label = "Orientation:",
                                  inline = F, 
                                  status = status,
                                  choices = c(
                                    vertical = "x",
                                    horizontal = "y"
                                  ), 
                                  selected ="x")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   collapsible = F,
                                   status = status,
                                   type = "tabs",
                                   
                                   tabPanel("Contingency Tables",
                                            icon = icon("table"),
                                            dataTableOutput("ctf2"), br(),
                                            dataTableOutput("ctfp2"),
                                            htmlOutput("mcn")
                                   ),
                                   tabPanel("Stacked Bar Chart (f)",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("graf4") 
                                   ),
                                   tabPanel("Stacked Bar Chart (%)", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("graf5")
                                   )
                            )
                        )
                  ),
                  # 4. Grouping Continuous Data      
                  tabItem(tabName = "GCD",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                awesomeRadio("GCD", 
                                             label = "",
                                             inline = F, 
                                             status = status,
                                             choices = "", 
                                             selected =""
                                ), hr(),
                                sliderInput("nb",
                                            "Number of Bins:",
                                            min = 3,
                                            max = 15,
                                            value = 7
                                ), hr(),
                                htmlOutput("br"),
                                a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Grouping-Continuous-Data/", target="_blank")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   status = status,
                                   type = "tabs",
                                   tabPanel("Frequency Distribution", 
                                            icon = icon("table"),
                                            dataTableOutput("ftab"),
                                   ),
                                   tabPanel("Histogram", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("hist")
                                   ),
                                   tabPanel("Frequency Polygons",
                                            icon = icon("chart-line"),
                                            plotlyOutput("poly")
                                   )
                             )
                          )
                    ),
                # 5. Descirptive Parameters       
                  tabItem(tabName = "DP",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                awesomeCheckboxGroup("DP", 
                                             label = "",
                                             inline = F, 
                                             status = status,
                                             choices = "", 
                                             selected =""),
                                awesomeRadio("DP2", 
                                             label = "",
                                             inline = F, 
                                             status = status,
                                             choices = "", 
                                             selected =""),
                                a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Descirptive-Parameters", target="_blank")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   status = status,
                                   collapsible = F,
                                   type = "tabs",
                                   tabPanel("Descirptive Parameters", 
                                            icon = icon("table"),
                                            dataTableOutput("des")
                                   ),
                                   tabPanel("Box and Whiskers plot", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("boxds")
                                   )
                            )
                        )
                  ),
                # 5. Testing for Normality       
                tabItem(tabName = "TN",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeRadio("TN", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected =""
                              ),
                              sliderInput("NB",
                                          "Number of Bins:",
                                          min = 1,
                                          max = 100,
                                          value = 7
                              ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Testing-for-Normality/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 7,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Testing for Normality", 
                                          icon = icon("note-sticky"), 
                                          textOutput("ks"),
                                          textOutput("lks"),
                                          textOutput("ad"),
                                          textOutput("sf"),
                                          textOutput("sw")
                                 ),
                                 tabPanel("Histogram", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("histogram")
                                 ),
                                 tabPanel("Box and Whiskers plot", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("box")
                                 ),
                                 tabPanel("QQ plot", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("qq")
                                 )
                          )
                      )
                ),
                # 6. Confidence Intervals for the Population Mean        
                tabItem(tabName = "PM",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeRadio("PM", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected =""
                              ),
                              sliderInput("PMp",
                                          "p-value:",
                                          min = 0.01,
                                          max = 0.99,
                                          value = 0.05
                              ),
                              sliderInput("PMn",
                                          "Sample Size:",
                                          min = 3,
                                          max = 500,
                                          value = 100
                              ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/CI-for-Population-Mean/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 6,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("CI for the Population Mean", 
                                          icon = icon("note-sticky"),
                                          htmlOutput("ci")
                                 ),
                                 tabPanel("Standard Error and Sample Size", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("gsem")
                                 )
                          )
                        )
                ),
                # 8.  Correlations       
                tabItem(tabName = "COR",
                        fluidRow(
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("COR", 
                                                 "Select Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""
                              ),
                              awesomeRadio("CORM", "Method:",
                                           inline = F, 
                                           status = status,
                                           choices = c(
                                             Pearson = "pearson",
                                             Spearman = "spearman",
                                             Kendall = "kendall"
                                           ),
                                           selected = "pearson"
                              ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Correlations-Analysis/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 maximizable = T,
                                 tabPanel("Correlations",
                                          icon = icon("table"),
                                          p("Marked correlations are significant at p < 0.05"),
                                          dataTableOutput("r")
                                 ),
                                 tabPanel("Level of Significance", 
                                          icon = icon("table"),
                                          p("Marked p-value < 0.05"),
                                          dataTableOutput("p")
                                 ),
                                 tabPanel("Scatter Plot", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("scatter")
                                 )
                          ),
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              tabPanel("Scatter Plot",
                                       icon = icon("chart-simple"),
                                       awesomeRadio("XX", 
                                                    label = "",
                                                    inline = F, 
                                                    status = status,
                                                    choices = "", selected =""),
                                       awesomeRadio("YY", 
                                                    label = "",
                                                    inline = F, 
                                                    status = status,
                                                    choices = "", selected ="")
                              )
                          ) 
                      )
                ),
                # 9.  Independent Samples T-Test 
                tabItem(tabName = "ISTT",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("DVITT", 
                                                 "Dependent Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", selected =""
                              ),
                              awesomeRadio("IVITT", 
                                           label = "Independent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("ISTD", 
                                           label = "Dependent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Independent-Samples-T%E2%80%90Test/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("T-test (more variables)",
                                          icon = icon("table"),
                                          dataTableOutput("titt")
                                 ),
                                 tabPanel("Descirptive Parameters", 
                                          icon = icon("table"),
                                          dataTableOutput("dest"),
                                          hr(),
                                          h5("Box and Whiskers plot"),
                                          plotlyOutput("boxt")
                                 ),
                                 tabPanel("T-test (one variable)", 
                                          icon = icon("note-sticky"),
                                          htmlOutput("t"),
                                          hr(),
                                          h5("Mean Plot with 95% Confidence Interval"),
                                          plotlyOutput("err")
                                 )
                          )
                     )
                ),
                # 10.  Dependent (Paired) Samples T-Test   
                tabItem(tabName = "DSTT",
                        fluidRow(
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("DSTDV", 
                                                 "Dependent Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", selected =""
                              ),
                              awesomeRadio("DSTI", 
                                           label = "Independent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("DSTD", 
                                           label = "Dependent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Dependent-Samples-T%E2%80%90Test/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("T-test (more variables)",
                                          icon = icon("table"),
                                          dataTableOutput("tdtt")
                                 ),
                                 tabPanel("Descirptive Parameters", 
                                          icon = icon("table"),
                                          dataTableOutput("dest2"),
                                          hr(),
                                          h5("Normality Test (Shapiro-Wilk)"),
                                          textOutput ("shapiro2"),
                                          hr(),
                                          h5("Box and Whiskers plot"),
                                          plotlyOutput("boxt2")

                                 ),
                                 tabPanel("T-test (one variable)", 
                                          icon = icon("note-sticky"),
                                          htmlOutput("t2"),
                                          hr(),
                                          h5 ("Mean Plot with 95% Confidence Interval"),
                                          plotlyOutput("err2")
                                 )

                          )
                      )
                ),
                # 11.  One Way ANOVA
                tabItem(tabName = "OWA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("ANOVADV", 
                                                 "Dependent Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", selected =""
                              ),
                              awesomeRadio("ANOVAI", 
                                           label = "Independent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("ANOVAD", 
                                           label = "Dependent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("ADJ", 
                                           label = "Method for adjusting p-values:",
                                           inline = F, 
                                           status = status,
                                           choices = c(
                                             none = "none",
                                             Tukey = "tukey",
                                             Scheffe = "scheffe",
                                             Bonferroni = "bonferroni",
                                             Holm  = "holm",
                                             Sidak ="sidak"),
                                           selected = "none"
                              ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/One-Way-ANOVA/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("ANOVA (more variable)",
                                          icon = icon("table"),
                                          dataTableOutput("arez")
                                 ),
                                 tabPanel("Descirptive Parameters", 
                                          icon = icon("table"),
                                          dataTableOutput("desa"),
                                          hr(),
                                          h5("Box and Whiskers plot"),
                                          plotlyOutput("bwp")
                                          
                                 ),
                                 tabPanel("ANOVA (one variable)", 
                                          icon = icon("note-sticky"),
                                          dataTableOutput("anovat"),
                                          hr(),
                                          htmlOutput("anova"),
                                          hr(),
                                          h5 ("Mean Plot with 95% Confidence Interval"),
                                          plotlyOutput("erra"),
                                          hr(),
                                          h5("Post Hoc Test for Multiple Comparisons"),
                                          dataTableOutput ("PH")
                                 )
                          )
                      )
                ),
                # 12. Repeated Measures ANOVA
                tabItem(tabName = "RMA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeRadio("ENT", 
                                           label = "Entities:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("RMAM", 
                                           label = "Measurements:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("RMADV", 
                                           label = "Dependent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("PHr", 
                                           "Post-hoc pairwise comparisons:",
                                           inline = F, 
                                           status = status,
                                           choices = c(
                                             none = "none",
                                             Tukey = "tukey",
                                             Scheffe = "scheffe",
                                             Bonferroni = "bonferroni",
                                             Holm  = "holm",
                                             Sidak ="sidak"),
                                           selected = "none"
                              )
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Descirptive Parameters", 
                                          icon = icon("table"),
                                          dataTableOutput("desrma"), 
                                          hr(),
                                          h5("Box and Whiskers plot"),
                                          plotlyOutput("bwprma")
                                          
                                 ),
                                 tabPanel("ANOVA", 
                                          icon = icon("note-sticky"),
                                          dataTableOutput("aovrm"), 
                                          hr(),
                                          htmlOutput("rma2"),
                                          hr(),
                                          h5 ("Mean Plot with 95% Confidence Interval"),
                                          plotlyOutput("errrma"),
                                          hr(),
                                          h5("Post Hoc Test for Multiple Comparisons"),
                                          dataTableOutput ("pwc")
                                 )
                          )
                      )
                ),
                # 12.  Regression Analysis (simple)
                tabItem(tabName = "SRA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeRadio("X", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected =""),
                              awesomeRadio("Y", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected ="")
                              
                          ),
                          tabBox(title = "",
                                 width = 6,
                                 status = status,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 type = "tabs",
                                 tabPanel("Regression Results", 
                                          icon = icon("note-sticky"),
                                          htmlOutput("rsra"), hr(),
                                          plotlyOutput("gsra")
                                 ),
                                 tabPanel("Predict Dependent Var.", 
                                          icon = icon("note-sticky"),
                                          numericInput("b0", "Intercept (b0):", 0, width = '220px'),
                                          numericInput("xx", "Independent value:", 0, width = '220px'),
                                          numericInput("pp", "Confidence interval (%):", 95, width = '220px'),
                                          htmlOutput("yy")
                                 ),
                                 tabPanel("Regression Scores", 
                                          icon = icon("table"),
                                          dataTableOutput("rs")
                          )
                       )
                    )
                ),
                # 13.  Regression Analysis (multipe)
                tabItem(tabName = "RA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("PRED", 
                                                 label = "",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""),
                              awesomeRadio("KRIT", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected =""),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Regression-Analysis/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Reg. Results", 
                                          icon = icon("table"),
                                          htmlOutput("ro"), hr(),
                                          dataTableOutput("rez")
                                 ),
                                 tabPanel("Graph (P)", 
                                          icon = icon("chart-simple"),
                                          h6("Graph of Partial Coefficients of Determinations (P)"),
                                          plotlyOutput("gro")
                                 ),
                                 tabPanel("Graph (Beta, Part R, R)", 
                                          icon = icon("chart-simple"),
                                          h6("Graph of Standardized Regression Coeff.(BETA), Partial Correlations (PART-R) and Correlations (R)"),
                                          plotlyOutput("gcoef")
                                 ),
                                 tabPanel("Reg. Scores", 
                                          icon = icon("table"),
                                          dataTableOutput("dep")
                                 )
                          )
                      )
                ),
                
                # 14.  Factor Analysis
                tabItem(tabName = "FA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("MV", 
                                                 "Select Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""
                              ),
                              sliderInput("k",
                                          "Number Principal Components:",
                                          step = 1,
                                          min = 1,
                                          max = 1,
                                          value = 1
                              ),
                              awesomeRadio("method2", 
                                           inline = F, 
                                           status = status,
                                           "Rotated Principal Components:",
                                           choices = c(none = "none",
                                                       varimax = "varimax",
                                                       quartimax = "quartimax",
                                                       oblimin = "oblimin",
                                                       promax = "promax"
                                           ),
                                           selected = "none"
                              ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Factor-Analysis/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Eigenvalue", 
                                          icon = icon("table"),
                                          dataTableOutput("Lamda"),
                                          htmlOutput("ssmc")
                                 ),
                                 tabPanel("Scree Plot", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("spgk")
                                 ),
                                 tabPanel("Pattern", 
                                          icon = icon("table"),
                                          dataTableOutput("A")
                                 ),
                                 tabPanel("Structure", 
                                          icon = icon("table"),
                                          dataTableOutput("FF")
                                 ),
                                 tabPanel("Correlation", 
                                          icon = icon("table"),
                                          dataTableOutput("M")
                                 ),
                                 tabPanel("Graph", 
                                          icon = icon("chart-simple"),
                                          h6("Graph Factor Loadings (Variables)"),
                                          plotlyOutput("GFF1"),
                                          h6("Graph Factor Loadings (Factors)"),
                                          plotlyOutput("GFF2")
                                 ),
                                 tabPanel("Scores", 
                                          icon = icon("table"),
                                          dataTableOutput("Scores")
                                 )
                          )
                      )
                ),
                # 15.  Canonical Analysis
                tabItem(tabName = "CA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("CA1", 
                                                 "Select Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""
                              ),
                              awesomeCheckboxGroup("CA2", 
                                                 "Select Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""
                              ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Canonical-Analysis/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Canonical Factor",
                                          icon = icon("table"),
                                          dataTableOutput("RC")
                                 ),
                                 tabPanel("Structure",
                                          icon = icon("table"),
                                          h6("Factor Structure 1. Set"),
                                          dataTableOutput("F1"),
                                          hr(),
                                          h6("Factor Structure 2. Set"), 
                                          dataTableOutput("F2"),
                                 ),
                                 tabPanel("Graph", 
                                          icon = icon("chart-simple"),
                                          h6("Graph Factor Structure 1. Set"),
                                          plotlyOutput("GFFF1"),
                                          hr(),
                                          h6("Graph Factor Structure 2. Set"),
                                          plotlyOutput("GFFF2"),
                                 ),
                                 tabPanel("Scores 1. Set", 
                                          icon = icon("table"),
                                          dataTableOutput("CF1")
                                 ),
                                 tabPanel("Scores 2. Set", 
                                          icon = icon("table"),
                                          dataTableOutput("CF2")
                                 )
                           )
                      )
                ),
                # 16.  Discriminant Analysis
                tabItem(tabName = "DA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("DAD", 
                                                   "Select Variables:",
                                                   inline = F, 
                                                   status = status,
                                                    choices = "", 
                                                    selected =""
                              ),
                              awesomeRadio("DAI", 
                                           label = "Independent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected =""
                              ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Discriminant-Analysis/", target="_blank")
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("DF", 
                                          icon = icon("table"),
                                          dataTableOutput("df")
                                 ),
                                 tabPanel("Structure", 
                                          icon = icon("table"),
                                          h6("Structure Discriminant Functions"),
                                          dataTableOutput("sdf")
                                 ),
                                 tabPanel("Centroids",
                                          icon = icon("table"),
                                          h6("Group Centroids"),
                                          dataTableOutput("cg")
                                 ),
                                 tabPanel("Classifications",
                                          icon = icon("table"),
                                          h6("Classifications Matrix (Frequency)"),
                                          dataTableOutput("clas1"),
                                          hr(),
                                          h6("Classifications Matrix (%)"),
                                          dataTableOutput("clas2"),
                                          hr(),
                                          "Rows: Actual classifications, Columns: Predicted classifications"
                                 ),
                                 tabPanel("Graph",
                                          icon = icon("chart-simple"),
                                          h6("Graph Structure Discriminant Functions"),
                                          plotlyOutput("gsdf")
                                 ),
                                 tabPanel("Scores", 
                                          icon = icon("table"),
                                          dataTableOutput("dfs")
                                 )
                          )
                      )
                ),
                # 17.  Cluster Analysis
                tabItem(tabName = "CLA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("CLV", 
                                                   "Select Variables:",
                                                   inline = F, 
                                                   status = status,
                                                   choices = "", 
                                                   selected =""
                              ),
                              awesomeRadio("dist", "Distances:",
                                           inline = F, 
                                           status = status,
                                           choices = c(euclidean = "euclidean",
                                                       maximum = "maximum",
                                                       manhattan = "manhattan",
                                                       canberra = "canberra",
                                                       minkowski = "minkowski"
                                           ),
                                           selected = "euclidean"
                              ),
                              awesomeRadio("clmethod", "Method:",
                                           inline = F, 
                                           status = status,
                                           choices = c(ward.D = "ward.D",
                                                       ward.D2 = "ward.D2",
                                                       single = "single",
                                                       complete = "complete",
                                                       average = "average",
                                                       mcquitty = "mcquitty",
                                                       median = "median",
                                                       centroid ="centroid"
                                           ),
                                           selected = "ward.D"
                              ),
                              sliderInput("nk",
                                          "Number Clusters:",
                                          step = 1,
                                          min = 1,
                                          max = 1,
                                          value = 1
                              )
                          ),
                          box(title = "Groups",
                              icon = icon("table"),
                              width = 2,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              dataTableOutput("clus")
                          ),
                          box(title = "Cluster dendrogram",
                              icon = icon("sitemap"),
                              width = 8,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              collapsible = F,
                              status = status,
                              plotlyOutput("dend")
                          )
                      )
                ),
                # 18.  Reliability Analysis
                tabItem(tabName = "REA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("REA", 
                                                 "Select Variables:",
                                                 choices = "", 
                                                 selected =""
                              ),
                              hr(),
                              h5("Intraclass Correlation"),
                              awesomeRadio("model", "Model:",
                                           inline = F, 
                                           status = status,
                                           c("Oneway" = "one",
                                             "Twoway" = "two")),
                              awesomeRadio("type", "Type:",
                                           inline = F, 
                                           status = status,
                                           c("Consistency" = "con",
                                             "Agreement" = "agr")),
                              awesomeRadio("unit", "Unit:",
                                           inline = F, 
                                           status = status,
                                           c("Single" = "sin",
                                             "Average" = "ave")
                                           ),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Reliability-Analysis/", target="_blank")
                          ),
                          box(title = "Reliability Analysis Results",
                              icon = icon("table"),
                              width = 5, 
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              h5("Reliability Coefficients"),
                              htmlOutput("alpha"),
                              hr(),
                              h5("Intraclass Correlation"),
                              htmlOutput("icc"),
                              hr(),
                              h5("Item Reliability Statistics"),
                              dataTableOutput("irs")
                          ),
                          box(title = "Condensed Data",
                              icon = icon("table"),
                              width = 3, 
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              dataTableOutput("cd")
                          )
                        )
                ),
                # 19.  Probability Calculator
                # Normal (Gauss) Distribution
                tabItem(tabName = "PCN",
                        fluidRow(
                          box(title = "Input Values",
                              icon = icon("pen"),
                              width = 2,
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              numericInput("as1", "Mean", 180, width = '150px'),
                              verbatimTextOutput("as1"),
                              numericInput("sd1", "Standard Deviation", 10, width = '150px'),
                              verbatimTextOutput("sd1"),
                              numericInput("rez1", "Value (X)", 190, width = '150px'),
                              verbatimTextOutput("rez1"), hr(),
                              htmlOutput("px"),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Normal-Distribution/", target="_blank")
                          ),
                          box(title = "Normal (Gauss) Distribution",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("grafnd")
                          )
                        )
                ),
                tabItem(tabName = "PCT",
                        fluidRow(
                          box(title = "Input Values",
                              icon = icon("pen"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 2,
                              numericInput("dft", 
                                           "df", 
                                           value = 3, 
                                           min = 1,
                                           max = 500,
                                           step = 1,
                                           width = '150px'
                              ),
                              numericInput("pt", 
                                           "p-value", 
                                           value = 0.05, 
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           width = '150px'
                              ), br(),
                              htmlOutput("tp"), hr(),
                              numericInput("xt", 
                                           "t-value", 
                                           value = 2, 
                                           min = 0,
                                           max = 10,
                                           step = 0.01,
                                           width = '150px'
                              ), br(),
                              htmlOutput("pt"),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/T-%E2%80%93-Distribution/", target="_blank")
                          ),
                          box(title = "T - Distribution - Upper tailed",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("graftd")
                          ),
                          box(title = "T - Distribution - Two tailed",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("graftd2")
                          )
                        )
                ),
                tabItem(tabName = "PCF",
                        fluidRow(
                          box(title = "Input Values",
                              icon = icon("pen"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 2,
                              numericInput("df1", 
                                           "df1", 
                                           value = 3, 
                                           min = 1,
                                           max = 100,
                                           step = 1,
                                           width = '150px'
                              ),
                              numericInput("df2", 
                                           "df2", 
                                           value = 100, 
                                           min = 1,
                                           max = 500,
                                           step = 1,
                                           width = '150px'
                              ),
                              numericInput("pf", 
                                           "p-value", 
                                           value = 0.05, 
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           width = '150px'
                              ), br(),			           
                              htmlOutput("fp"), hr(),
                              numericInput("xf", 
                                           "F-value", 
                                           value = 3, 
                                           min = 0,
                                           max = 10,
                                           step = 0.01,
                                           width = '150px'
                              ),br(),
                              htmlOutput("pf"),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/F-%E2%80%93-Distribution/", target="_blank")
                          ),
                          box(title = "F-Distribution",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("graff")
                          )
                        )
                ),
                tabItem(tabName = "PCH",
                        fluidRow(
                          box(title = "Input Values",
                              icon = icon("pen"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 2,
                              numericInput("dfh", 
                                           "df", 
                                           value = 10, 
                                           min = 1,
                                           max = 30,
                                           step = 1,
                                           width = '150px'
                              ),
                              numericInput("ph", 
                                           "p-value", 
                                           value = 0.05, 
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           width = '150px'
                              ), br(),
                              htmlOutput("hp"), hr(),
                              numericInput("xh", 
                                           "Chi-value", 
                                           value = 10, 
                                           min = 0,
                                           max = 100,
                                           step = 0.1,
                                           width = '150px'
                              ), br(),
                              htmlOutput("ph"),
                              a(icon("question"), "Help", href="https://github.com/dd-km/R-STAT/wiki/Chi-Squere-%E2%80%93-Distribution /", target="_blank")
                          ),
                          box(title = "Chi-Square Distribution",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("grafh")
                          )
                      )
                  ),
                # Import Data
                tabItem(tabName = "data",
                        fluidRow(
                          box(title = "Choose Data File",
                              width = 4,
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              icon = icon("folder-open"),
                              import_file_ui(
                                id = "myid",
                                preview_data = T,
                                file_extensions = c(".csv", ".xls", ".xlsx", ".sav", ".ods")
                              ), 
                              htmlOutput("nm"),
                              hr(),
                              actionButton("do", "Export File to Database")
                          )
                      )
                ),
                  # 22. About   
                  tabItem(tabName = "about",
                          fluidRow(
                            box(title = "About",
                                icon = icon("circle-info"),
                                width = 12,
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                tags$img(src=home), 
                                br(),
                                h4("Version: 2.0"),
                                hr(),
                                h6("Built on: 21.10.2023."),
                                h6("Licence: MIT LicenseCopyright (c) 2021 Quantitative-Methods"),
                                h6("Permission is hereby granted, free of charge, to any person obtaining a copy
                    							of this software and associated documentation files (the 'Software'), to deal
                    							in the Software without restriction, including without limitation the rights
                    							to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
                    							copies of the Software, and to permit persons to whom the Software is 
                    							furnished to do so, subject to the following conditions:
                    							The above copyright notice and this permission notice shall be included in all
                    							copies or substantial portions of the Software."),
                                hr(),
                                h6("THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
                    							IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
                    							FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
                    							AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
                    							LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
                    							OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
                    							SOFTWARE."),
                                hr(),
                                h6("Authors: Dražan Dizdar & Darko Katović"),
                                hr(),
                                h5(a("Source", href="https://github.com/dd-km/R-STAT"))
                              )
                          )
                  )

            )
      )
)
