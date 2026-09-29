dashboardPage(
  dashboardHeader(title = "TSGenerator 2.0 — ST"),
  dashboardSidebar(sidebarMenu(menuItem("Seasonal Trajectories", tabName="st", icon=icon("download")))),
  dashboardBody(useShinyjs(), tabItems(tabItem(tabName="st",
    fluidRow(box(width=4, title="WEkEO / HDA", status="primary", solidHeader=TRUE,
      textInput("user","WEkEO username (optional)"), passwordInput("password","WEkEO password (optional)"),
      helpText("Leave credentials blank to use ~/.hdarc."), actionButton("check","Check WEkEO", icon=icon("plug")),
      verbatimTextOutput("diag"))),
    fluidRow(box(width=6, title="ST request", status="primary", solidHeader=TRUE,
      dateInput("start","Start date", value=Sys.Date()-30), dateInput("end","End date", value=Sys.Date()),
      checkboxGroupInput("product","Product", choices=c("PPI","QFLAG"), selected="PPI"),
      textInput("tile","Sentinel-2 tile", placeholder="30TXM"),
      textInput("outdir","Output directory", value=file.path(getwd(),"HRVPP_ST")),
      checkboxInput("overwrite","Overwrite existing files", FALSE),
      actionButton("preview","Preview", icon=icon("search")), actionButton("download","Download", icon=icon("download"))
    ), box(width=6, title="Request summary", status="info", solidHeader=TRUE, tableOutput("summary"), verbatimTextOutput("status")))
  )))
)
