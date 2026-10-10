dashboardPage(
  dashboardHeader(title = "TSGenerator 2.0 — VPP"),
  dashboardSidebar(sidebarMenu(menuItem("VPP parameters", tabName="vpp", icon=icon("download")))),
  dashboardBody(useShinyjs(), tabItems(tabItem(tabName="vpp",
    fluidRow(box(width=4, title="WEkEO / HDA", status="primary", solidHeader=TRUE,
      textInput("user","WEkEO username (optional)"), passwordInput("password","WEkEO password (optional)"),
      helpText("Leave credentials blank to use ~/.hdarc."), actionButton("check","Check WEkEO", icon=icon("plug")),
      verbatimTextOutput("diag"))),
    fluidRow(box(width=6, title="VPP request", status="primary", solidHeader=TRUE,
      dateInput("start","Start date", value=as.Date(paste0(format(Sys.Date(),"%Y"),"-01-01"))), dateInput("end","End date", value=Sys.Date()),
      selectizeInput("product","Parameters", multiple=TRUE,
        choices=c("MINV","MAXD","LENGTH","SOSD","QFLAG","EOSV","TPROD","MAXV","AMPL","SOSV","LSLOPE","EOSD","RSLOPE","SPROD"),
        selected=c("SOSD","MAXD","EOSD","LENGTH")),
      checkboxGroupInput("season","Season", choices=c("s1","s2"), selected="s1"),
      textInput("tile","Sentinel-2 tile", placeholder="30TXM"), textInput("outdir","Output directory", value="", placeholder="Choose a directory before downloading"),
      checkboxInput("overwrite","Overwrite existing files", FALSE),
      actionButton("preview","Preview", icon=icon("search")), actionButton("download","Download", icon=icon("download"))
    ), box(width=6, title="Request summary", status="info", solidHeader=TRUE, tableOutput("summary"), verbatimTextOutput("status")))
  )))
)
