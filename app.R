# app.R
# ---------------------------------------------------------------------------
# Shiny app developed for Edutrack. Click on "Run app" in the RStudio IDE to execute.
# ---------------------------------------------------------------------------

library(shiny)
library(bslib)
library(shinyWidgets)
library(ggplot2)
library(ggiraph)
library(sf)
library(dplyr)
library(readr)

# ---- Load pre-built world polygons ----------------------------------------
world <- readRDS("world_cache.rds")

var_labels <- c(
  ftrack =  "educational tracking",
  length_unesco =  "% tracked curriculum",
  age_oecd = "age of first selection",
  ntrack_oecd = "# edu. programs, age 15"
)

# Mapping: variable code -> short description
var_desc <- c(
  ftrack =  "Factor score of educational tracking using principle component factor analysis, based on three indicators: age of first selection, number of educational programs, of tracked curriculum), mean of 0 and a standard deviation of 1 [own calculation]",
  length_unesco =  "Percentage of the total curriculum that is tracked [Source: UNESCO/OECD]",
  age_oecd = "The age at which students are first sorted into different school types, educational programs, or vocational streams [Source: OECD/PISA]",
  ntrack_oecd = "The number of educational programs available to 15-year-old students [Source: OECD/PISA]"
)

# ---- Data: read once from example_scores.csv ------------------------------
# CSV layout: iso3, year, then ONE COLUMN PER VARIABLE. Every column that
# isn't iso3/year becomes a selectable variable in the dropdown.
scores <- readr::read_csv("Dataset_Pergap_Edutrack_Schneider_Pomianowicz_2026_v01_long.csv", show_col_types = FALSE)
scores <- scores %>% select(iso3c, year, ftrack, length_unesco, age_oecd, ntrack_oecd)
scores$iso3 <- toupper(trimws(as.character(scores$iso3c)))
scores$year <- suppressWarnings(as.integer(scores$year))
scores <- scores[!is.na(scores$year), , drop = FALSE]

# Everything except iso3/year is a variable; make sure it's numeric.
value_vars <- c("ftrack", "length_unesco", "age_oecd", "ntrack_oecd")

all_years <- sort(unique(scores$year))   # for the year buttons

# ---- UI -------------------------------------------------------------------
# ---- UI -------------------------------------------------------------------
ui <- page_fillable(
  fillable_mobile = FALSE,   # let things stack at natural height on phones
  tags$head(tags$style(HTML(
    ".selectize-input { font-size: 13px; }
     .selectize-dropdown { font-size: 13px; }
     #var_description, #data_source { font-size: 12px; }
     @media (min-width: 768px) {
       #var_description, #data_source { font-size: 15px; }
     }"
  ))),
  layout_sidebar(
    sidebar = sidebar(
      width = 260,
      open  = list(desktop = "open", mobile = "always-above"),
      selectInput(
        "variable", "Variable",
        choices = c(
          "educational tracking"    = "ftrack",
          "% tracked curriculum"    = "length_unesco",
          "age of first selection"  = "age_oecd",
          "# edu. programs, age 15" = "ntrack_oecd"
        ),
        selected = value_vars[1],
        width    = "100%"
      ),
      sliderTextInput(
        "year", "Year",
        choices  = all_years,
        selected = 2012,
        grid     = TRUE,
        width    = "100%",
        animate  = animationOptions(interval = 1500, loop = TRUE)
      ),
      div(
        style = "text-align: justify; hyphens: auto;",
        lang = "en",
        textOutput("var_description", container = tags$div)
      ),
      div(
        id = "data_source",
        "EduTrack Data: ",
        tags$a(
          href   = "https://doi.org/10.34810/DATA3646",
          target = "_blank",
          "doi.org/10.34810/DATA3646"
        )
      )
    ),
    girafeOutput("map", width = "100%")
  )
)

# ---- Server ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # Update description based on selected variable
  output$var_description <- renderText({
    var_desc[input$variable]
  })
  
  # The buttons return the chosen year as a string.
  sel_year <- reactive({
    req(input$year)
    as.integer(input$year)
  })
  
  # The dropdown returns the chosen variable (a column name).
  sel_var <- reactive({
    req(input$variable)
    input$variable
  })
  
  # Colour limits fixed across ALL years for the chosen variable, so the year
  # animation stays comparable; they re-fit when you switch variables.
  pal_limits <- reactive({
    range(scores[[sel_var()]], na.rm = TRUE)
  })
  
  # --- Play / pause: step through the years on a 1s timer ------------------
  playing <- reactiveVal(FALSE)
  
  observeEvent(input$play, {
    playing(!playing())
    updateActionButton(
      session, "play",
      label = if (playing()) "\u23F8 Pause" else "\u25B6 Play"
    )
  })
  
  observe({
    if (!playing()) return()
    invalidateLater(1000, session)         # re-run every second while playing
    isolate({
      idx <- match(as.integer(input$year), all_years)
      nxt <- all_years[(idx %% length(all_years)) + 1]   # advance, looping
      updateRadioGroupButtons(session, "year", selected = as.character(nxt))
    })
  })
  
  output$map <- renderGirafe({
    yr  <- sel_year()
    var <- sel_var()
    
    # Pull the chosen year + variable, renaming the column to `value`.
    yr_dat <- dplyr::transmute(
      dplyr::filter(scores, year == yr),
      iso3, value = .data[[var]]
    )
    dat <- dplyr::left_join(world, yr_dat, by = "iso3")
    
    # dat$tip <- sprintf(
    #   "<b>%s</b><br/>%s (%d): %s",
    #   dat$country_name, var, yr,
    #   ifelse(is.na(dat$value), "no data", format(round(dat$value, 2)))
    # )
    
    dat$tip <- sprintf(
      "<b>%s</b><br/>%s",
      dat$country_name,
      ifelse(is.na(dat$value), "no data", format(round(dat$value, 2)))
    )
    
    # Reverse the colour ramp only for "Age of first selection"
    dir <- if (var == "age_oecd") -1 else 1
    
    gg <- ggplot(dat) +
      geom_sf_interactive(
        aes(fill = value, tooltip = tip, data_id = iso3),
        color = "white", linewidth = 0.1
      ) +
      scale_fill_viridis_c(limits = pal_limits(), na.value = "grey90",
                           name = var_labels[var],
                           option = "rocket",
                           direction = dir) +
      coord_sf(datum = NA, expand = FALSE) +
      #labs(title = paste0(var, ", ", yr)) +
      guides(fill = guide_colorbar(
        barwidth = unit(11, "cm"), barheight = unit(0.4, "cm")
      )) +
      labs(caption = "© PERGAP") +
      theme_void(base_size = 13) +
      theme(
        # plot.title = element_text(face = "bold", hjust = 0.5,
        #                           margin = margin(b = 6)),
        plot.margin           = margin(0, 0, 0, 0),
        legend.margin         = margin(t = 2, b = 0),
        legend.position       = "bottom",
        legend.title.position = "top",                 # title ABOVE the bar
        legend.title          = element_text(hjust = 0.5, size = 11)
        #legend.margin         = margin(t = 4),
      )
    
    girafe(
      ggobj = gg, width_svg = 10, height_svg = 5.4,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_selection(type = "none"),  # disables lasso & click selection
        opts_hover(css = "stroke:#333;stroke-width:0.8px;"),
        opts_tooltip(
          css = paste(
            "background:#fff;border:1px solid #ccc;border-radius:4px;",
            "padding:4px 8px;font-size:13px;color:#222;"
          )
        ),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })
}

shinyApp(ui, server)