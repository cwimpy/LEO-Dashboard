# Interactive Map Setup Guide

The dashboard now includes an Interactive Map tab, but to display clickable jurisdictions, you'll need to add geographic boundary files.

## Current Status

✅ **Implemented:**
- Map tab with Leaflet integration
- Metric selection dropdown (Turnout Rate, Mail Return Rate, etc.)
- Basic map centered on the US
- Framework for displaying jurisdiction data

⚠️ **Requires Geographic Data:**
- County boundary shapefiles or GeoJSON files
- Without these, the map shows a base map but no jurisdiction boundaries

## How to Add Geographic Boundaries

### Option 1: US Census Bureau TIGER/Line Shapefiles (Recommended)

**Download County Boundaries:**
1. Visit: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
2. Select the most recent year (e.g., 2023)
3. Download "Counties (and equivalent)" shapefile
4. File: `tl_2023_us_county.zip`

**Install Required R Package:**
```r
install.packages("sf")  # Spatial features package
```

**Add to Dashboard:**
```r
# At the top of eavs_dashboard.R, add:
library(sf)

# In the server function, load the shapefile:
county_shapes <- st_read("path/to/tl_2023_us_county.shp")

# Merge with jurisdiction data (using FIPS codes):
county_shapes <- county_shapes %>%
  mutate(fips_code = paste0(STATEFP, COUNTYFP)) %>%
  left_join(jurisdiction_data, by = "fips_code")

# In the map render function, add polygons:
leaflet(county_shapes) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric("YlOrRd", turnout_rate)(turnout_rate),
    fillOpacity = 0.7,
    color = "#444",
    weight = 1,
    popup = ~popup_text,
    layerId = ~fips_code,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9
    )
  )

# Add click handler:
observeEvent(input$jurisdiction_map_shape_click, {
  clicked <- input$jurisdiction_map_shape_click
  fips <- clicked$id

  # Navigate to jurisdiction profile
  # (update state and jurisdiction dropdowns)
})
```

### Option 2: Simplified GeoJSON (Smaller File Size)

**Use TopoJSON or Simplified GeoJSON:**
- Smaller file size, faster loading
- Available from: https://github.com/deldersveld/topojson or https://eric.clst.org/tech/usgeojson/

**Example:**
```r
library(geojsonio)

# Load GeoJSON
counties <- geojson_read("us-counties.json", what = "sp")

# Convert to sf object
counties_sf <- st_as_sf(counties)
```

### Option 3: State-Level Map (Quick Implementation)

For a quick implementation showing just states:

```r
# Install and use built-in state boundaries
install.packages("maps")
library(maps)

# Get state boundaries
states_map <- map("state", fill = TRUE, plot = FALSE)
states_sf <- st_as_sf(states_map)

# Aggregate data by state
state_data <- jurisdiction_data %>%
  group_by(state_abbr) %>%
  summarise(
    avg_turnout = mean(turnout_rate, na.rm = TRUE),
    avg_mail_return = mean(mail_return_rate, na.rm = TRUE),
    jurisdictions = n()
  )

# Add to map
leaflet(states_sf) %>%
  addPolygons(...)
```

## Recommended Implementation Steps

1. **Download Census TIGER/Line county shapefile** (Option 1 above)
2. **Install `sf` package** in R
3. **Add shapefile loading** to the dashboard startup
4. **Update map render function** to display polygons with data
5. **Add click handler** to navigate to jurisdiction profile
6. **Add color scale legend** for the selected metric

## Example Complete Map Code

Here's a complete example you can add to the dashboard:

```r
# Load geographic data (add near top of server function)
county_boundaries <- reactive({
  tryCatch({
    shapes <- st_read("data/tl_2023_us_county.shp", quiet = TRUE)
    shapes %>%
      mutate(
        fips_code = paste0(STATEFP, COUNTYFP),
        fips_code = sprintf("%05d", as.numeric(fips_code))
      )
  }, error = function(e) {
    cat("ERROR loading shapefile:", e$message, "\n")
    NULL
  })
})

# Update map render
output$jurisdiction_map <- renderLeaflet({
  req(county_boundaries())

  shapes <- county_boundaries() %>%
    left_join(data()$jurisdiction, by = "fips_code")

  # Create color palette
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = shapes$turnout_rate,
    na.color = "#808080"
  )

  # Create popups
  popups <- sprintf(
    "<strong>%s, %s</strong><br/>
    Registered: %s<br/>
    Turnout Rate: %.1f%%<br/>
    <em>Click to view details</em>",
    shapes$jurisdiction_name,
    shapes$state_abbr,
    format(shapes$a1a, big.mark = ","),
    shapes$turnout_rate
  )

  # Render map
  leaflet(shapes) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(turnout_rate),
      fillOpacity = 0.7,
      color = "#444",
      weight = 1,
      popup = popups,
      layerId = ~fips_code,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#CC092F",
        fillOpacity = 0.9
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~turnout_rate,
      title = "Turnout Rate",
      opacity = 0.7
    )
})

# Handle clicks
observeEvent(input$jurisdiction_map_shape_click, {
  clicked <- input$jurisdiction_map_shape_click
  fips <- clicked$id

  # Get state for this FIPS code
  juris_info <- data()$jurisdiction %>%
    filter(fips_code == fips) %>%
    slice(1)

  if (nrow(juris_info) > 0) {
    # Navigate to jurisdiction profile tab
    updateTabsetPanel(session, "tabs", selected = "jurisdiction_profile")

    # Update dropdowns
    updateSelectInput(session, "profile_state", selected = juris_info$state_abbr)
    updateSelectInput(session, "profile_jurisdiction", selected = fips)
  }
})
```

## Data Requirements

- **FIPS Codes**: 5-digit codes matching between shapefile and jurisdiction data
- **County Names**: For validation and display
- **Coordinate System**: WGS84 (EPSG:4326) for web mapping

## Performance Tips

1. **Simplify boundaries**: Use `st_simplify()` to reduce polygon complexity
2. **Subset data**: Only show selected states or regions initially
3. **Use TopoJSON**: More efficient than shapefiles for web display
4. **Add loading indicators**: Maps can take time to render

## Alternative: Point-Based Map

If you want a simpler implementation without boundary files:

```r
# Create point map using jurisdiction centroids
# (requires lat/lon coordinates in your data)

leaflet(jurisdiction_data) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~sqrt(a1a) / 100,  # Size by registered voters
    color = ~colorNumeric("YlOrRd", turnout_rate)(turnout_rate),
    popup = ~popup_text,
    layerId = ~fips_code
  )
```

## Resources

- **US Census TIGER/Line**: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
- **Leaflet for R**: https://rstudio.github.io/leaflet/
- **sf Package**: https://r-spatial.github.io/sf/
- **Eric Celeste's GeoJSON**: https://eric.clst.org/tech/usgeojson/
- **TopoJSON Repository**: https://github.com/deldersveld/topojson

## Next Steps

1. Download county boundary shapefile
2. Install `sf` package
3. Add the example code above to your dashboard
4. Test with a single state first
5. Expand to full US map

Once implemented, users will be able to:
- See all jurisdictions colored by selected metric
- Click any jurisdiction to view its full profile
- Compare jurisdictions visually on the map
- Zoom and pan to explore different regions
