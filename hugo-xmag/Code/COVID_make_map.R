# Data source ----
# JHU CSSE  ---- 
# https://github.com/CSSEGISandData/COVID-19

suppressPackageStartupMessages({
  library("data.table")
  library("ggplot2")
  library("readr")
})


#' world daily report
#' 
get.JHU.daily <- function(){
  url_daily_report <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/06-15-2020.csv")
  dt_JUH <- fread(url_daily_report)
  dt_JUH
}

#' USA daily report by state
#' https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports_us
get.JHU.us.state <- function(){
  url_us_daily_report <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/06-15-2020.csv")
  dt_JUH_US <- fread(url_us_daily_report)
  dt_JUH_US
}

# make maps ---- 
#' get centroids from a shp file dataset
get.centroids <- function(
  data = dt1, 
  long = "long", lat = "lat", 
  by_var = "state",  # the grouping variable, e.g. state: get centroid by state
  fill_var = NULL # the variable to plot 
){
  data <- data[!is.na(data[[by_var]]),]
  data[[by_var]] <- as.character(data[[by_var]]) # sometimes there is empty factor level
  dt1_df <- sp::SpatialPointsDataFrame(coords = data[, c(long, lat), with = FALSE], data = data)
  dt1_geo <- by(dt1_df, dt1_df[[by_var]], function(x) {sp::Polygon(x[c(long, lat)])@labpt})
  centroids <- stats::setNames(do.call("rbind.data.frame", dt1_geo), c(long, lat))
  centroids$name <- names(dt1_geo) 
  if(!is.null(fill_var)){ # if need to join fill value 
    setkeyv(setDT(centroids), "name")
    dt_var <- unique(data[,c(by_var, fill_var), with = FALSE])
    setkeyv(dt_var, by_var)
    centroids <- dt_var[centroids]
  }
  return(centroids)
}

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1, 1)), substring(c, 2),
        sep="", collapse=" ")
}

get_state_name <- function(){
  states <- setDT(ggplot2::map_data("state"))
  states[, subregion:= NULL]
  setkey(states, region)
  
  abb_names <- data.table(state = tolower(state.name), abb = state.abb) # these are R dataset
  abb_names$state_upper <- sapply(abb_names$state, CapStr)
  setkey(abb_names, state)
  
  states <- abb_names[states]
  states[!is.na(abb)] # remove DC
}

make_heatmap <- function(
  data, 
  geo_data,
  state_var, 
  fill_var, 
  label_var = NULL, # for US label using abbreviation
  fill_var_label = NULL
){
  if(is.null(fill_var_label)) fill_var_label <- sub("_", " ", fill_var)
  if(is.null(label_var)) label_var <- "state"
  dt1 <- setDT(data)[, c(state_var, fill_var), with = FALSE]
  setnames(dt1, c("state", "value"))
  
  # remove NA state
  dt1 <- dt1[!is.na(state)]
  dt_state <- geo_data
  
  if(dt1$state[1]%in% unique(dt_state$state)) {
    dt1 <- dt1[state%in%unique(dt_state$state),]
    setkey(dt_state, state)
    setkey(dt1, state)
    
  } else if (dt1$state[1]%in% unique(dt_state$abb)) {
    dt1 <- dt1[state%in%unique(dt_state$abb),]
    setnames(dt1, "state", "abb")
    setkey(dt_state, abb)
    setkey(dt1, abb)
    
  } else if(dt1$state[1]%in% unique(dt_state$state_upper)){
    dt1 <- dt1[state%in%unique(dt_state$state_upper),]
    setnames(dt1, "state", "state_upper")
    setkey(dt_state, state_upper)
    setkey(dt1, state_upper)
  } else {stop("Cannot match state names, three allowed formats: ", paste(dt_state[1, 1:3], collapse = ", "))}
  
  # join data to map: left join states to dt1
  dt2 <- dt1[dt_state]
  dt2
  
  if(nrow(dt2)>0){
    centroids <- get.centroids(data = dt2, long = "long", lat = "lat", by_var = label_var, fill_var = "value")
    
    x_boundary = -77
    gg1 = ggplot(data = dt2, aes(x = long, y = lat, group = group))+
      geom_polygon(aes(fill = value))+
      geom_path()+ 
      scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "GnBu"), 
                           na.value = "grey90",
                           guide = guide_colourbar(barwidth = 25, barheight = 0.4,
                                                   #put legend title on top of legend
                                                   title.position = "top")) +
      # shadowtext::geom_shadowtext(data = centroids,
      #                             aes(x = long, y = lat, label = abb),
      #                             size = 3,
      #                             inherit.aes = FALSE) + 
      # if need to repel labels... could further finetune
      ggrepel::geom_label_repel(data = centroids[long >= x_boundary,],
                                aes_string(x = "long", y = "lat", label = label_var, fill = "value"),
                                force = 1, size = 3,
                                inherit.aes = F
      ) +
      # the normal labels:
      geom_text(data=centroids[long < x_boundary,],
                aes_string(x = "long", y = "lat", label = label_var),
                size = 3, inherit.aes=F) +
      labs(fill = fill_var_label, x = "Longitude", y = "Latitude") + 
      coord_map() + 
      # map scale
      ggsn::scalebar(data = dt2, dist = 500, dist_unit = "km",
                     border.size = 0.4, st.size = 3,
                     box.fill = c('black','white'),
                     transform = TRUE, model = "WGS84") + 
      theme_void() + 
      # put legend at the bottom, adjust legend title and text font sizes
      theme(legend.position = "bottom",
            legend.title=element_text(size=12),  # font size of the legend 
            legend.text=element_text(size=10))
    
    # return(list(gg_us_map = gg1, dt_map = dt2))
    return(gg1)
  } else {
    return(NULL)
  }
}

