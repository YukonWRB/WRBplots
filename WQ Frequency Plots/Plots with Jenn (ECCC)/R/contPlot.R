#===============================================================================
# CONTINUITY PLOT FUNCTION
#===============================================================================


contPlot <- function(dat, 
                     datevar = Date, yvar = VariableName, 
                     color_var=NULL, color_title = NULL,
                     facet_var = NULL, 
                     xlabel = NULL, ylabel = NULL, 
                     title = NULL, subtitle=NULL){
  
  p1 <- ggplot2::ggplot(data = dat, aes(x = {{datevar}}, y = {{yvar}})) +
    
    ggplot2::geom_point(shape=1, 
                        aes(color= if(is.null(color_var)){NULL} else factor(.data[[color_var]]))) +
    
    #format date axis -- this will show yearly breaks
    ggplot2::scale_x_date(date_breaks ="1 year", date_labels="%Y") +
    
    #add lables/title
    ggplot2::labs(
      title=title , 
      subtitle = subtitle, 
      color=color_title,   
      x=xlabel,                       
      y=ylabel) +
  
    #facet_by group
    ggplot2::facet_grid(rows = vars({{facet_var}}), 
                        scales="free_y", space="free", drop=TRUE,
                        labeller = label_wrap_gen(width = 17, multi_line = TRUE)) +
    
    #add whatever theme elements you want
    ggplot2::theme_bw() +  
    ggplot2::theme(
          axis.title.x=element_blank(), 
          axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "right", 
          legend.justification = "top")
  
  
  return(p1)
}



