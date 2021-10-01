###################################################
#    Function: Create forest plot using gtsummary output
#
# Describe: Using the gtsummary tbl_uvregression and tbl_regression output to create basic forestplot
#
# @Inputs:
# @gtsum_tb: gtsummary output from tbl_uvregression and tbl_regression
# @txt_vars: text shown in the forestplot ("characteric is default and cannot be dropped),
#            It can select from the tibble table output columns such as "hr","pvalue" etc)
# @graph_pos: the column place to draw the forest plot, cannot exceed length(txt_vars)+2
# @boxsize:  The forestplot estimate box size
# @title_line_col: The color of the line dividing the header text and the rest
# @...: other forestplot command paramters
#
# @Outputs
# @forestplot.obj: ggplot objective which can draw forestplot
# @forestplot_tb: The table used behind the forestplot.obj
###################################################

tblreg_forest<-function(gtsum_tb,
                          txt_vars=c("ci","pvalue"),
                          graph_pos=2,
                          boxsize=0.3,
                          title_line_col="darkblue",
                          ...){

  #load the necessary packages#
  require(dplyr)
  require(tidyr)
  require(biostatR)
  require(stringr)
  require(forestplot)

  #######################################
  #Output the main text part table ###
  ###################################
  txt_tb1<-gtsum_tb %>%
    modify_column_unhide() %>%
    modify_fmt_fun(contains("stat") ~ style_number) %>%
    as_tibble(col_labels=F) %>%
    mutate(ci=ifelse(!is.na(estimate),paste0(estimate,"(",gsub(", ","-",ci),")"), NA))

  #########################################################
  #Prepare the header of the text part (only one line here)#
  #########################################################
  txt_tb2<-gtsum_tb %>%
    modify_column_unhide() %>%
    as_tibble() %>%
    names()

  ### remove the gt bold sign "**" from the header
  ### And add a summary indicating variable
  txt_tb2<- data.frame(matrix(gsub("\\**","", txt_tb2), nrow=1), stringsAsFactors = F)
  names(txt_tb2)<-names(txt_tb1)

  txt_tb2<- txt_tb2 %>% mutate(ci=paste0(estimate,"(",ci,")"),
                               summary=TRUE)

  txt_tb<- bind_rows(txt_tb2, txt_tb1) %>%
    select(-estimate)

  #######################################################
  ### Combine the forest plot stats with the txt_tb ##
  ######################################################

  line_stats<- gtsum_tb$table_body %>%
    select(estimate, conf.low, conf.high) %>%
    add_row(.before=0)

  ## form the forest plot input matrix ###
  forestplot_tb<-bind_cols(txt_tb, line_stats)


  #######################################################
  ### Create forest plot and save necessary stats ##
  ######################################################
  # N needs to be changed to be changed to stat_n
  txt_vars[toupper(txt_vars)=="N"]<-"stat_n"

  # Event needs to be changed to stat_nevent
  txt_vars[str_detect(toupper(txt_vars),"EVENT")]<-"stat_nevent"

  # HR or OR or CI needs to be changed to ci
  txt_vars[str_detect(toupper(txt_vars),"OR") |
             str_detect(toupper(txt_vars),"HR") |
             str_detect(toupper(txt_vars),"CI")]<-"ci"

  # pv or pvalue or p-value etc to p.value
  txt_vars[str_detect(toupper(txt_vars),"P")]<-"p.value"

  ### Create the labeltext input  ##
  label_txt<-forestplot_tb %>% select(label, one_of(txt_vars))

  forestplot.obj<- forestplot_tb%>%
    forestplot(mean=estimate,
               lower=conf.low,
               upper=conf.high,
               graph.pos=graph_pos,
               zero=1,
               lwd.zero=2,
               boxsize=boxsize,
               labeltext = label_txt,
               graphwidth=unit(9,'cm'),
               is.summary = summary,
               hrzl_lines = list("2"=gpar(lwd=2,col=title_line_col) ),
               ...
    )
  return(list(forestplot.obj=forestplot.obj, forestplot_tb=forestplot_tb) )
}


###########################
## example command ##
#########################

###==============
# Use tbl_uvregression example 1: tbl_uv_ex1
### ==============

#fplot1<-tblreg_forest(gtsum_tb=tbl_uv_ex1)

###==============
# Use tbl_uvregression example 2: tbl_uv_ex2
### ==============

#fplot2<-tblreg_forest(gtsum_tb=tbl_uv_ex2,
#                       txt_vars = c("N","HR","CI","p-value"))


###==============
# Use tbl_regression example 1: tbl_regression_ex1
### ==============

# fplot3<-tblreg_forest(gtsum_tb=tbl_regression_ex1,
#                        txt_vars = c("hr","pv"),
#                        boxsize=0.2,
#                        col=fpColors(box="darkred"))


