## -- Riverplot for BRAIN-ICU followup out to six years --------------------------------------------
if(Sys.info()['sysname'] == 'Darwin'){
  load('/Volumes/thomps23/ICUDelirium/BRAINICU/braindata.Rdata')
} else{
  load('/home/thomps23/ICUDelirium/BRAINICU/braindata.Rdata')  
}

library(tidyverse)
library(googleVis)

## -- Create data set with node values for each patient at each time point -------------------------
# ## Contains patients who remained alive and in study at 3m
# keepPts <- subset(brain.fu,
#                   fu.period == '3 Month' &
#                     status %in% c('Living', 'Living-Active in the study', 'Visit Not Completed'))$id

## Contains patients who did not withdraw in the hospital
keepPts <- subset(brain.oneobs, is.na(studywd.amt))$id

## -- Use brain.oneobs and brain.fu to determine whether, at each time point, patient was... -------
## - deceased
## - unknown (followup only) - no test score, withdrawn, lost to followup
## - alive, no known impairment (IQCODE < 3.6 @ baseline for discharge, RBANS > 78 at followup)
## - alive, with impairment (IQCODE >= 3.6 @ baseline for discharge, RBANS <= 78 at followup)

## Create dummy data set with one record per patient per time point (inc. 12m mortality, since
## death may be entered there)
shell_df <- expand.grid(keepPts, 1:6)
names(shell_df) <- c('id', 'timept')

## Add timept variables to brain.oneobs, brain.fu
brain.oneobs$timept <- 1
brain.fu$timept <- with(brain.fu, ifelse(fu.period == '3 Month', 2,
                                  ifelse(fu.period == '12 Month', 3,
                                  ifelse(fu.period == '12 Month Mortality', 4,
                                  ifelse(fu.period == '4 year', 5, 6)))))

## Combine data needed to determine status at each time point
shell_df <- shell_df %>%
  left_join(dplyr::select(brain.oneobs, id, timept, died.inhosp, studywd, iqcode.score.e),
            by = c('id', 'timept')) %>%
  left_join(dplyr::select(brain.fu, id, timept, status, rbans.global.score),
            by = c('id', 'timept')) %>%
  ## Create indicators for whether patient died, withdrew at a given time point; these are absorbing
  ## states
  mutate(died_now = ifelse(timept == 1, died.inhosp == 'Died in hospital', status == 'Deceased'),
         wd_now = ifelse(timept == 1, studywd == 'Yes', status == 'Living-Withdrew from the study'))

## Figure out first time point of death, withdrawal for each patient
dfFirstDied <- shell_df %>%
  filter(died_now) %>%
  arrange(id, timept) %>%
  group_by(id) %>%
  summarise(firstDied = head(timept, n = 1))

dfFirstWD <- shell_df %>%
  filter(wd_now) %>%
  arrange(id, timept) %>%
  group_by(id) %>%
  summarise(firstWD = head(timept, n = 1))

## Merge death, withdrawal time points back onto shell data; determine current status at each time
brainStatus <- reduce(list(shell_df, dfFirstDied, dfFirstWD), .f = left_join, by = 'id') %>%
  ## 12m mortality "time point" only means anything if patient died; we've already got that info
  filter(timept != 4) %>%
  mutate(generalStatus = factor(ifelse(!is.na(firstDied) & timept >= firstDied, 6,
                                ifelse(!is.na(firstWD) & timept >= firstWD, 5,
                                ifelse(timept == 1 & iqcode.score.e < 3.6, 1,
                                ifelse(timept == 1, 2,
                                ifelse(is.na(rbans.global.score), 5,
                                ifelse(rbans.global.score <= 78, 4, 3)))))),
                                levels = 1:6,
                                labels = c('No baseline impairment',
                                           'Mild CI at baseline',
                                           'Normal cognition',
                                           'Cognitive impairment',
                                           'Unknown',
                                           'Died')),
         timelabel = ifelse(timept == 1 & generalStatus != 'Died', '',
                     ifelse(timept == 1, 'in hospital',
                     ifelse(timept == 2, '3m',
                     ifelse(timept == 3, '12m',
                     ifelse(timept == 5, '4yr', '6yr'))))),
         sourceNode = ifelse(timept == 1 & generalStatus != 'Died', as.character(generalStatus),
                      ifelse(timept == 1, paste(as.character(generalStatus), timelabel),
                             paste(as.character(generalStatus), timelabel, sep = ', ')))) %>%
  arrange(id, timept)

## -- Use googleVis to create Sankey plot ----------------------------------------------------------
## Calculate number of patients in each node/edge
sankeyData <- brainStatus %>%
  group_by(id) %>%
  mutate(targetNode = lead(sourceNode)) %>%
  ungroup() %>%
  filter(!is.na(targetNode)) %>%
  group_by(sourceNode, targetNode) %>%
  summarise(weight = n()) %>%
  ## Try arranging in the order I want to see if that works...
  separate(sourceNode, into = c('sourceStatus', 'sourceTime'),
           sep = ', ', remove = FALSE, fill = 'right') %>%
  separate(targetNode, into = c('targetStatus', 'targetTime'),
           sep = ', ', remove = FALSE, fill = 'right') %>%
  mutate(sourceStatusSort = ifelse(sourceStatus %in% c('Normal cognition', 'No baseline impairment'),
                                   1,
                            ifelse(sourceStatus %in% c('Cognitive impairment', 'Mild CI at baseline'),
                                   2,
                            ifelse(sourceStatus %in% c('Died', 'Died in hospital'), 4, 3))),
         sourceTimeSort = ifelse(is.na(sourceTime), 1,
                          ifelse(sourceTime == '3m', 2,
                          ifelse(sourceTime == '12m', 3,
                          ifelse(sourceTime == '4yr', 4, 5)))),
         targetStatusSort = ifelse(targetStatus %in% c('Normal cognition', 'No baseline impairment'),
                                   1,
                            ifelse(targetStatus %in% c('Cognitive impairment', 'Mild CI at baseline'),
                                   2,
                            ifelse(targetStatus %in% c('Died', 'Died in hospital'), 4, 3))),
         targetTimeSort = ifelse(is.na(targetTime), 1,
                          ifelse(targetTime == '3m', 2,
                          ifelse(targetTime == '12m', 3,
                          ifelse(targetTime == '4yr', 4, 5))))) %>%
  arrange(sourceTimeSort, sourceStatusSort, targetTimeSort, targetStatusSort) %>%
  dplyr::select(sourceNode, targetNode, weight, sourceTimeSort:targetStatusSort) %>%
  ungroup()

## Get total Ns for each sourceNode
sourceNs <- sankeyData %>%
  group_by(sourceNode) %>%
  summarise(sourceN = sum(weight))

## Add Ns onto main sankeyData, create text for tooltip (transition, N / denom, %)
sankeyData <- sankeyData %>%
  left_join(sourceNs, by = 'sourceNode') %>%
  mutate(info.tooltip = paste0('<b>', sourceNode, ' -> ', targetNode, ':</b><br>',
                               weight, '/', sourceN, ' (', round((weight / sourceN)*100), '%)'))

## Create googleVis object
gvisBRAIN <- gvisSankey(sankeyData,
                        from = 'sourceNode',
                        to = 'targetNode',
                        weight = 'weight',
                        options = list(height = 600, width = 1000, tooltip = "{isHtml:'True'}",
                                       sankey = "{link: { colorMode: 'gradient' },
                                                  node: { colors: ['#3D7900', '#3D7900', '#AC0001',
                                                                   '#0057AC', 'black', '#AC0001',
                                                                   'black', '#3D7900', '#AC0001',
                                                                   '#0057AC', 'black', '#3D7900',
                                                                   '#AC0001', '#0057AC', 'black',
                                                                   '#3D7900', '#AC0001', '#0057AC',
                                                                   'black'],
                                                          label: { color: '#003D79',
                                                                   fontSize: 14,
                                                                   bold: true } },
                                                  iterations: 0
                                                 }"))

## 0057AC: blue (unknown)
## 3D7900: green (normal)
## AC0001: red (impaired)
## black: died

plot(gvisBRAIN)
print(gvisBRAIN, file = 'brain_phase3_sankey.html')

# ## -- DIDN'T WORK: riverplot looked OK, but things are hard coded and it didn't look awesome -------
# ## -- Trying to edit functions myself resulted in much frustration ---------------------------------
# ## -- Edit riverplot() to allow more flexibility ---------------------------------------------------
# library(riverplot)
# ## Source functions that weren't exported
# source('riverplot_functions.R')
#
# ## Main function. Changes:
# ## - Add flexibility for margins
# riverplotJT <- function (x, lty = 0, srt = NULL, default_style = NULL, gravity = "top",
#                          node_margin = 0.1, nodewidth = 1.5, plot_area = 0.5, nsteps = 50,
#                          add_mid_points = NULL, yscale = "auto", margins = par()$mar)
# {
#   default_style <- getstyle(NULL, default_style)
#   plot.new()
#   dmsgc("--------------\nDefault style:\n-----------\n")
#   dmsg(default_style)
#   dmsgc("--------------\n")
#   x2 <- x
#   x2$nodes$ID <- as.character(x2$nodes$ID)
#   dmsg("checking edges")
#   # x2$edges <- checkedges(x2$edges, names(x2))
#   x2 <- orderConnections(x2)
#   if (is.null(add_mid_points)) {
#     if (ypos_present(x2))
#       add_mid_points <- FALSE
#     else add_mid_points <- TRUE
#   }
#   dmsg("adding mid points")
#   if (add_mid_points)
#     x2 <- add_mid_points(x2)
#   for (n in c(x2$nodes$ID, x2$edges$ID)) {
#     x2$styles[[n]] <- getstyle(x2$styles[[n]], default_style,
#                                update.missing = FALSE)
#   }
#   dmsgc("Updated styles:\n")
#   dmsg(x2$styles)
#   dmsgc("--------------\n")
#   if (yscale == "auto")
#     yscale <- autoscale(x2)
#   if (yscale != 1)
#     x2$edges$Value <- x2$edges$Value * yscale
#   dmsg("calculating sizes")
#   sizes <- calcsizes2(x2)
#   dmsg(sizes)
#   dmsg("calculating positions")
#   positions <- calcpos(x2, sizes, gravity = gravity, node_margin = node_margin)
#   dmsg("done")
#   xrange <- range(x2$nodes$x)
#   xlim <- xrange + (xrange[2] - xrange[1]) * c(-0.1, 0.1)
#   ylim <- range(positions[c("bottom", "top"), ])
#   b <- (ylim[2] - ylim[1]) * (1 - plot_area)/plot_area/2
#   ylim <- ylim + c(-b, b)
#   par(mar = margins)
#   par(usr = c(xlim, ylim))
#   l <- names(x2$nodes)[order(x2$nodes)]
#   dev.hold()
#   on.exit(dev.flush())
#   w <- strwidth("hjKg") * nodewidth/2
#   dmsg("drawing edges")
#   draw.edges(x2, positions, sizes, lty = lty, nsteps = nsteps,
#              boxw = w)
#   dmsg("drawing nodes")
#   draw.nodes(x2, positions, sizes, srt = srt, boxw = w, lty = lty)
#   return(invisible(positions))
# }

# ## Create $nodes element
# riverNodes <- data.frame(ID = sort(unique(brainStatus$currentNode))) %>%
#   separate(ID, into = c("labels", "x"), sep = '_', remove = FALSE) %>%
#   mutate(x = as.numeric(x),
#          y = ifelse(labels == 'Normal cognition', 1,
#                     ifelse(labels == 'Cognitive impairment', 2,
#                            ifelse(labels == 'Died', 3, 4))),
#          labels = ifelse(x == 1 | labels == 'Died' & x == 2, labels, '')) %>%
#   dplyr::select(ID, x, y, labels)
#
# ## Calculate Ns in each transition
# riverEdges <- brainStatus %>%
#   dplyr::select(id, currentNode) %>%
#   group_by(id) %>%
#   mutate(nextNode = lead(currentNode)) %>%
#   ungroup() %>%
#   dplyr::select(-id) %>%
#   filter(!is.na(nextNode)) %>%
#   group_by(currentNode, nextNode) %>%
#   summarise(Value = n())
# names(riverEdges) <- c('N1', 'N2', 'Value')
#
# palette <- paste0(c("#8F8F8F",  ## Unknown
#                     "#2D0000",  ## Died
#                     "#790001",  ## Impaired
#                     "#003D79"), ## Not impaired
#                   "95") ## % transparency
# styles <- lapply(riverNodes$y, function(n) {
#   if(n == 2){
#     textColor = '#D4D4D4'
#   } else{
#     textColor = 'black'
#   }
#   list(col = palette[n], lty = 'blank', textcol = textColor, srt = "0")
# })
# names(styles) = riverNodes$ID
#
# riverObj <- makeRiver(nodes = as.data.frame(riverNodes), edges = as.data.frame(riverEdges),
#                       node_styles = styles)

# pdf(file = 'brain_phase3_riverplot.pdf', width = 10, height = 7)
# par(adj = 0.7, cex = 0.75)
# riverplot(riverObj, plot_area = 0.90)
# dev.off()

# ## Try sankey package
# library(sankey)
# sankeyNodes <- as.data.frame(riverNodes)
# names(sankeyNodes) <- c('id', 'x', 'y', 'label')
# sankeyNodes$id <- as.character(sankeyNodes$id)
# sankeyNodes$col <- c(rep('black', 3), rep('red', 4), rep('green', 4), rep('gray', 4))
#
# sankeyEdges <- as.data.frame(riverEdges)
# names(sankeyEdges) <- c('N1', 'N2', 'weight')
# sankeyEdges$colorstyle <- 'gradient'
#
# junk <- make_sankey(nodes = sankeyNodes,
#                     edges = sankeyEdges)
#
# ## Can't get this one to not cut off the edges? What's up?
# pdf(file = 'brain_phase3_sankeyplot.pdf', width = 10, height = 7)
# sankey(junk)
# dev.off()
#
