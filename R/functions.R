# This saves ggplot in PDF and PNG
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

# This creates scatter plot with regression lines
create_regplot <- function(data, yval, xval, pred_xval, axpos = 2, ...){
  # create a plot
  f <- formula(paste(yval, xval, sep = "~"))
  plot(f, data = data, col = CW, pch = 16, cex = 1.5, axes = FALSE, ...)
  if(!is.na(axpos)) axis(axpos)
  box()
  
  # regression analysis
  f2 <- formula(paste(yval, "~", xval, "* co2 * water"))
  m <- lm(f2, data = data)
  ms <- dredge(m, rank = "AICc") # rank models with AICc
  mb <- get.models(ms, 1)[[1]] # choose the one with samllest AICc
  print(Anova(mb))
  
  # model average between those with dAICc <2 
  if(length(get.models(ms, subset = delta < 2)) > 1) {
    print(summary(model.avg(ms, subset = delta < 2)))
  }
  
  # plot regression lines
  newd <- ddply(data, .(co2, water), 
                function(x) data.frame(xv = seq(min(x[,pred_xval]), max(x[,pred_xval]), 
                                                length.out = 100)))
  names(newd)[3] <- pred_xval
  
  newd <- mutate(newd, 
                 pred_val = predict(mb, newd), 
                 colval   = as.numeric(co2 : water))
  
  f3 <- formula(paste("pred_val ~", xval))
  d_ply(newd, .(co2, water), 
        function(x) lines(f3, col = alpha(x$colval, .6), lwd = 2, data = x))
  }


# this creates boxplot for given response variable agains CO2 x Water
create_boxplot <- function(data, yval, ...){
  f <- formula(paste(yval, "~ CW"))
  boxplot(f, data = data, border = 1:4, axes = FALSE, ...)
  box()
}


# create plots for OA drivers 
create_fig_OAdriver <- function(data, showlayout = TRUE, 
                                xval1, xval2, xval3,
                                pred_xval1, pred_xval2, pred_xval3,
                                xlim1 = NULL, xlim2 = NULL, xlim3 = NULL,
                                xlab1, xlab2, xlab3,
                                yval1, ylim1 = NULL, ylab1){
  
  # define xy limit
  xy_lim <- llply(c(xval1, xval2, xval3, yval1), 
                  function(x) with(data, range(eval(parse(text = x)))))
  
  if(is.null(xlim1)) xlim1 <- xy_lim[[1]]
  if(is.null(xlim2)) xlim2 <- xy_lim[[2]]
  if(is.null(xlim3)) xlim3 <- xy_lim[[3]]
  if(is.null(ylim1)) ylim1 <- xy_lim[[4]]
  
  # plot layout
  nf <- layout(matrix(1:8, byrow = TRUE, 2, 4), c(4, 3, 3, 1.5), c(1, 4), respect = FALSE)
  if(showlayout) layout.show(nf)
  
  # define margins
  mar_box1 <- c(0, 5, 1, 0)
  mar_box2 <- c(0, 1, 1, 0)
  mar_reg1 <- c(5, 5, 1, 0)
  mar_reg2 <- c(5, 1, 1, 0)
  
  # boplot with xvals
  m_ply(cbind(yval = c(xval1, xval2, xval3),
              ylim = paste0("xlim", 1:3),
              mar_val = c("mar_box1", rep("mar_box2", 2))),
        function(..., ylim, mar_val) {
          par(mar = get(mar_val))
          create_boxplot(..., data = data, xlab = "", ylab = "", 
                         ylim = get(ylim), horizontal = TRUE)
        }
  )
  
  # blank plot with legend
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("left", legend = levels(data$CW), col = 1:4, pch = 19, bty = "n")
  
  # regression plots
  nlab <- "" # empty label
  
  m_reg <-cbind(xval      = c(xval1, xval2, xval3), 
                pred_xval = c(pred_xval1, pred_xval2, pred_xval3),
                xlim      = paste0("xlim", 1:3),
                xlab      = paste0("xlab", 1:3),
                mar_val   = c("mar_reg1", rep("mar_reg2", 2)),
                ylab      = c("ylab1", "nlab", "nlab"),
                yaxis     = c(TRUE, FALSE, FALSE)) # matrix for regressino plots
  
  m_ply(m_reg,
        function(..., xlim, xlab, yaxis, mar_val, ylab){
          par(mar = get(mar_val))
          create_regplot(..., data = data, 
                         yval = yval1, ylim = ylim1, ylab = get(ylab),
                         xlim = get(xlim), xlab = get(xlab), axpos = 1)
          if(yaxis) axis(2)
        }
  )

  # boxplot for yval1
  par(mar = c(5, 1, 1, 1))
  create_boxplot(data = data, yval = yval1, xlab = "", ylab = "", ylim = ylim1)
  
}


# create plot for OA-driven factors 
create_fig_byOA <- function(data, xval1, xlab1, pred_xval1, 
                            yval1, yval2, yval3, yval4,
                            ylab1, ylab2, ylab3, ylab4,
                            xlim1 = NULL, ylim1 = NULL, ylim2 = NULL, 
                            ylim3 = NULL, ylim4 = NULL, 
                            showlayout = TRUE){
  
  # define xy limit
  xy_lim <- llply(c(xval1, yval1, yval2, yval3, yval4), 
                  function(x) with(data, range(eval(parse(text = x)))))
  
  if(is.null(xlim1)) xlim1 <- xy_lim[[1]]
  if(is.null(ylim1)) ylim1 <- xy_lim[[2]]
  if(is.null(ylim2)) ylim2 <- xy_lim[[3]]
  if(is.null(ylim3)) ylim3 <- xy_lim[[4]]
  if(is.null(ylim4)) ylim4 <- xy_lim[[5]]
  
  # plot layout
  nf <- layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 5, 2), 
               c(4, 1), c(2, 3, 3, 3, 3.5), respect = FALSE)
  if(showlayout) layout.show(nf)
  
  # OA vs co2 x water
  par(mar = c(1, 6, 1, 1))
  create_boxplot(data = data, yval = xval1, ylim = xlim1, horizontal = TRUE)
  
  # regression plot with yvals 
  par(mar = c(1, 6, 0, 1))
  create_regplot(data = data, yval = yval1, ylim = ylim1, ylab = ylab1, 
                 xval = xval1, xlim = xlim1,  pred_xval = pred_xval1, xlab = "")
  create_regplot(data = data, yval = yval2, ylim = ylim2, ylab = ylab2, 
                 xval = xval1, xlim = xlim1,  pred_xval = pred_xval1, xlab = "")
  create_regplot(data = data, yval = yval3, ylim = ylim3, ylab = ylab3, 
                 xval = xval1, xlim = xlim1,  pred_xval = pred_xval1, xlab = "")
  par(mar = c(4, 6, 0, 1))
  create_regplot(data = data, yval = yval4, ylim = ylim4, ylab = ylab4, 
                 xval = xval1, xlim = xlim1,  pred_xval = pred_xval1, 
                 xlab = xlab1)
  axis(1)
  
  # blank plot with legend
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("left", legend = levels(data$CW), col = 1:4, pch = 19, bty = "n")
  
  # boxplot for yvals
  par(mar = c(1, 0, 0, 1))
  create_boxplot(data = data, yval = yval1, ylim = ylim1)
  create_boxplot(data = data, yval = yval2, ylim = ylim2)
  create_boxplot(data = data, yval = yval3, ylim = ylim3)
  par(mar = c(4, 0, 0, 1))
  create_boxplot(data = data, yval = yval4, ylim = ylim4)
}


# save png file with 600 dpi 

save_png600 <- function(...) png(..., res = 600, units = "in")


# create multi-plots for OA-driven factors 

create_fig_byOA_wide <- function(data, showlayout = TRUE, fig_title,
                                 xval1, xval2, xval3, xval4,
                                 xlab1, xlab2, xlab3, xlab4,
                                 pred_xval1, pred_xval2, pred_xval3, pred_xval4, 
                                 yval1, yval2, yval3, yval4,
                                 ylab1, ylab2, ylab3, ylab4,
                                 xlim1 = NULL, xlim2 = NULL, xlim3 = NULL,
                                 xlim4 = NULL,
                                 ylim1 = NULL, ylim2 = NULL, ylim3 = NULL, 
                                 ylim4 = NULL){
  
  
  # . define xy limit ====
  xy_lim <- llply(c(xval1, xval2, xval3, xval4, yval1, yval2, yval3, yval4), 
                  function(x) with(data, range(eval(parse(text = x)))))
  
  if(is.null(xlim1)) xlim1 <- xy_lim[[1]]
  if(is.null(xlim2)) xlim2 <- xy_lim[[2]]
  if(is.null(xlim3)) xlim3 <- xy_lim[[3]]
  if(is.null(xlim4)) xlim4 <- xy_lim[[4]]
  if(is.null(ylim1)) ylim1 <- xy_lim[[5]]
  if(is.null(ylim2)) ylim2 <- xy_lim[[6]]
  if(is.null(ylim3)) ylim3 <- xy_lim[[7]]
  if(is.null(ylim4)) ylim4 <- xy_lim[[8]]
  
  # matrix for y parameters
  ymat <- cbind(yval = c(yval1, yval2, yval3), 
                ylim = c("ylim1", "ylim2", "ylim3"))
  # This matrix will be passed to mlply, so don't use data.frame but cbind to 
  # keep character vectors. 
  
  # ylim cannot be directly passed as it is a vector with two elements (i.e.
  # min and max values). the values in ylim will be called using get().
  
  # . plot layour ====
  nf <- layout(matrix(c(26, 26, 26, 26, 26,
                         1,  2,  3,  4,  5,
                         6, 10, 14, 18, 22,
                         7, 11, 15, 19, 23,
                         8, 12, 16, 20, 24,
                         9, 13, 17, 21, 25),
                      byrow = TRUE, 6, 5), 
               widths  = c(6.5, 4, 4, 4, 2.5), 
               heights = c(.4, 1.5, 2.5, 2.5, 2.5, 3.5), 
               respect = FALSE)
  if(showlayout) layout.show(nf)
  
  # . graphic margin ====
  par_r1c1   <- c(1, 6, 1, 1)
  par_r1c2   <- c(1, 0, 1, 1)
  par_r24c1  <- c(1, 6, 0, 1)
  par_r5c1   <- c(4, 6, 0, 1)
  par_r24c25 <- c(1, 0, 0, 1)
  par_r5c25  <- c(4, 0, 0, 1)
  
  # . boxplot in the 1st row ====
  m_R1 <- cbind(yval    = c(xval1, xval2, xval3, xval4),
                ylim    = c("xlim1", "xlim2", "xlim3", "xlim4"),
                par_val = c("par_r1c1", rep("par_r1c2", 3)))
  m_ply(m_R1, function(..., ylim, par_val) {
    par(mar = get(par_val))
    create_boxplot(..., data = data, ylim = get(ylim), horizontal = TRUE)
  })
  
  # . blank plot with legend ====
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("left", legend = levels(data$CW), col = 1:4, pch = 19, bty = "n")
  
  # . regression plots ====
  nlab <- "" # no label
  
  m_R25C14 <- cbind(xval      = rep(c(xval1, xval2, xval3, xval4), each = 4), 
                    xlab      = c(rep("nlab", 3), "xlab1", rep("nlab", 3), "xlab2", 
                                  rep("nlab", 3), "xlab3", rep("nlab", 3), "xlab4"),
                    xlim      = rep(paste0("xlim", 1:4), each = 4),
                    pred_xval = rep(c(pred_xval1, pred_xval2, pred_xval3, pred_xval4), 
                                    each = 4),
                    yval      = rep(c(yval1, yval2, yval3, yval4), 4), 
                    ylab      = c("ylab1", "ylab2", "ylab3", "ylab4", rep("nlab", 12)),
                    ylim      = rep(paste0("ylim", 1:4), 4),
                    par_val   = c(rep("par_r24c1", 3), "par_r5c1", 
                                  rep(c(rep("par_r24c25", 3), "par_r5c25"), 3)),
                    axpos     = c(rep(2, 4), rep(NA, 12)),
                    xaxis     = rep(c(rep(FALSE, 3), TRUE), 4))
  
  m_ply(m_R25C14, function(..., xlim, ylim, xlab, ylab, par_val, xaxis){
    par(mar = get(par_val))
    create_regplot(..., data = data,
                   xlim = get(xlim), ylim = get(ylim), 
                   xlab = get(xlab), ylab = get(ylab))
    if(xaxis) axis(1)
  })
  
  # . boxplot for yvals ====
  ymat <- cbind(yval    = c(yval1, yval2, yval3, yval4),
                ylim    = paste0("ylim", 1:4), 
                par_val = c(rep("par_r24c25", 3), "par_r5c25"))
  m_ply(ymat, function(ylim, par_val, ...){
    par(mar = get(par_val))
    create_boxplot(data = data, ylim = get(ylim), ...)
  })
  
  # . fig title ====
  par(mar = c(0, 0, 0, 0))
  plot.new()
  text(x = .5, y = .3, labels = fig_title, cex = 1.2)
}

create_fig_OAdriver_wid <- function(data, showlayout = TRUE, fig_title, 
                                    xval1, xval2, xval3,
                                    pred_xval1, pred_xval2, pred_xval3,
                                    xlim1 = NULL, xlim2 = NULL, xlim3 = NULL,
                                    xlab1, xlab2, xlab3,
                                    yval1, yval2, yval3, yval4,
                                    ylim1 = NULL, ylim2 = NULL, ylim3 = NULL, 
                                    ylim4 = NULL,
                                    ylab1, ylab2, ylab3, ylab4){
  
  # . define xy limit ====
  xy_lim <- llply(c(xval1, xval2, xval3, yval1, yval2, yval3, yval4), 
                  function(x) with(data, range(eval(parse(text = x)))))
  
  if(is.null(xlim1)) xlim1 <- xy_lim[[1]]
  if(is.null(xlim2)) xlim2 <- xy_lim[[2]]
  if(is.null(xlim3)) xlim3 <- xy_lim[[3]]
  if(is.null(ylim1)) ylim1 <- xy_lim[[4]]
  if(is.null(ylim2)) ylim2 <- xy_lim[[5]]
  if(is.null(ylim3)) ylim3 <- xy_lim[[6]]
  if(is.null(ylim4)) ylim4 <- xy_lim[[7]]
  
  # . plot layout ====
  nf <- layout(matrix(c(21, 21, 21, 21,
                         1,  2,  3,  4,
                         5,  9, 13,  17,
                         6, 10, 14,  18,
                         7, 11, 15,  19,
                         8, 12, 16,  20),
                      byrow = TRUE, 6, 4), 
               widths  = c(4, 3, 3, 1.5), 
               heights = c(.6, 2, 4, 4, 4, 5.5), respect = FALSE)
  if(showlayout) layout.show(nf)
  
  # . fig margin ====
  mar_r1c1   <- c(1, 5, 1, 1) # plot 1
  mar_r24c1  <- c(1, 5, 0, 1) # plot 5, 6, 7
  mar_r5c1   <- c(5, 5, 0, 1) # plot 8 
  mar_r1c23  <- c(1, 0, 1, 1) # plot 2, 3
  mar_r24c24 <- c(1, 0, 0, 1) # plot 9-11, 13-15, 17-19
  mar_r5c25  <- c(5, 0, 0, 1) # plot 12, 16, 20
  
  # . boplots on the 1st row ====
  m_R1 <- cbind(yval    = c(xval1, xval2, xval3),
                ylim    = paste0("xlim", 1:3), 
                mar_val = c("mar_r1c1", rep("mar_r1c23", 2))) # margin
  
  m_ply(m_R1, function(..., ylim, mar_val){
    par(mar = get(mar_val))
    create_boxplot(..., data = data, xlab = "", ylab = "", horizontal = TRUE,
                   ylim = get(ylim))
  })
  
  # . blank plot with legend ====
  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("left", legend = levels(data$CW), col = 1:4, pch = 19, bty = "n")
  
  # . define matrix for regression plots ====
  nlab <- "" # empty label
  
  m_reg <- cbind(xval      = rep(c(xval1, xval2, xval3), each = 4),
                 xlab      = c(rep("nlab", 3), "xlab1", rep("nlab", 3), "xlab2", 
                               rep("nlab", 3), "xlab3"),
                 pred_xval = rep(c(pred_xval1, pred_xval2, pred_xval3), each = 4),
                 xlim      = rep(paste0("xlim", 1:3), each = 4),
                 
                 yval      = rep(c(yval1, yval2, yval3, yval4), 3),
                 ylab      = c(paste0("ylab", 1:4), rep("nlab", 8)),
                 ylim      = rep(paste0("ylim", 1:4), 3),
                 
                 mar_val   = c(rep("mar_r24c1" , 3), "mar_r5c1", 
                               rep(c(rep("mar_r24c24", 3), "mar_r5c25"), 2)),
                 axpos     = c(rep(2, 4), rep(NA, 8)),
                 xaxis     = rep(c(rep(FALSE, 3), TRUE), 3))
  
  # . regression plots ====
  m_ply(m_reg, function(..., xlab, xlim, ylab, ylim, mar_val, xaxis){
    par(mar = get(mar_val))
    create_regplot(..., data = data, 
                   xlim = get(xlim), ylim = get(ylim), 
                   xlab = get(xlab), ylab = get(ylab))
    if(xaxis) axis(1)
  })
  
  # . boxplots ====
  m_ply(cbind(yval    = c(yval1, yval2, yval3, yval4),
              ylim    = paste0("ylim", 1:4),
              mar_val = c(rep("mar_r24c24", 3), "mar_r5c25")), 
        function(..., ylim, mar_val) {
          par(mar = get(mar_val))
          create_boxplot(..., data = data, xlab = "", ylab = "", ylim = get(ylim))
        })
  
  # . fig title ====
  par(mar = c(0, 0, 0, 0))
  plot.new()
  text(x = .5, y = .3, labels = fig_title, cex = 1.2)
}
