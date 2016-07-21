rm(list = ls(all = TRUE))

source("R/packages.R")
source("R/functions.R")


# load data ---------------------------------------------------------------

# treatment
  treat_dd   <- read.csv("Data/Treatment.csv") 

# root exudate
  load("Data/exudate_raw_dd_cst.RData") 
  # remove colmuns with all 0
  exudate_raw_dd_cst <- exudate_raw_dd_cst[, !apply(exudate_raw_dd_cst, 2, function(x) all(x == 0))]

# leaf chemistry
  load("Data/leaf_chem_dd.RData")       
  leaf_chem_dd <- leaf_chem_dd %>%
                    select(sample_id, C, N, P, Mn) %>%
                      mutate(NP_ratio = N/P,
                             CN_ratio = C/N,
                             CP_ratio = C/P)
  names(leaf_chem_dd)[-1] <- paste0("leaf_", names(leaf_chem_dd)[-1])

# soil chemistry      
  load("Data/soilnut_dd.RData")         
  head(soilnut_dd)
  soilnut_dd <- mutate(soilnut_dd, 
                       N        = ammonium + nitrate,
                       NP_ratio = N/phosphate)
  names(soilnut_dd)[-1] <- paste0("Soil_", names(soilnut_dd)[-1])

# root morphology
  load("Data/root_CumSum_dd_cst.RData") 

# plant biomass
  biomass_dd <- read.csv("Data/plant_biomass.csv") 
  biomass_dd$above_biomass <- rowSums(biomass_dd[, c("leaf","stem", "seed")], na.rm = TRUE) 
  biomass_dd$total_biomass <- rowSums(biomass_dd[, -1], na.rm = TRUE) 
  biomass_dd <- select(biomass_dd, -seed, -stem)

# Phosphatase activity
  load("Data/phosphtase_dd.RData")
  
# organic acids, plant biomass and root -----------------------------------

  OA_biomass_dd <-Reduce(function(...) merge(..., by = "sample_id"), 
                         list(treat_dd, biomass_dd, root_CumSum_dd_cst, exudate_raw_dd_cst)) 
  
  # melt by OAs
  OA_biomass_dd_mlt <- melt(OA_biomass_dd, 
                            id.vars = c(names(treat_dd), names(biomass_dd)[-1], 
                                        names(root_CumSum_dd_cst)[-1]))
  OA_biomass_dd_mlt <- rename(OA_biomass_dd_mlt,
                              OA_variable = variable, 
                              OA_value    = value)
  # melt by root diameter classes
  OA_biomass_root_dd_mlt <- melt(OA_biomass_dd_mlt, 
                                 id.vars = c(names(treat_dd), "OA_variable", "OA_value"))
  OA_biomass_root_dd_mlt <- rename(OA_biomass_root_dd_mlt, 
                                   BM_variable = variable,
                                   BM_value    = value)

  # compute R2 when OA is fitted against ranges of diamter class root
  # length/surface area and biomass for each species
  
  rd <- ddply(OA_biomass_root_dd_mlt, .(species, OA_variable, BM_variable), 
              function(x){
                if(all(x$OA_value == 0)) {
                  d <- NULL
                } else {
                m <- lm(OA_value ~ BM_value * co2 * water, data = x)
                m <- step(m, trace = FALSE, test = "F")
                d <- data.frame(r2 = summary(m)$r.squared)
                print(Anova(m))
                }
                return(d)
                })
  
  # remove na rows
  rd <- rd[complete.cases(rd), ]
  ddply(rd, .(species, OA_variable), function(x) data.frame(unique(x$BM_variable[which(x$r2 == max(x$r2))])))
  filter(rd, species == "Hakea" & OA_variable == "cis-aconitic_P" & BM_variable == "SA_DC15")
  mmm <- lm(r2 ~ species, data = rd)
  ddd <- Anova(mmm)
  data.frame(ddd)
  str(ddd)
  head(rd)

# create figs -------------------------------------------------------------
  
  # sepate biomass and root length and surface area
  rd$type <- factor(ifelse(grepl("^L_", as.character(rd$BM_variable)), "length", 
                           ifelse(grepl("^SA_", as.character(rd$BM_variable)),
                                  "surface_area", "biomass")))
  # add diameter class
  rd$DC <- factor(gsub("L_DC|SA_DC", "", as.character(rd$BM_variable)), 
                  levels = c(1:15, "root", "leaf", "above_biomass", "total_biomass"))
  
  # create fig and save
  d_ply(rd, .(species), function(x){
    p <- ggplot(x, aes(x = DC, y = r2, col = type))
    p2 <- p + 
      geom_point(shape = 19, alpha = .8, size = 2) +
      facet_wrap( ~ OA_variable, ncol = 3) +
      ggtitle(unique(x$species)) +
      theme_bw() +
      theme(axis.text.x  = element_text(angle = 90, hjust = 1, size =  8),
            legend.position = "top") +
      labs(x = "Diamter class", y = expression(R^2))
    figname <- paste0("Output/Figs/R2_OA_vs_Biomass_Root_", unique(x$species))
    ggsavePP(p2, filename = figname, width = 6.5, height = 7)
    })
  
  
  