###############################################################################
###############################################################################
###############################################################################

## instaluji a loaduji balíčky ------------------------------------------------

invisible(
    lapply(
        c(
            "xtable",
            "openxlsx",
            "foreign",
            "psych",
            "corrgram",
            "ppcor",
            "RColorBrewer"
        ),
        function(my_package){
            
            if(!(my_package %in% rownames(installed.packages()))){
                
                install.packages(
                    my_package,
                    dependencies = TRUE,
                    repos = "http://cran.us.r-project.org"
                )
                
            }
            
            library(my_package, character.only = TRUE)
            
        }
    )
)


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji handling se zipováním v R ----------------------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip") 


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(!"__domaci_ukol_2__.R" %in% dir()){
    setwd(choose.dir())
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím posložky pracovní složky ------------------------------------------

setwd(mother_working_directory)

for(my_subdirectory in c("vstupy", "vystupy")){
    
    if(!file.exists(my_subdirectory)){
        
        dir.create(file.path(
            
            mother_working_directory, my_subdirectory
            
        ))
        
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################

## loaduji data ---------------------------------------------------------------

setwd(
    paste(mother_working_directory, "vstupy", sep = "/")
)

my_data <- read.spss(
    
    file = "du2_30.sav",
    to.data.frame = TRUE
    
)

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## (pre)processing dat --------------------------------------------------------

#### přetypovávám všechny proměnné datasetu na textové ------------------------

for(i in 1:dim(my_data)[2]){
    
    my_data[, i] <- as.character(my_data[, i])
    
}


#### nyní měním hodnotu "Ano" na numerickou 1 a hodnotu "Ne" na numerickou 0 --

for(i in 1:dim(my_data)[2]){
    
    my_data[, i] <- ifelse(
        
        my_data[, i] == "Ano",
        1,
        0
        
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## explorativní analýza dat ---------------------------------------------------

#### počítám korelační matici -------------------------------------------------

my_correlations <- cor(
    
    x = my_data,
    method = "spearman"
    
)


#### ukládám korelogram -------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "korelogram.eps",
    width = 8,
    height = 8,
    pointsize = 14
)

par(mar = c(0.1, 0.1, 0.1, 0.1))

corrgram(
    x = my_correlations,
    order = TRUE,
    lower.panel = panel.pie
)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## vhodnost použití faktorové analýzy -----------------------------------------

#### počítám Kaiser-Meyer-Olkinovu míru ---------------------------------------

KMO(my_data)

# Kaiser-Meyer-Olkin factor adequacy
# Call: KMO(r = my_data)
# Overall MSA =  0.95
# MSA for each item = 
#  auto  hist  horo  hudb  inze  kino  krim  kriz  kuti  medi  nemo  obch  povi
#  0.85  0.96  0.95  0.91  0.97  0.94  0.96  0.95  0.96  0.95  0.96  0.94  0.95
# prace  preh  prir  rodi  spor  tipy  umen  vare  veda  vzde  zaba  zpza  zeny
#  0.96  0.94  0.96  0.96  0.88  0.95  0.96  0.93  0.95  0.96  0.92  0.92  0.91


KMO(my_data)$MSA   # 0.9450468


#### anebo též

my_partial_correlations <- pcor(my_data, method = "spearman")$estimate

sum(
    my_correlations[upper.tri(my_correlations, diag = FALSE)] ^ 2,
    my_correlations[lower.tri(my_correlations, diag = FALSE)] ^ 2
) / sum(
    my_correlations[upper.tri(my_correlations, diag = FALSE)] ^ 2,
    my_correlations[lower.tri(my_correlations, diag = FALSE)] ^ 2,    
    my_partial_correlations[
        upper.tri(my_partial_correlations, diag = FALSE)
    ] ^ 2,
    my_partial_correlations[
        lower.tri(my_partial_correlations, diag = FALSE)
    ] ^ 2
)


#### Bartlettův test sféricity ------------------------------------------------

cortest.bartlett(
    R = my_correlations,
    n = dim(my_data)[1],
    diag = FALSE
)

# $chisq
# [1] 64534.35

# $p.value
# [1] 0

# $df
# [1] 325


## ----------------------------------------------------------------------------

###############################################################################

## dimenzionalita úlohy -------------------------------------------------------

#### elbow fenomén nalézáme u 4. faktoru --------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "scree_plot.eps",
    width = 8,
    height = 5,
    pointsize = 12
)

par(mar = c(4.1, 4.1, 0.5, 0.3))

plot(
    summary(prcomp(my_data))[["importance"]]["Proportion of Variance", ],
    type = "b",
    xlab = "číslo hlavní komponenty (faktoru)",
    ylab = "podíl vysvětlené variability"
)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## faktorová analýza pro čtyři faktory ----------------------------------------

first_factor_analysis <- factanal(
    x = my_data,
    factors = 4,
    rotation = "varimax"
)

my_table <- unclass(first_factor_analysis[["loadings"]])

rownames(my_table) <- attr(my_data, "variable.labels")
colnames(my_table) <- paste("faktor", 1:dim(my_table)[2], sep = " ")

print(
    xtable(
        my_table,
        align = rep("", ncol(my_table) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE,
    format.args = list(decimal.mark = ","))    
)


#### ukládám heatmapu ---------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "heatmap_first_factanal.eps",
    width = 8,
    height = 5,
    pointsize = 12
)

par(mar = c(4.1, 4.1, 0.1, 0.1))

heatmap(
    my_table,
    margins = c(0.2, 4),
    #Rowv = NA,
    #Colv = NA,
    col = brewer.pal(9, "Blues"),
    revC = TRUE
)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## faktorová analýza pro sedm faktorů -----------------------------------------

second_factor_analysis <- factanal(
    x = my_data,
    factors = 6,
    rotation = "varimax"
)

my_table <- unclass(second_factor_analysis[["loadings"]])

rownames(my_table) <- attr(my_data, "variable.labels")
colnames(my_table) <- paste("faktor", 1:dim(my_table)[2], sep = " ")

print(
    xtable(
        my_table,
        align = rep("", ncol(my_table) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE,
    format.args = list(decimal.mark = ",")
)


#### ukládám heatmapu ---------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "heatmap_second_factanal.eps",
    width = 8,
    height = 5,
    pointsize = 12
)

par(mar = c(4.1, 4.1, 0.1, 0.1))

heatmap(
    my_table,
    margins = c(0.2, 4),
    #Rowv = NA,
    #Colv = NA,
    col = brewer.pal(9, "Blues"),
    revC = TRUE
)

dev.off()

setwd(mother_working_directory)


#### komunality ---------------------------------------------------------------

my_communalities <- data.frame(second_factor_analysis$uniqueness)

rownames(my_communalities) <- attr(my_data, "variable.labels")
colnames(my_communalities) <- "komunalita"

my_table <- data.frame(cbind(
    rownames(my_communalities)[
        1:(dim(my_communalities)[1] / 2)
    ],
    my_communalities[
        1:(dim(my_communalities)[1] / 2),
    ],
    rownames(my_communalities)[
        (dim(my_communalities)[1] / 2 + 1):dim(my_communalities)[1]
    ],
    my_communalities[
        (dim(my_communalities)[1] / 2 + 1):dim(my_communalities)[1],
    ]
))

for(i in c(2, 4)){
    
    my_table[, i] <- as.numeric(as.character(my_table[, i]))
    
}

print(
    xtable(
        my_table,
        align = rep("", ncol(my_table) + 1),
        digits = c(0, 0, 3, 0, 3)
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = FALSE,
    include.colnames = FALSE,
    format.args = list(decimal.mark = ",")
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





