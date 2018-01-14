###############################################################################
###############################################################################
###############################################################################

## instaluji a loaduji balíčky ------------------------------------------------

invisible(
    lapply(c(
            "xtable",
            "openxlsx",
            "foreign",
            "MVN"
        ),
        function(package){
            
            if(!(package %in% rownames(installed.packages()))){
                
                install.packages(
                    package,
                    dependencies = TRUE,
                    repos = "http://cran.us.r-project.org"
                )
                
            }
            
            library(package, character.only = TRUE)
            
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

while(!"__domaci_ukol_1__.R" %in% dir()){
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

my_data <- data.frame(
    
    setNames(
        object = read.spss(    
            file = "du1_30.sav",
            to.data.frame = TRUE
        ),
        nm = c(
            "id",
            "obvod hrudníku",
            "obvod břicha",
            "obvod stehen",
            "obvod předloktí",
            "obvod kolene",
            "obvod kotníku"
        )
    ),
    check.names = FALSE
    
)

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## Exploratory Data Analysis --------------------------------------------------

#### nejdříve vytvářím diagram závislostí jednotlivých proměnných mezi
#### sebou --------------------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "scatterplot_matrix.eps",
    width = 8,
    height = 8,
    pointsize = 14
)

par(mar = c(1.1, 1.1, 0.1, 0.1))


pairs(
    my_data[, grepl("obvod", colnames(my_data))],
    panel = "panel.smooth"
)

dev.off()

setwd(mother_working_directory)


#### --------------------------------------------------------------------------

#### nyní vytvářím pro každou spojitou proměnnou boxplot, histogram
#### a QQ-plot ----------------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

for(my_variable in colnames(my_data)[grepl("obvod", colnames(my_data))]){
    
    #### boxploty -------------------------------------------------------------
    
    cairo_ps(
        file = paste(
            gsub(
                " ",
                "_",
                iconv(my_variable, to = "ASCII//TRANSLIT")
            ),
            "_boxplot.eps",
            sep = ""
        ),
        width = 5,
        height = 5,
        pointsize = 18
    )
    
    par(mar = c(4.1, 4.1, 0.5, 0.3))
    
    boxplot(
        x = my_data[, my_variable],
        col = "lightgrey",
        xlab = "soubor žen",
        ylab = paste(
            my_variable,
            " [cm]",
            sep = ""
        )
    )
    
    dev.off()
    
    
    #### QQ-ploty -------------------------------------------------------------
    
    cairo_ps(
        file = paste(
            gsub(
                " ",
                "_",
                iconv(my_variable, to = "ASCII//TRANSLIT")
            ),
            "_qqplot.eps",
            sep = ""
        ),
        width = 5,
        height = 5,
        pointsize = 18
    )
    
    par(mar = c(4.1, 4.1, 0.5, 0.3))
    
    qqnorm(
        y = my_data[, my_variable],
        xlab = "teoretické kvantily",
        ylab = "výběrové kvantily",
        main = ""
    )
    
    qqline(
        y = my_data[, my_variable],
        col = "red"
    )
    
    dev.off()
    
    
    #### histogramy -----------------------------------------------------------
    
        cairo_ps(
        file = paste(
            gsub(
                " ",
                "_",
                iconv(my_variable, to = "ASCII//TRANSLIT")
            ),
            "_histogram.eps",
            sep = ""
        ),
        width = 5,
        height = 5,
        pointsize = 18
    )
    
    par(mar = c(4.1, 4.1, 0.5, 0.3))
    
    hist(
        x = my_data[, my_variable],
        col = "lightgrey",
        xlab = paste(
            my_variable,
            " [cm]",
            sep = ""
        ),
        ylab = "absolutní četnosti",
        main = ""
    )
    
    dev.off()
    
}

setwd(mother_working_directory)


#### do konzole tisknu, která pozorování dané proměnné jsou suspektně
#### odlehlá ------------------------------------------------------------------

for(my_variable in colnames(my_data)[grepl("obvod", colnames(my_data))]){
    
    print("################################################")
    
    print(
        paste(
            "Suspektně odlehlé proměnné '",
            my_variable,
            "':",
            sep = ""
        )
    )
    
    print("-- dolní odlehlé:")
    print(
        which(
            my_data[, my_variable] < quantile(
                my_data[, my_variable],
                probs = 1/4,
                names = FALSE
            ) - 1.5 * (
                quantile(
                    my_data[, my_variable],
                    probs = 3/4,
                    names = FALSE
                ) - quantile(
                    my_data[, my_variable],
                    probs = 1/4,
                    names = FALSE
                )
            )
        )
    )
    
    print("-- horní odlehlé:")
    print(
        which(
            my_data[, my_variable] > quantile(
                my_data[, my_variable],
                probs = 3/4,
                names = FALSE
            ) + 1.5 * (
                quantile(
                    my_data[, my_variable],
                    probs = 3/4,
                    names = FALSE
                ) - quantile(
                    my_data[, my_variable],
                    probs = 1/4,
                    names = FALSE
                )
            )
        )
    )
    
}


#### počítám p-hodnoty Kolmogorova-Smirnova testu a Shapiro-Wilkova testu -----

my_p_values <- NULL

for(my_variable in colnames(my_data)[grepl("obvod", colnames(my_data))]){
    
    my_p_values <- rbind(
        
        my_p_values,
        c(
            suppressWarnings(
                ks.test(my_data[, my_variable], y = "pnorm")$p.value
            ),
            shapiro.test(my_data[, my_variable])$p.value
        )
        
    )
    
    rownames(my_p_values)[
        dim(my_p_values)[1]
    ] <- my_variable
    
    colnames(my_p_values) <- c(
        "p_level_kolmogorov",
        "p_level_shapiro"
    )
    
}

print(
    xtable(
        my_p_values,
        align = rep("", ncol(my_p_values) + 1),
        digits = 4
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)


#### vykresluji contour diagramy bivariantní normality ------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "bivariant_normality_matrix.eps",
    width = 10,
    height = 10,
    pointsize = 12
)

par(mar = c(4.1, 4.1, 2.1, 2.1))
par(mfrow = c(6, 6))

for(i in 2:7){
    
    for(j in 2:7){
        
        if(i == j){
            
            plot(
                0,
                type = "n",
                axes = FALSE,
                ann = FALSE
            )
            
        }else{
            
            mvnPlot(
                hzTest(my_data[, c(i, j)]),
                type = "contour",
                default = TRUE
            )
            
        }
        
    }
    
}

dev.off()


cairo_ps(
    file = "bivariant_normality_perspective_matrix.eps",
    width = 10,
    height = 10,
    pointsize = 14
)

par(mar = c(0.1, 0.1, 0.1, 0.1))
par(mfrow = c(6, 6))

for(i in 2:7){
    
    for(j in 2:7){
        
        if(i == j){
            
            plot(
                0,
                type = "n",
                axes = FALSE,
                ann = FALSE
            )
            
        }else{
            
            mvnPlot(
                hzTest(my_data[, c(i, j)]),
                type = "persp",
                default = TRUE,
                ylab = "",
                yaxt='n'
            )
            
        }
        
    }
    
}

dev.off()

setwd(mother_working_directory)


#### zkoumám outliery ---------------------------------------------------------

my_outliers <- mvOutlier(
    my_data[, grepl("obvod", colnames(my_data))],
    qqplot = TRUE,
    method = "adj.quan",
    label = TRUE
)

rownames(my_outliers$newData)[as.logical(my_outliers$outlier[, "Outlier"])]
    # které indexy jsou vícerozměrně odlehlé?
    
my_outliers$outlier[, "Mahalanobis Distance"][
    as.logical(my_outliers$outlier[, "Outlier"])
]   # jak velké mají Mahalanobisovy distance


#### vytvářím tabulku p-hodnot Henze-Zirklerova testu -------------------------

henze_zinkler_p_values <- matrix(rep(0, 6 * 6), nrow = 6)

for(i in 2:7){
    
    for(j in 2:7){
        
        if(i == j){
            
            henze_zinkler_p_values[i - 1, j - 1] <- 1.0
            
        }else{
            
            henze_zinkler_p_values[i - 1, j - 1] <- attr(
                hzTest(my_data[, c(i, j)]), "p.value"
            )
            
        }
        
    }
    
}

colnames(henze_zinkler_p_values) <- colnames(my_data)[
    grepl("obvod", colnames(my_data))
]

rownames(henze_zinkler_p_values) <- colnames(my_data)[
    grepl("obvod", colnames(my_data))
]

print(
    xtable(
        henze_zinkler_p_values,
        align = rep("", ncol(henze_zinkler_p_values) + 1),
        digits = 4
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)


## ----------------------------------------------------------------------------

###############################################################################

## zkouším PCA ----------------------------------------------------------------

#### kovarianční matice -------------------------------------------------------

cov(my_data[, grepl("obvod", colnames(my_data))])


#### modeluji PCA -------------------------------------------------------------

my_pca <- prcomp(
    my_data[, grepl("obvod", colnames(my_data))],
    center = TRUE,
    scale. = TRUE
)


#### tisknu sumář PCA ---------------------------------------------------------

summary(my_pca)

my_pca[["rotation"]]

print(
    xtable(
        my_pca[["rotation"]],
        align = rep("", ncol(my_pca[["rotation"]]) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)

print(
    xtable(
        summary(my_pca)[["importance"]],
        align = rep("", ncol(summary(my_pca)[["importance"]]) + 1),
        digits = 3
    ),
    floating = FALSE,
    tabular.environment = "tabular",
    hline.after = NULL,
    include.rownames = TRUE,
    include.colnames = TRUE
)


#### tisknu scree-plot --------------------------------------------------------

#screeplot(my_pca)

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "scree_plot.eps",
    width = 8,
    height = 5,
    pointsize = 12
)

par(mar = c(4.1, 4.1, 0.5, 0.3))

plot(
    summary(my_pca)[["importance"]]["Proportion of Variance", ],
    type = "b",
    xlab = "číslo hlavní komponenty",
    ylab = "podíl vysvětlené variability"
)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





