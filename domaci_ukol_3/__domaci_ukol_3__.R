###############################################################################
###############################################################################
###############################################################################

## instaluji a loaduji balíčky ------------------------------------------------

invisible(
    lapply(
        c(
            "xtable",
            "openxlsx",
            "foreign"
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

while(!"__domaci_ukol_3__.R" %in% dir()){
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
    
    file = "du3.sav",
    to.data.frame = TRUE
    
)

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## (pre)processing dat --------------------------------------------------------

#### doplňuji jména proměnným v datasetu "my_data" ----------------------------

for(i in 1:dim(my_data)[2]){
    
    if(unname(attr(my_data, "variable.labels"))[i] != ""){
        
        colnames(my_data)[i] <- unname(
            attr(
                my_data,
                "variable.labels"
            )
        )[i]
        
    }
    
}


#### odstaňuji z datasetu duplicity -------------------------------------------

my_data <- my_data[!duplicated(my_data$model), ]


#### z hodnot proměnné "model" vytvářím názvy řádků ---------------------------

rownames(my_data) <- as.character(my_data[, "model"])

my_data <- my_data[, setdiff(colnames(my_data), "model")]


## ----------------------------------------------------------------------------

###############################################################################

## omezuji dataset na proměnné zájmu ------------------------------------------

#### ponechávám následující ---------------------------------------------------

# cena (EUR),
# objem válců (ccm),
# výkon (kW),
# maximální rychlost (km/h),
# zrychlení 0 -- 100 km/h (s),
# spotřeba - kombinovaná (l),
# hmotnost (kg),
# délka (mm),
# výška (mm),
# užitná hmotnost (kg) a
# objem zavazadlového prostoru (l)

#### odstraňuji následující ---------------------------------------------------

# spotřeba -- město (l)
# spotřeba -- mimo město (l)
# emise (g/km)
# šířka (mm)
# rozvor kol (mm)

my_data <- my_data[
    
    ,
    setdiff(
        colnames(my_data),
        c(
            "spotřeba - město (l)",
            "spotřeba - mimo město (l)",
            "emise (g/km)",
            "šířka (mm)",
            "rozvor kol (mm)"            
        )
    )
    
]


## ----------------------------------------------------------------------------

###############################################################################

## explorativní analýza dat ---------------------------------------------------

#### vytvářím boxploty --------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

for(my_variable in colnames(my_data)){

    cairo_ps(
        file = paste(
            gsub(
                "[ /\\(\\)]",
                "_",
                iconv(my_variable, to = "ASCII//TRANSLIT")
            ),
            "_boxplot.eps",
            sep = ""
        ),
        width = 5,
        height = 6,
        pointsize = 18
    )
    
    par(mar = c(4.1, 4.1, 2.1, 0.1))
    
    boxplot(
        x = my_data[, my_variable],
        col = "lightgrey",
        ylab = my_variable,
        main = if(my_variable == "objem zavazadlového prostoru (l)"){
            "objem zavaz. prostoru (l)"
        }else{
            my_variable
        }
    )
    
    dev.off()
    
}


#### dummy prázdný čtverec pro potřeby sazby dokumentu ------------------------

cairo_ps(
    file = "empty_square.eps",
    width = 5,
    height = 6,
    pointsize = 18
)

par(mar = c(4.1, 4.1, 2.1, 0.1))

plot(0, type = 'n', axes = FALSE, ann = FALSE)

dev.off()
    
    
setwd(mother_working_directory)


#### ukládám matici scatterplotů ----------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "scatterplot_matrix.eps",
    width = 8,
    height = 8,
    pointsize = 14
)

par(mar = c(0.1, 0.1, 0.1, 0.1))

temp_data <- my_data
#colnames(temp_data) <- gsub("(.*)( \\(.*)", "\\1", colnames(my_data))
colnames(temp_data) <- c(
    "cena",
    "objem\nválců",
    "výkon",
    "maxim.\nrychlost",
    "zrychlení",
    "spotřeba",
    "hmotnost",
    "délka",
    "výška",
    "užitná\nhmotnost",
    "objem\nzavaz.\nprostoru"
)

pairs(
    temp_data,
    panel = "panel.smooth",
    cex = 0.6#0.75
)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## shluková analýza -----------------------------------------------------------

#### standardizace proměnných na interval <0, 1> ------------------------------

for(i in 1:dim(my_data)[2]){
    
    my_data[, i] <- (
        my_data[, i] - min(my_data[, i])
    ) / (
        max(my_data[, i]) - min(my_data[, i])
    )
    
}


#### hierarchická shluková analýza, Wardova metoda ----------------------------

my_hclust <- hclust(dist(x, method = 'euclidean'), method = 'ward.D2')



#### ukládám dendrogram -------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

cairo_ps(
    file = "dendrogram.eps",
    width = 18,
    height = 24,
    pointsize = 14
)

par(mar = c(4, 4, 0, 10))

plot(
    as.dendrogram(my_hclust),
    horiz = TRUE,
    xlab = "míra nepodobnosti / 100 000",
    ylab = "model automobilu",
    xaxt = "n",
    axes = FALSE
)

axis(side = 1, at = seq(0, 8e5, by = 2e5), labels = seq(0, 8, by = 2))

dev.off()

setwd(mother_working_directory)


#### subdendrogramy -----------------------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))


#### spíše levnější a užitkové vozy, rozšířené, oblíbené ----------------------

cairo_ps(
    file = "subdendrogram_1.eps",
    width = 6,
    height = 8,
    pointsize = 18
)

par(mar = c(4, 4, 0.5, 8.2))

plot(
    as.dendrogram(my_hclust),
    horiz = TRUE,
    xlab = "míra nepodobnosti\n/ 100 000",
    ylab = "model automobilu",
    xaxt = "n",
    axes = FALSE,
    xlim = c(2e5, 0),
    ylim = c(1, 29)
)

axis(side = 1, at = seq(0, 8e5, by = 2e5), labels = seq(0, 8, by = 2))

dev.off()


#### heterogenní skupina vozů, spíše levnější, mnohdy menších rozměrů ---------

cairo_ps(
    file = "subdendrogram_2.eps",
    width = 6,
    height = 8,
    pointsize = 18
)

par(mar = c(4, 4, 0.5, 8.2))

plot(
    as.dendrogram(my_hclust),
    horiz = TRUE,
    xlab = "míra nepodobnosti\n/ 100 000",
    ylab = "model automobilu",
    xaxt = "n",
    axes = FALSE,
    xlim = c(2e5, 0),
    ylim = c(30, 60)
)

axis(side = 1, at = seq(0, 8e5, by = 2e5), labels = seq(0, 8, by = 2))

dev.off()


#### dostupné městské vozy, často combi ---------------------------------------

cairo_ps(
    file = "subdendrogram_3.eps",
    width = 6,
    height = 8,
    pointsize = 18
)

par(mar = c(4, 4, 0.0, 10))

plot(
    as.dendrogram(my_hclust),
    horiz = TRUE,
    xlab = "míra nepodobnosti\n/ 100 000",
    ylab = "model automobilu",
    xaxt = "n",
    axes = FALSE,
    xlim = c(2e5, 0),
    ylim = c(61, 79)
)

axis(side = 1, at = seq(0, 8e5, by = 2e5), labels = seq(0, 8, by = 2))

dev.off()


#### klasické městské vozy, spíše dražší --------------------------------------

cairo_ps(
    file = "subdendrogram_4.eps",
    width = 6,
    height = 8,
    pointsize = 18
)

par(mar = c(4, 4, 0.5, 10))

plot(
    as.dendrogram(my_hclust),
    horiz = TRUE,
    xlab = "míra nepodobnosti\n/ 100 000",
    ylab = "model automobilu",
    xaxt = "n",
    axes = FALSE,
    xlim = c(2e5, 0),
    ylim = c(80, 106)
)

axis(side = 1, at = seq(0, 8e5, by = 2e5), labels = seq(0, 8, by = 2))

dev.off()


#### drahé, luxusní vozy ------------------------------------------------------

cairo_ps(
    file = "subdendrogram_5.eps",
    width = 6,
    height = 8,
    pointsize = 18
)

par(mar = c(4, 4, 0.0, 10))

plot(
    as.dendrogram(my_hclust),
    horiz = TRUE,
    xlab = "míra nepodobnosti\n/ 100 000",
    ylab = "model automobilu",
    xaxt = "n",
    axes = FALSE,
    xlim = c(2e5, 0),
    ylim = c(107, 127)
)

axis(side = 1, at = seq(0, 8e5, by = 2e5), labels = seq(0, 8, by = 2))

dev.off()


#### velmi drahé, špičkové modely ---------------------------------------------

cairo_ps(
    file = "subdendrogram_6.eps",
    width = 6,
    height = 8,
    pointsize = 18
)

par(mar = c(4, 4, 0.0, 10))

plot(
    as.dendrogram(my_hclust),
    horiz = TRUE,
    xlab = "míra nepodobnosti\n/ 100 000",
    ylab = "model automobilu",
    xaxt = "n",
    axes = FALSE,
    xlim = c(2e5, 0),
    ylim = c(128, 135)
)

axis(side = 1, at = seq(0, 8e5, by = 2e5), labels = seq(0, 8, by = 2))

dev.off()


#### --------------------------------------------------------------------------

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





