# 1 Preparation ===========
## 1.1 Load packages =========
pacman::p_load(
    curl,
    data.table,
    formatR,
    ggplot2,
    ggpubr,
    gstat,
    kableExtra,
    knitr,
    lme4,
    openxlsx,
    raster,
    rgdal,
    sf,
    sp,
    stringr
)

## 1.2 Set plot labels, function & parameters =====

{
    # Create color palette for density plot
    palette_den <- rev(c("#a1286a", "#cc595f", "#f78a53", "#ffd23f"))
    names(palette_den) <- levels(cut(2:38, breaks = c(1, 2, 10, 20, 38)))

    # Create labels for density plot
    label_den <- c("2", "3–10", "11–20", "21–38")
    names(label_den) <- levels(cut(2:38, breaks = c(1, 2, 10, 20, 38)))


    # Create color palette for access plot
    palette_acce <- c("#cdeae5", "#a8dadc", "#457b9d", "#366183", "#1d3557")
    names(palette_acce) <- levels(cut(0:2800, breaks = c(0, 60, 180, 540, 720, 2800)))

    # Create labels for access plot
    label_acce <- c("0-60", "60-180", "180-540", "540-720", "720+")
    names(label_acce) <- levels(cut(0:2800, breaks = c(0, 60, 180, 540, 720, 2800)))

    # Create variable vector with values and names
    variable <- c("Mean", "P2.5", "P97.5")
    names(variable) <- c("mean", "X0.025quant", "X0.975quant")
}

## 1.3 Read data ==========
{
    ancdata <- setDT(read.xlsx("https://raw.githubusercontent.com/MaiHoshito/2023-R-Final-Assignment/main/workdata/2015nepal.xlsx"))
    map <- st_read("/vsicurl/https://raw.githubusercontent.com/MaiHoshito/2023-R-Final-Assignment/main/workdata/province/2.shp")
    map <- st_as_sf(map)
    map_raster <- raster(map, resolution = 0.05)
    acce_raster <- readRDS(url("https://raw.githubusercontent.com/MaiHoshito/2023-R-Final-Assignment/main/workdata/accessibility_to_cities_2015_Nepal.rds"))
    acce_raster <- resample(acce_raster, map_raster)
}

## 1.4 Clean & recode =========

{
    ndhs <- ancdata[, .SD]

    ndhs <- ndhs[, X1 := str_pad(X1, width = 4, side = "left", pad = 0)][, setnames(.SD, "X1", "id")][complete.cases(ndhs), .SD]

    ndhs[, `:=`(
        age.cat = fcase(
            age > 10 & age <= 20, "11--20",
            age > 20 & age <= 42, "21--42"
        )
    )]

    ndhs[, `:=`(
        age.cat = factor(age.cat, levels = c("11--20", "21--42"))
    )]

    ndhs[, province := as.factor(province)]

    ndhs[, c("media", "ante.cat") := .(fcase(media == 1, "Yes", media == 0, "No"), fcase(antenatal_care == 1, "Yes", antenatal_care == 0, "No"))]


    ndhs[, `:=`(
        reli.cat = fcase(
            religion == 1, "Hindu",
            religion == 2, "Buddhist",
            religion == 3, "Muslim",
            religion == 4, "Kirat",
            religion == 5, "Christian",
            religion == 96, "Others"
        )
    )]

    ndhs[, `:=`(
        reli.cat = factor(reli.cat, levels = unique(ndhs$ reli.cat)[c(1, 3, 4, 5, 2)])
    )]

    ndhs[, `:=`(
        eth.cat = fcase(
            ethnicity == 1, "Hill Brahmin",
            ethnicity == 2, "Hill Chhetri",
            ethnicity == 3, "Terai Brahmin/Chhetri",
            ethnicity == 4, "Other Terai caste",
            ethnicity == 5, "Hill Dalit",
            ethnicity == 6, "Terai Dalit",
            ethnicity == 7, "Newar",
            ethnicity == 8, "Hill Janajati",
            ethnicity == 9, "Terai Janajati",
            ethnicity == 10, "Muslim",
            ethnicity == 96, "Others"
        )
    )]

    ndhs[, `:=`(
        eth.cat = factor(eth.cat, levels = unique(ndhs$eth.cat)[c(1:4, 6:7, 9, 5, 8, 11, 10)])
    )]

    ndhs[, `:=`(
        place_of_residence = fcase(
            place_of_residence == 1, "Urban",
            place_of_residence == 2, "Rural"
        )
    )]
    ndhs[education == 3, ]
    ndhs[, `:=`(
        edu.cat = fcase(
            education == 0, "No education",
            education == 1, "Primary, incompleted",
            education == 2, "Primary, completed",
            education == 3, "Secondary, incompleted",
            education == 4, "Secondary, completed",
            education == 5, "Higher"
        )
    )]

    ndhs[, `:=`(
        edu.cat = factor(
            edu.cat,
            levels = c(
                "No education", "Primary, incompleted",
                "Primary, completed",
                "Secondary, incompleted",
                "Secondary, completed", "Higher"
            )
        )
    )]

    ndhs[, `:=`(
        weal.cat = fcase(
            wealth == 1, "Poorest",
            wealth == 2, "Poorer",
            wealth == 3, "Middle",
            wealth == 4, "Richer",
            wealth == 5, "Richest"
        )
    )]

    ndhs[, `:=`(
        weal.cat = factor(weal.cat,
            levels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
        )
    )]
}

## 1.5 Accessibility data ============
{
    # Value inside
    acce_raster_inside <- mask(acce_raster, map)
    acce_raster_inside <- rasterToPoints(acce_raster_inside)
    acce_raster_inside <- data.table(acce_raster_inside)
    # Value outside
    acce_raster_outside <- mask(acce_raster, map, inverse = TRUE)
    acce_raster_outside <- rasterToPoints(acce_raster_outside)
    acce_raster_outside <- data.table(acce_raster_outside)
    setnames(acce_raster_inside, "accessibility_to_cities_2015_v1.0", "value")
    setnames(acce_raster_outside, "accessibility_to_cities_2015_v1.0", "value")
}


# 2 Descriptive analysis ========
## 2.1 Characteristics of participants ========

{
    character <- data.table(matrix(NA_character_, nrow = 40, ncol = 3))

    # Demographic

    j <- c(
        "age.cat", "reli.cat",
        "edu.cat", "weal.cat",
        "media",
        "place_of_residence", "ante.cat"
    )
    q <- 2
    p <- 3
    for (i in 1:length(j)) {
        k <- nrow(ndhs[, .N, by = c(j[i])][order(get(c(j[i])))])
        character[1, 1] <- "Number of participants"
        character[1, 2] <- ndhs[, .N]
        character[q, 1] <- c(j[i])

        character[p:(p + k - 1), 1:2] <- ndhs[, .N, by = c(j[i])][order(get(c(j[i])))]
        character[p:(p + k - 1), 3 := format(round(((as.numeric(V2) / ndhs[, .N]) * 100), 1), nsmall = 1)]

        p <- k + p + 1
        q <- q + k + 1
    }

    # Rename
    character[, V1 := fcase(
        V1 == "age.cat",
        "Age, year",
        V1 == "reli.cat",
        "Religion",
        V1 == "eth.cat",
        "Ethnicity",
        V1 == "edu.cat",
        "Education",
        V1 == "weal.cat",
        "Wealth",
        V1 == "media",
        "Access to media",
        V1 == "place_of_residence",
        "Place of residence",
        V1 == "ante.cat",
        "Antenatal care",
        rep_len(TRUE, length(V1)), V1
    )]

    character[, V1 := fcase(
        V1 %in% c("Yes", "No", "Rural", "Urban", 1:10, unlist(lapply(ndhs, levels))), paste("\u00A0\u00A0", V1),
        rep_len(TRUE, length(V1)), V1
    )]
}
# Remove rows from the data table where all columns have missing values
character <- character[!Reduce(`&`, lapply(character, is.na))]
View(character)

## 2.2 Geographic distribution ========
### 2.2.1 Participants ==========

sf_donor <- ancdata[, 2:12]
sf_donor_count <- ancdata[, 2:3]

coordinates(sf_donor) <- c("LONGNUM", "LATNUM")
crs(sf_donor) <- "+proj=longlat +datum=WGS84 +no_defs"
coordinates(sf_donor_count) <- c("LONGNUM", "LATNUM")
crs(sf_donor_count) <- "+proj=longlat +datum=WGS84 +no_defs"

sf_donor <- st_as_sf(sf_donor)
sf_donor_count <- st_as_sf(sf_donor_count)

donor_raster <- stack(
    rasterize(
        sf_donor,
        map_raster,
        fun = function(x, ...) mean(x)
    ),
    rasterize(
        sf_donor_count,
        map_raster,
        fun = function(x, ...) length(x)
    )
)

donor_raster <- rasterToPoints(donor_raster)

donor_raster <- data.table(donor_raster)
max(donor_raster$layer)
min(donor_raster$layer)

graph_subj <- ggplot() +
    theme_minimal() +
    geom_sf(
        data = map$geometry, colour = "#343a40",
        fill = "#F2F8EE", linewidth = 0.5
    ) +
    geom_tile(
        data = donor_raster,
        aes(x = x, y = y, fill = cut(layer, breaks = c(0, 1, 2, 10, 20, 38)))
    ) +
    ggtitle("A") +
    scale_fill_manual(
        values = palette_den,
        labels = label_den,
        guide = guide_legend(
            reverse = FALSE, nrow = 1,
            label.position = "bottom",
            title.position = "left"
        ),
        name = "Number of participants"
    ) +
    theme(
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 10, b = 10, r = 0, l = 0),
        legend.key.size = unit(0.5, "lines"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, margin = margin(b = 10), vjust = 1),
        legend.background = element_rect(fill = NA, colour = NA),
        panel.spacing.y = unit(0, "lines"),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggsn::north(map,
        symbol = 12,
        scale = 0.1,
        location = "topright"
    ) +
    ggsn::scalebar(map,
        dist = 100, dist_unit = "km", st.dist = 0.04,
        location = "bottomleft",
        transform = TRUE, model = "WGS84", st.size = 4
    )

### 2.2.2 Access to cities ==========

graph_acce <- ggplot() +
    theme_minimal() +
    geom_tile(
        data = acce_raster_inside,
        aes(x = x, y = y, fill = cut(value, breaks = c(0, 60, 180, 540, 720, 2800)))
    ) +
    geom_sf(
        data = map$geometry, colour = "#343a40",
        fill = NA, linewidth = 0.5
    ) +
    ggtitle("B") +
    scale_fill_manual(
        values = palette_acce,
        labels = label_acce,
        guide = guide_legend(
            reverse = FALSE, nrow = 1,
            label.position = "bottom",
            title.position = "left"
        ),
        name = "Travel Time (min)",
    ) +
    theme(
        legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 10, b = 10, r = 0, l = 5),
        legend.key.size = unit(0.5, "lines"),
        legend.key.width = unit(2, "lines"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, margin = margin(b = 10), vjust = 1),
        legend.background = element_rect(fill = NA, colour = NA),
        panel.spacing.y = unit(0, "lines"),
        panel.spacing.x = unit(2, "lines")
    ) +
    ggsn::north(map,
        symbol = 12,
        scale = 0.1,
        location = "topright"
    ) +
    ggsn::scalebar(map,
        dist = 100, dist_unit = "km", st.dist = 0.04,
        location = "bottomleft",
        transform = TRUE, model = "WGS84", st.size = 4
    )

fig1 <- ggpubr::ggarrange(
    graph_subj, graph_acce,
    widths = c(1, 1),
    ncol = 2,
    nrow = 1
) + ggpubr::bgcolor("#ffffff") + ggpubr::border(NA)

### 2.2.3 Extract raster values at participant locations =========

ndhs[, access_time := extract(acce_raster, cbind(LONGNUM, LATNUM))]
ndhs <- ndhs[, .SD, keyby = .(LONGNUM, LATNUM)][, .SD, by = rleid(LONGNUM, LATNUM)]
setnames(ndhs, "rleid", "gid")

## 2.3 Fit a glm model ===========

model <- glm(antenatal_care ~ access_time + age.cat + media + reli.cat + eth.cat + edu.cat + weal.cat, data = ndhs, family = binomial())


# Summarize the model
esti <- data.table(summary(model)$coef, keep.rownames = TRUE)[`Pr(>|z|)` < 0.05]
# Calculate the 95% CI
esti[, `Estimate (95% CI)` := paste0(
    round(Estimate, 3),
    " (",
    round(Estimate - 1.96 * `Std. Error`, 3),
    ", ",
    round(Estimate + 1.96 * `Std. Error`, 3),
    ")"
)]

esti[, rn := gsub("(\\.[a-z]+)([A-Z])", "\\1-\\2", rn)]
esti[rn == "mediaYes", rn := gsub("([a-z]+)([A-Z])", "\\1-\\2", rn)]

esti[, `Pr(>|z|)` := fcase(
    `Pr(>|z|)` < 0.01, "<0.01",
    `Pr(>|z|)` < 0.05 & `Pr(>|z|)` >= 0.01, "<0.05"
)]

names(esti)
setnames(esti, "rn", "Variable")
View(esti)
midfile <- list(character, fig1, esti)
names(midfile) <- c("tab1", "fig1", "tab2")
saveRDS(midfile, "midfile.RDS")
