## Read GEDCOM file

## The Devil is in the Data
## lucidmanager.org/data-science
## Dr Peter Prevos

read.gedcom <- function(gedcom.loc) {
    require(stringr)
    require(tibble)
    require(dplyr)
    require(lubridate)
  
    my_formats <- c('dmY','%d/%m/%Y','%d %m %Y','%d %b %Y','d/m/Y','Y','%Y')

    gedcom <- str_squish(readLines(gedcom.loc))
    idv <- sum(grepl("^0.*INDI$", gedcom))
    fam <- sum(grepl("^0.*FAM$", gedcom))
    cat(paste("Individuals: ", idv, "\n"))
    cat(paste("Families: ", fam, "\n"))
    family <- tibble(id = NA,
                     Full_Name = NA,
                     Gender = NA,
                     Birth_Date = NA,
                     Birth_Place = NA,
                     Father_id = NA,
                     Mother_id = NA,
                     Death_Date = NA,
                     Death_Place = NA)
    ## Extract data
    extract <- function(line, type) {
        str_trim(str_sub(line, str_locate(line, type)[2] + 1))
    }
    id <- 0
    for (l in 1:length(gedcom)) {
        if (str_detect(gedcom[l], "^0") & str_detect(gedcom[l], "INDI$")) {
            id <- id + 1
            family[id, "id"] <- unlist(str_split(gedcom[l], "@"))[2]
            l <- l + 1
            while(!str_detect(gedcom[l], "^0")) {
                if (grepl("NAME", gedcom[l])) {
                    family[id, "Full_Name"] <- extract(gedcom[l], "NAME")
                    l <- l + 1
                } else if (grepl("SEX", gedcom[l])) {
                    family[id, "Gender"] <- extract(gedcom[l], "SEX")
                    l <- l + 1
                } else if (grepl("BIRT|CHR", gedcom[l])) {
                    l <- l + 1
                    while (!str_detect(gedcom[l], "^1")) {
                        if (grepl("DATE", gedcom[l]))
                            family[id, "Birth_Date"] <- extract(gedcom[l], "DATE")
                        if (grepl("PLAC", gedcom[l]))
                            family[id, "Birth_Place"] <- extract(gedcom[l], "PLAC")
                        l <- l + 1
                    }
                } else if (grepl("DEAT|BURI", gedcom[l])) {
                    l <- l + 1
                    while (!str_detect(gedcom[l], "^1")) {
                        if (grepl("DATE", gedcom[l]))
                            family[id, "Death_Date"] <- extract(gedcom[l], "DATE")
                        if (grepl("PLAC", gedcom[l]))
                            family[id, "Death_Place"] <- extract(gedcom[l], "PLAC")
                        l <- l + 1
                    }
                } else {
                  l <- l + 1
                }
            }
        }
        if (str_detect(gedcom[l], "^0") & str_detect(gedcom[l], "FAM")) {
            l <- l + 1
            while(!str_detect(gedcom[l], "^0")) {
                if (grepl("HUSB", gedcom[l]))
                    husband <- unlist(str_split(gedcom[l], "@"))[2]
                if (grepl("WIFE", gedcom[l]))
                    wife <- unlist(str_split(gedcom[l], "@"))[2]
                if (grepl("CHIL", gedcom[l])) {
                    child <- which(family$id == unlist(str_split(gedcom[l], "@"))[2])
                    family[child, "Father_id"] <- husband
                    family[child, "Mother_id"] <- wife
                }
                l <- l + 1
            }
        }
    }
    family %>%
        mutate(Full_Name = gsub("/", "", str_trim(Full_Name)),
               Birth_Date = parse_date_time(family$Birth_Date, my_formats),
               Death_Date = parse_date_time(family$Death_Date, my_formats)) %>%
        return()
}

