#Testing CUSUM table for COVID markdown
prev_dates <- c("-", "-", "-", "25 July 2022", "13 June 2022", "-", "24 January 2022", "8 August 2022", "24 January 2022",
                "8 August 2022", "8 August 2022", "8 August 2022", "7 February 2022", "7 February 2022", "7 February 2022",
                "7 February 2022", "7 February 2022", "7 February 2022", "-", "-", "-", "-", "-", "-", "7 February 2022",
                "7 February 2022", "7 February 2022", "7 February 2022", "25 July 2022", "8 August 2022", "8 August 2022",
                "8 August 2022", "8 August 2022", "8 August 2022", "8 August 2022", "8 August 2022")


commas <- function(x) format(x, big.mark = ",")

tab <- data.frame(Event = rep(c("Fever", "MA"), times = c(3, 33)),
                  Brand = rep(c("Spikevax 6m-5y (Moderna)", "Comirnaty 5-11 (Pfizer)", "Comirnaty (Pfizer)",
                                "Vaxzevria (AstraZeneca)", "Spikevax 6m-5y (Moderna)", "Spikevax 6-11 (Moderna)",
                                "Spikevax (Moderna)", "Nuvaxovid (Novavax)"), 
                              times = c(3, 3, 6, 6, 3, 3, 6, 6)),
                  Dose = rep(c(rep(c("1", "2", "3 & Booster"), 2), rep(rep(c("1", "2", "3 & Booster"), each = 2), 2)), 2),
                  Age = c(rep("6m-5 years", 3), rep("5-11 years", 3),
                          rep(c("12-<50 years", paste("\U2265", "50 years", sep = "")), 3*2),
                          rep("6m-5 years", 3), rep("6-11 years", 3),
                          rep(c("12-<50 years", paste("\U2265", "50 years", sep = "")), 3),
                          rep(c("18-<50 years", paste("\U2265", "50 years", sep = "")), 3)),
                  prev_dates)
colnames(tab) <- c("Event Type", "Vaccine", "Dose", "Age Group", "Analysis Date")

tab <- tab[-which(tab$Vaccine %in% c("Spikevax 6m-5y (Moderna)", "Spikevax 6-11 (Moderna)") & tab$Dose == "3 & Booster"),]
tab <- tab[-which(tab$Vaccine %in% c("Spikevax 6m-5y (Moderna)", "Spikevax 6-11 (Moderna)") & tab$Dose == "2"),]
stopped_sgps <- list(pf = which(tab$Vaccine == "Comirnaty (Pfizer)" & tab$Dose %in% c("1","2")),
                     az = which(tab$Vaccine == "Vaxzevria (AstraZeneca)"),
                     sp = which(tab$Vaccine == "Spikevax (Moderna)" & tab$Dose %in% c("1","2")))
                    
tab[unlist(stopped_sgps), 4:8] <- ""
tab[min(stopped_sgps$pf), 4] <- "CUSUM analysis ceased as of 17 October 2022 due to small numbers of responses"
tab[c(min(stopped_sgps$az), min(stopped_sgps$sp)), 4] <- "CUSUM analysis ceased as of 7 February 2022 due to small numbers of responses"
tab$v_dummy <- paste0(tab$Vaccine, tab$Dose)


ft_1 <- flextable(tab, col_keys = c("Event Type", "Vaccine", "Dose", "Age Group", "Analysis Date")) %>% 
  color(part = "header", color = "white") %>%
  bold(part = "header", bold = TRUE) %>%
  align(part = "all", align = "center") %>%
  bg(part = "header", bg = rgb(27/255,117/255,188/255)) %>%
  merge_v(j = 1) %>%
  merge_at(i = stopped_sgps$pf, j = 4:5, part = "body") %>%
  merge_at(i = stopped_sgps$az, j = 4:5, part = "body") %>%
  merge_at(i = stopped_sgps$sp, j = 4:5, part = "body") %>%
  border_inner_h(border = fp_border(), part = "body") %>%
  fontsize(size = 8.5, part = "all") %>%
  padding(padding = 1, part = "body") %>%
  set_table_properties(layout = "autofit")

ft_2 <- merge_v(ft_1, j = c("Vaccine", "v_dummy"), target = c("Vaccine", "Dose"))

ft_2
