#install.packages("dplyr")
#install.packages("rvest")
#install.packages("googlesheets")
#install.packages("reshape2")
library(dplyr)
library(rvest)
library(googlesheets)
library(reshape2)
setwd("~/GitHub/kicktipp-to-tableau/")


## authorize Google Sheets
gs_auth(new_user = TRUE)


## -- main loop, to be run every ~5 minutes --
repeat {
  # print timestamp
  print(format(Sys.time(), "%a %b %d %H:%M:%S %Y"))
    
  
  # initialize
  spieltage <- 1:11
  spiel_id <- 1
  spiele_forGS <- data.frame(spiel_id = integer(), 
                             name = character(), 
                             ergebnis = character())
  tipps_forGS <- data.frame(spieler = character(), 
                            spiel_id = integer(), 
                            tipp = character(), 
                            punkte = integer())
  
  
  # download data
  for(i in spieltage) {
    url <- paste0("https://www.kicktipp.de/tableau/tippuebersicht?&spieltagIndex=", i)
    ## download page HTML
    tabelle <- url %>%
      read_html() %>%
      html_node(xpath='//*[@id="ranking"]') %>%
      html_table(fill = TRUE, 
                 header = FALSE)
    
    ## shorten table
    mannschaft1 <- tabelle[1, ] %>% 
      unlist() %>% 
      t() %>% 
      as.vector()
    mannschaft2 <- tabelle[2, ] %>% 
      unlist() %>% 
      t() %>% 
      as.vector()
    indices <- c(3, which(mannschaft1 != ""))
    tabelle <- select(tabelle, indices)
    
    ## extract matches
    spiele <- paste(tabelle[1, 2:length(indices)], 
                    tabelle[2, 2:length(indices)], 
                    sep = " - ")
    ergebnisse <- tabelle[3, 2:length(indices)] %>% 
      unlist() %>% 
      t() %>% 
      as.vector()
    spiele_tabelle <- cbind(spiel_id = spiel_id:(spiel_id + length(spiele) - 1), 
                            name = spiele, 
                            ergebnis = ergebnisse)
    
    ## extract bets
    tipps <- tabelle[4:nrow(tabelle), 1:ncol(tabelle)]
    names(tipps) <- c("spieler", spiel_id:(spiel_id + length(spiele) - 1))
    tipps <- melt(tipps, id = "spieler")
    tipps$tipp <- gsub("(\\d{1}:\\d{1})(\\d?)", "\\1", tipps$value)
    tipps$punkte <- ifelse(gsub("(\\d{1}:\\d{1})(\\d?)", "\\2", tipps$value) %in% c("", "-:-"), 
                           0, 
                           gsub("(\\d{1}:\\d{1})(\\d?)", "\\2", tipps$value))
    tipps_tabelle <- select(tipps, 
                    c("spieler", "variable", "tipp", "punkte")) %>% 
      rename(spiel_id = variable)
    
    
    ## append paginated data
    spiele_forGS <- rbind(spiele_forGS, 
                          spiele_tabelle)
    tipps_forGS <- rbind(tipps_forGS, 
                         tipps_tabelle)
    
    
    ## update game counter
    spiel_id <- spiel_id + length(spiele)
  }
  
  
  ## save to Google Sheets
  gs <- gs_key(gs_ls("Kicktipp Tableau WM 2018")$sheet_key)
  gs <- gs_edit_cells(gs, 
                      ws = "Spiele", 
                      input = spiele_forGS, 
                      verbose = TRUE)
  gs <- gs_edit_cells(gs, 
                      ws = "Tipps", 
                      input = tipps_forGS, 
                      verbose = TRUE)
  
  
  ## -- end of main loop --
  Sys.sleep(3000) # wait 5 minutes
}
