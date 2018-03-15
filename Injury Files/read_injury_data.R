# Issues with string format (player names with accents)
Sys.setlocale("LC_ALL", "C") 
library(data.table)
#library(sqldf)
#library(dplyr)

# Read Data
player_minutes <- read.csv("Injury Files/ScrapedMinutes.csv", header = T, stringsAsFactors = F)
colnames(player_minutes)[1] <- "Competition"
transfer_market <- read.csv(file = 'Injury Files/TransferMarktInjuries.tsv', stringsAsFactors = F, sep = '\t', header = TRUE)

player_minutes <- as.data.table(player_minutes)
transfer_market <- as.data.table(transfer_market)

# Reformat date columns to data type
transfer_market$DateBirthday <- as.Date(transfer_market$DateBirthday)
transfer_market$StartDate <- as.Date(transfer_market$StartDate)
transfer_market$EndDate <- as.Date(transfer_market$EndDate)

player_minutes$Date <- as.Date(player_minutes$Date, format = "%m/%d/%Y")
player_minutes$PlayerBirthdate <- as.Date(player_minutes$PlayerBirthdate, format = "%y/%m/%d")

# Split injuries into soft tissue and contact injury
string_pattern <- c("break", "fracture", "crack", "broke", "malleolar")
transfer_market <- transfer_market[, InjuryType := ifelse(grepl(paste(string_pattern, collapse = "|"), tolower(Description)), "Contact Injury", "Soft Injury")]

# Duration of Injury
setkey(transfer_market, PlayerData, DateBirthday)
transfer_market <- transfer_market[, Duration := (EndDate - StartDate), by = c("PlayerData", "DateBirthday")]

# Split players into short rest and long rest
setkey(player_minutes, Player, PlayerBirthdate)
player_minutes <- player_minutes[, RestPeriod := c(0,diff(Date)), by = c("Player","PlayerBirthdate")]
player_minutes <- player_minutes[, Rest := ifelse(RestPeriod < 7, "Short Rest", "Long Rest")]

# join both tables by name and birthday
merge_table <- merge(x = player_minutes, y = transfer_market,
                     by.x = c("Player","PlayerBirthdate"),
                     by.y = c("PlayerData", "DateBirthday"), all.y = T, allow.cartesian = T)

# Add column to show whether player is playing with prior injury
setkey(merge_table, Player, PlayerBirthdate)
merge_table <- merge_table[, PlayingWithInjury := ifelse(merge_table$Date > merge_table$StartDate & merge_table$Date <= merge_table$EndDate, "Yes", "No")]

# Add column to show whether the injury was caused during a match or not
setkey(merge_table, Player, PlayerBirthdate)
merge_table <- merge_table[, Occurrence := ifelse(merge_table$Date == merge_table$StartDate, " Current Match", "Previous Match")]

# subset by matches which lie between the start and end date of the injury
merge_table_subset <- merge_table[merge_table$Date >= merge_table$StartDate & merge_table$Date <= merge_table$EndDate,]

# subset by matches where the injury occured
#injury_during_match <- merge_table[merge_table$Date == merge_table$StartDate,]

# subset by injury not in a match (found afterwards or during training?)
#injury_not_during_match <- merge_table[merge_table$Date > merge_table$StartDate & merge_table$Date <= merge_table$EndDate,]


write.csv(merge_table_subset, file = "Injury Files/merge_table.csv")



#####################################################################


# Using sqldf, works but dates are causing issues. scraped in the end


# # Split injuries into soft tissue and contact injury
# 
# y <- sqldf("SELECT x.Date, x.Player,
#            case when x.Description like '%break%' then 'Contact'
#            when x.Description like '%fracture%' then 'Contact'
#            ELSE 'Soft' END as Injury
#            From x")
# 
# # using sql because it makes life so much easier
# 
# x = sqldf("SELECT pm.*, tm.StartDate, tm.EndDate, tm.Description  
#           FROM player_minutes pm INNER JOIN transfer_market tm
#           ON pm.Player = tm.PlayerData
#           WHERE pm.Date >= tm.StartDate AND pm.Date <= tm.EndDate")
# 
# # Split players into short rest and long rest
# 
# short_rest <- sqldf("SELECT * FROM player_minutes pm
#                     WHERE exists (select 1 
#                     from player_minutes pm2
#                     where pm2.Player = pm.Player and
#                     pm.Date between pm2.Date and pm2.Date + 7)")
# 
# 
# short_rest <- sqldf("SELECT * FROM player_minutes pm
#                     WHERE exists (select pm2.Player, pm2.Date, 
#                     case when (pm2.Player = pm.Player and pm.Date Datediff(pm2.Date, pm2.Date) <= 7) then 'Short'
#                     Else 'Long' END as Rest
#                     FROM player_minutes)")
# 
# short_rest <- sqldf("
#                     SELECT 
#                     pm2.Player,
#                     cast (pm2.Date as Date) as Date1,
#                     cast(pm.Date as date) as Date2,
#                     (cast(pm2.Date as date) - cast(pm.Date as date)) as date_diff
#                     FROM player_minutes pm2
#                     
#                     LEFT JOIN (SELECT 
#                     Date,
#                     Player
#                     FROM player_minutes
#                     GROUP BY Date,Player) PM ON PM.Player = PM2.Player
#                     group by 1,2
#                     
#                     ")
# 
# short_rest <- sqldf("SELECT 
#                     pm2.Player,
#                     cast (pm2.Date as Date) as Date1,
#                     cast(pm.Date as date) as Date2,
#                     (cast(pm2.Date as date) - cast(pm.Date as date)) as date_diff
#                     FROM player_minutes pm2
#                     
#                     LEFT JOIN (SELECT 
#                     Date,
#                     Player
#                     FROM player_minutes
#                     GROUP BY Date,Player) PM ON PM.Player = PM2.Player
#                     group by 1,2
#                     
#                     ")
# 
# 
# dates <- sqldf("select * From player_minutes 
#                where Date < '20-08-2012'")
# 
# 
