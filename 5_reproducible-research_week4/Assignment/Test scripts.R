agg_eco_dmg_2 <- cleaned_data %>%
        group_by(EVTYPE) %>%
        summarise(propdmg_costs = sum(propdmg), 
                  cropdmg_costs = sum(cropdmg),
                  total_costs = sum(propdmg)+sum(cropdmg),
                  avg_costs = mean(propdmg) + mean(cropdmg)) %>%
        arrange(desc(total_costs)) %>%
        mutate_each(funs(prettyNum(., big.mark=",")))
print(head(agg_eco_dmg_2,10))

ggplot(top_10_data, aes(x = year, y = total_victims)) +
        geom_line() +
        facet_wrap(~EVTYPE, scales = "free") +
        labs(title = "Top 10 weather events causing the most victims", x = "Year", y = "Total Victims")

gathered_data <- cleaned_data %>%
        filter(EVTYPE %in% unique(top_10$EVTYPE)) %>%
        gather("victimType", "Victims", FATALITIES:INJURIES) %>%
        mutate(victimType = as.factor(victimType))

ggplot(top_10_data, aes(x = year, y = total_costs)) +
        geom_line() +
        facet_wrap(.~EVTYPE, scales = "free", shrink = TRUE) +
        labs(title = "Top 10 weather events causing the most economical damage", x = "Year", y = "Total Costs")