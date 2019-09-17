# Unit 4 Live Session R Code

#Question 1

# Dataframe Arranged But Plot Not Ordered
FIFA %>% filter(!is.na(BallControl)) %>% group_by(Position) %>% summarize(meanBC = mean(BallControl),count = n()) %>% arrange(meanBC)
FIFA %>% filter(!is.na(BallControl)) %>% group_by(Position) %>% summarize(meanBC = mean(BallControl),count = n()) %>% arrange(meanBC) %>% ggplot(aes(x = Position, y = meanBC)) + geom_col()


# Data Frame arranged and Plot Ordered (arrange here is reducndent)

FIFA %>% filter(!is.na(BallControl)) %>% group_by(Position) %>% summarize(meanBC = mean(BallControl),count = n()) %>% arrange(meanBC) %>% ggplot(aes(x = factor(Position, levels = Position[order(meanBC)]), y = meanBC)) + geom_col()


# Data Frame not arranged but Plot Ordered
FIFA %>% filter(!is.na(BallControl)) %>% group_by(Position) %>% summarize(meanBC = mean(BallControl),count = n()) %>% ggplot(aes(x = factor(Position, levels = Position[order(meanBC)]), y = meanBC)) + geom_col()

#Question 2

#Dropping Unused Levels from a Factor using droplevels

FIFA %>% select(Agility,Acceleration,Position) %>% filter(Position == "LM" | Position == "LF") %>% droplevels() %>% ggpairs(aes(color = Position))

# Hack ... create new variable with only those levels

FIFA2 = FIFA %>% filter(Position == "LM" | Position == "LF")
FIFA2$Position = as.factor(as.character(FIFA2$Position))
FIFA2 %>% select(Agility,Acceleration,Position) %>% ggpairs(aes(color = Position))

