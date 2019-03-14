#title: 
#description:
#inputs:
#outputs:

library(jpeg)
library(grid)
library(ggplot2)

court_file <- "C:/Users/CK/Documents/stat133/hw-stat133/workout01/images/nba-court.jpg"

court_image <- rasterGrob(readJPEG(court_file), width = unit(1, "npc"), height = unit(1, "npc"))

klay_shot_chart <- ggplot(data = thompson) + geom_point(aes(x = x, y = y, color = shot_made_flag))

klay_shot_chart <- ggplot(data = thompson) + annotation_custom(court_image, -250, 250, -50, 420)+ geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Klay Thompson (2016 season)') + theme_minimal()
ggsave("klay-thompson-shot-chart.pdf", plot = klay_shot_chart, path = "C:/Users/CK/Documents/stat133/hw-stat133/workout01/images")

draymond_shot_chart <- ggplot(data = green) + annotation_custom(court_image, -250, 250, -50, 420)+ geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Draymond Green (2016 season)') + theme_minimal()
ggsave("draymond-green-shot-chart.pdf", plot = draymond_shot_chart, path = "C:/Users/CK/Documents/stat133/hw-stat133/workout01/images")


kevin_shot_chart <- ggplot(data = durant) + annotation_custom(court_image, -250, 250, -50, 420)+ geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Kevin Durant (2016 season)') + theme_minimal()
ggsave("kevin-durant-shot-chart.pdf", plot = kevin_shot_chart, path = "C:/Users/CK/Documents/stat133/hw-stat133/workout01/images")


andre_shot_chart <- ggplot(data = iguodala) + annotation_custom(court_image, -250, 250, -50, 420)+ geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Andre Iguodala (2016 season)') + theme_minimal()
ggsave("andre-iguodala-shot-chart.pdf", plot = andre_shot_chart, path = "C:/Users/CK/Documents/stat133/hw-stat133/workout01/images")


stephen_shot_chart <- ggplot(data = curry) + annotation_custom(court_image, -250, 250, -50, 420)+ geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: Stephen Curry (2016 season)') + theme_minimal()
ggsave("stephen-curry-shot-chart.pdf", plot = stephen_shot_chart, path = "C:/Users/CK/Documents/stat133/hw-stat133/workout01/images")

players_shot_chart <- ggplot(data = players) + annotation_custom(court_image, -250, 250, -50, 420)+ geom_point(aes(x = x, y = y, color = shot_made_flag)) + ylim(-50, 420) + ggtitle('Shot Chart: GSW (2016 season)') + facet_wrap(~ name)
ggsave("gsw-shot-chart.pdf", plot = stephen_shot_chart, path = "C:/Users/CK/Documents/stat133/hw-stat133/workout01/images")
ggsave("gsw-shot-chart.png", plot = stephen_shot_chart, path = "C:/Users/CK/Documents/stat133/hw-stat133/workout01/images")




