crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
head(USArrests)
head(crimes$state)
head(crimes)


# create tooltips and onclick events
# states_ <- sprintf("<p><strong>%s</strong></p>", as.character(crimes$state) )
states_ <- sprintf("<p><strong>%s</strong></p>", crimes$state )
table_ <- paste0(
  '<table align="right"><tr>',
  "<td>인구(만)</td>",
  sprintf("<td>%.0f</td>", crimes$UrbanPop * 10),
  '</tr><tr>',
  "<td>살인</td>",
  sprintf('<td>%.0f</td>', crimes$Murder),
  "</tr><tr>",
  "<td>폭행</td>",
  sprintf("<td>%.0f</td>", crimes$Assault),
  "</tr></table>"
)

# onclick <- sprintf(
#   "window.open(\"%s%s\")",
#   "http://en.wikipedia.org/wiki/",
#   as.character(crimes$state)
# )

onclick = sprintf("ttt(\"%s\")", as.character(crimes$state))

crimes$labs <- paste0(states_, table_)
crimes$onclick = onclick

# if (require("maps") ) {
  states_map <- map_data("state")
  gg_map <- ggplot(crimes, aes(map_id = state))
  gg_map <- gg_map + geom_map_interactive(aes(
    fill = Murder,
    tooltip = labs,
    data_id = state,
    onclick = onclick
  ),
  map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat)
  ggiraph(code = print(gg_map))
  girafe(ggobj = gg_map)
# }