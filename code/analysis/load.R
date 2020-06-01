library(tidyverse)
library(haven)
library(stargazer)
library(starpolishr)
library(ggsignif)
library(starbility)

palette = c('#808080', '#006400', '#bb0a1e')
tablenotes = rjson::fromJSON(file='code/table-notes.json')

theme_excuses <- function () { 
  theme_bw(base_size=12, base_family="LM Roman 10") %+replace% 
    theme(
      axis.title.x = element_blank()
    )
}
theme_excuses = list(theme_bw(base_size=14, 
                              base_family="LM Roman 10"),
                     theme(axis.title.x = element_blank(),
                           axis.text = element_text(size=14)),
                     scale_fill_manual(values=palette),
                     scale_color_manual(values=palette),
                     guides(fill=F,col=F))

two_palette = list(scale_fill_manual(values=palette[2:3]),
                   scale_color_manual(values=palette[2:3]))
