#kable1a.R
# https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf

# Learning how to use the kable package - elegant tables

# The script generates latex input, that is copied and pasted from RStudio Console window to a 
# Latex script, kable1a.tex and processed

options(knitr.table.format = "latex")


#devtools::install_github("haozhu233/kableExtra")
# Tutorial using mtcars dataset
library(kableExtra)
library(dplyr)
dt <- mtcars[1:5, 1:6]

################################################
# Simple table
kable(dt,"latex",booktabs=TRUE)


################################################
# Table with alternate rows shaded
kable(dt, "latex", booktabs = T) %>%
  kable_styling(latex_options = "striped")

################################################
# Table with selected rows shaded - need to add linesep
kable(mtcars[1:8, 1:4], "latex", booktabs = T,linesep = "") %>%
  kable_styling(latex_options = "striped", stripe_index = c(1,2, 5:6))

################################################
# Table scaled down, wil not work with long table
kable(cbind(dt, dt, dt), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

################################################
# Long table with repeat headers
long_dt <- rbind(mtcars, mtcars)
kable(long_dt, longtable = T, booktabs = T, caption = "Longtable") %>%
  add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6)) %>%
  kable_styling(latex_options = c("repeat_header"))

################################################
# Table - full width with caption
kable(dt,caption="Demo Wrap-around", booktabs = T) %>%
  kable_styling(full_width = TRUE) %>%
  column_spec(1, width = "8cm")

################################################
# Table for wrap text
kable(dt, caption="Demo Wrap-around", booktabs = T) %>%
  kable_styling(position = "float_right")

################################################
# Adjust font of text in table
kable(dt, booktabs = T) %>%
  kable_styling(font_size = 14)

################################################
#  Table with lots of text and need to adjust columns
text_tbl <- data.frame(
  Items = c("Item 1", "Item 2", "Item 3"),
  Features = c(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin vehicula tempor ex. Morbi malesuada sagittis turpis, at venenatis nisl luctus a. ",
    "In eu urna at magna luctus rhoncus quis in nisl. Fusce in velit varius, posuere risus et, cursus augue. Duis eleifend aliquam ante, a aliquet ex tincidunt in. ",
    "Vivamus venenatis egestas eros ut tempus. Vivamus id est nisi. Aliquam molestie erat et sollicitudin venenatis. In ac lacus at velit scelerisque mattis. "
  )
)
kable(text_tbl, booktabs = T) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, color = "red") %>%
  column_spec(2, width = "30em")

################################################
# More table manipulation with column_spec
that_cell <- c(rep(F, 7), T)
mtcars[1:8, 1:8] %>%
  kable(booktabs = T, linesep = "") %>%
  kable_paper(full_width = F) %>%
  column_spec(2, color = spec_color(mtcars$mpg[1:8]),
              link = "https://haozhu233.github.io/kableExtra") %>%
  column_spec(6, color = "white",
              background = spec_color(mtcars$drat[1:8], end = 0.7),
              popover = paste("am:", mtcars$am[1:8])) %>%
  column_spec(9, strikeout = that_cell, bold = that_cell,
              color = c(rep("black", 7), "red"))

################################################
# Table manipulation with row specs
kable(dt, booktabs = T) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(7, border_left = T, bold = T) %>%
  row_spec(1, strikeout = T) %>%
  row_spec(3:5, bold = T, color = "white", background = "navyblue")

################################################
# Header row with angle
kable(dt, booktabs = T, align = "c") %>%
  kable_styling(latex_options = "striped", full_width = F) %>%
  row_spec(0, angle = 45)

################################################
# Conditional logic for cell - needs manipulation of df inputted into kable() function
# Needs escape = F because of the introduction of latex elements in the df

cs_dt <- mtcars[1:10, 1:2]
cs_dt$car = row.names(cs_dt)

row.names(cs_dt) <- NULL
cs_dt$mpg = cell_spec(cs_dt$mpg, color = ifelse(cs_dt$mpg > 20, "red", "blue"))
cs_dt$cyl = cell_spec(
  cs_dt$cyl, color = "white", align = "c", angle = 45,
  background = factor(cs_dt$cyl, c(4, 6, 8), c("#666666", "#999999", "#BBBBBB")))
cs_dt <- cs_dt[c("car", "mpg", "cyl")]
kable(cs_dt, escape = F,format = "html") %>%
  kable_paper("striped", full_width = F)

################################################
# Another version, using dplyr

mtcars[1:10, 1:2] %>%
  mutate(
    car = row.names(.),
    mpg = cell_spec(mpg, "html", color = ifelse(mpg > 20, "red", "blue")),
    cyl = cell_spec(cyl, "html", color = "white", align = "c", angle = 45,
                    background = factor(cyl, c(4, 6, 8),
                                        c("#666666", "#999999", "#BBBBBB")))
  ) %>%
  select(car, mpg, cyl) %>%
  kable(format = "html", escape = F) %>%
  kable_styling("striped", full_width = F)

# Above works in html, need unclearly specified manual editing for latex

# now viridis colors
vs_dt <- iris[1:10, ]
vs_dt[1:4] <- lapply(vs_dt[1:4], function(x) {
  cell_spec(x, bold = T,
            color = spec_color(x, end = 0.9),
            font_size = spec_font_size(x))
})
vs_dt[5] <- cell_spec(vs_dt[[5]], color = "white", bold = T,
                      background = spec_color(1:10, end = 0.9, option = "A", direction = -1))
kable(vs_dt, escape = F, align = "c",format="html") %>%
  kable_classic("striped", full_width = F)

################################################
# Grouped heading
kable(dt, booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))

################################################
# Multi-level grouped heading
kable(dt, booktabs = T) %>%
  kable_styling(latex_options = "striped") %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2" = 2, "Group 3" = 2)) %>%
  add_header_above(c(" ", "Group 4" = 4, "Group 5" = 2)) %>%
  add_header_above(c(" ", "Group 6" = 6), bold = T, italic = T)

################################################
# Landscape, footnote and pack rows middle different alignment
kable(dt, caption = "Demo Table (Landscape)[note]", booktabs = T) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_header_above(c(" ", "Group 1[note]" = 3, "Group 2[note]" = 3)) %>%
  add_footnote(c("This table is from mtcars",
                 "Group 1 contains mpg, cyl and disp",
                 "Group 2 contains hp, drat and wt"),
               notation = "symbol") %>%
  pack_rows("Group 1", 4, 5) %>%
  landscape()

################################################
# Decimal alignment
k <- mtcars[1:10,1:5]
names(k) <- paste("{", names(k), "}")
kableExtra::kable(
  k, "latex", booktabs = TRUE, longtable = TRUE,
  align = c("l", rep("d", 4)), linesep = "", escape = FALSE) %>%
  kable_styling(full_width=FALSE)


