library(ggplot2)
library(dplyr)
library(zoo)
rm(list = ls())
 
# olusturdugumuz formant tablosunun yer aldigi klasoru hazirliyoruz
setwd("C:/Users/SonerTrd/Desktop/unlu_semasi")

df <- read.csv("formants.Table", stringsAsFactors = FALSE) 

# olusturdugumuz data sizin dosyanizda da asagidaki gibi gorunmeli :
#     vowel time_index v_time time_abs   F1   F2   F3
# 1      a           1  0.000    0.751  642 1176 2705
# 2      a           2  0.124    0.874  634 1158 2714
# 3      e           1  0.000    3.718  495 1900 2578

#================================================================#
# https://tr.wikipedia.org/wiki/Uluslararas%C4%B1_Fonetik_Alfabe
# https://unicode-table.com/tr/alphabets/turkish/
vowel_lookup = 
  c(`a` = "\U0061",         # a
    `e` = "\U0065",         # e
    `i` = "\U0131",         # i
    `ii` = "\U0069",        # ii
    `o` = "\U006F",         # o
    `oo` = "\U00F6",        # oo
    `u` = "\U0075",         # u
    `uu` = "\U00FC"         # uu
  )

#================================================================#
# IPA sembollerini iceren bir sutun olusturma
# unlu seslerin kodlarini kullanarak isimleri siralama
df$IPA <- vowel_lookup[df$vowel]

#================================================================#
# disarida birakmak istedigimiz sesli harfler
exclude_these_Vs <- as.character("")

# bircok unlu tablosunda asagidaki sesler disarida birakilir 
exclude_these_Vs <- 
  c("cr","er","ar", "xx")
#================================================================#
# Eger ayni unlu sesin birden fazla kaydini aldiysaniz burada ortalamalari alinir
df_sum <- df %>%
  dplyr::filter(!vowel %in% exclude_these_Vs) %>%
  group_by(vowel, IPA, time_index) %>%
  summarise(F1 = mean(F1, na.rm = TRUE),
            F2 = mean(F2, na.rm = TRUE),
            F3 = mean(F3, na.rm = TRUE)) %>%
  group_by(vowel, IPA) %>%
  # ortalamaya gore 3 tane orneklem olusturuyor
  mutate(F1s = zoo::rollmean(F1, 3, na.pad = TRUE),
         F2s = zoo::rollmean(F2, 3, na.pad = TRUE),
         F3s = zoo::rollmean(F3, 3, na.pad = TRUE))

#================================================================#
# endpointlerden (aldigimiz timepointslerin son noktasindaki formant bilgileri) yeni bir data frame olusturma
df_endpt <- df_sum %>%
  dplyr::filter(!vowel %in% exclude_these_Vs) %>%
  group_by(vowel, IPA) %>%
  summarise(
    F1s = F1s[time_index == 9],
    F2s = F2s[time_index == 9],
    F3s = F3s[time_index == 9],
    #
    F1 = F1[time_index == 9],
    F2 = F2[time_index == 9],
    F3 = F3[time_index == 9])

#================================================================#
# unlu semamizi cizdiriyoruz
px_v_space_smooth <- df_sum %>%
  dplyr::filter(time_index > 1, time_index < 10) %>%
  ggplot(.)+
  aes(x = F2s, y = F1s, group = vowel, color = vowel)+
  # egimler
  geom_path(size = 1.2)+
  # IPA sembolleri kapsul icinde gosterilecek
  geom_label(data = df_endpt,label.padding = unit(0.01, "line"), 
             aes(label = IPA))+
  # asagidaki kodlarda yine IPA sembollerinin gosterimi ile ilgili, renk ve cizgiler
  geom_text(data = df_endpt,
            aes(label = IPA),
            color = "black", alpha = 0.6)+
  scale_x_reverse(position = "top", name = "F2 (Hz)")+
  scale_y_reverse(position = "right", name = "F1 (Hz)")+
  theme_bw()+
  theme(legend.position = "none")
px_v_space_smooth

# sekli kaydetme kismi
ggsave(px_v_space_smooth, file = "unlu_semam.png",
       height = 3.7, width = 4.8, dpi = 600)
# son

