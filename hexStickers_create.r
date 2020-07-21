pacman::p_load(hexSticker, extrafont)
loadfonts(device = "win", quiet = TRUE)

# Shego
download.file("https://i.pinimg.com/originals/9c/49/c4/9c49c48b0d6d674e560df2891838a696.jpg",
              destfile = paste0(tempdir() , ".png"), mode="wb")
#プロット
sticker(subplot = paste0(tempdir() , ".png"), package = "ShegoR",
        p_family = "Garamond", p_size = 10, p_x = 1, p_y = 1.65, s_x = 1.0, s_y = .85,
        s_width = 0.5, s_height = 0.5, p_color = "#16ae67", 
        h_fill = "#FDE4C0", h_color = "#76625A",
        filename = "shegoR.png")


# KP
download.file("https://imgix.bustle.com/uploads/image/2018/7/22/cef5812b-7674-489d-bcf3-7a40e70c0200-d1ceba98-f26a-4d8b-8fa6-a18631070cdd-kim-possible.png",
              destfile = paste0(tempdir() , ".png"), mode="wb")
#プロット
sticker(subplot = paste0(tempdir() , ".png"), package = "KP_R",
        p_size = 10, p_x = 1, p_y = 1.65, s_x = 1.0, s_y = .95,
        s_width = 0.75, s_height = 0.75, p_color = "green", 
        h_fill = "white", h_color = "black",
        filename = "KP_R.png")

## tvthemes
download.file("https://3.bp.blogspot.com/-kHda-2huAF8/WUdZKshMQkI/AAAAAAABFDM/sXt-PT9dKtYp2Y0Y_OV64TJzs7PvOLZmgCLcBGAs/s800/tv_boy_chikaku.png",
              destfile = paste0(tempdir() , ".png"), mode="wb")
#プロット
sticker(subplot = paste0(tempdir() , ".png"), package = "tvthemes",
        p_size = 14, p_x = 1, p_y = 1.65, s_x = 1.0, s_y = .95,
        s_width = 0.6, s_height = 0.6, p_color = "#8B4411", 
        h_fill = "#f5f5dc", h_color = "black",
        filename = "tvthems_hexsticker.png")

sticker(subplot = "../../../Desktop/tvthemes extras/tv_boy_chikaku.png", 
        package = "tvthemes",
        p_size = 18, p_x = 1, p_y = 1.65, s_x = 1.0, s_y = .95,
        s_width = 0.6, s_height = 0.6, p_color = "#8B4411", 
        h_fill = "#f5f5dc", h_color = "black",
        filename = "tvthemes_hexsticker_big.png")

sticker(subplot = "../../../Desktop/tvthemes extras/tv_boy_chikaku.png", 
        package = "tvthemes",
        p_size = 17, p_x = 1, p_y = 1.65, p_family = "Garamond",
        s_x = 1.0, s_y = .95,
        s_width = 0.6, s_height = 0.6, p_color = "#8B4411", 
        h_fill = "#f5f5dc", h_color = "black",
        filename = "tvthemes_hexsticker_big.png")


## bulletchartr
download.file("https://i.imgur.com/AcSNPNi.png",
              destfile = paste0(tempdir() , ".png"), mode="wb")
#プロット
sticker(subplot = paste0(tempdir() , ".png"), package = "bulletchartr",
        p_size = 14, p_x = 1, p_y = 1.55, s_x = 1.0, s_y = .95,
        s_width = 0.85, s_height = 0.85, p_color = "black", 
        h_fill = "white", h_color = "red",
        filename = "bulletchartr_hexsticker.png")

## 2
sticker(subplot = "../../../Desktop/bc_example_charts/bc_hex2.png", 
        package = "bulletchartr",
        p_size = 14, p_x = 1, p_y = 1.55, s_x = 1.0, s_y = .95,
        s_width = 0.78, s_height = 0.75, p_color = "black", 
        h_fill = "white", h_color = "red",
        filename = "bulletchartr_hexsticker.png")