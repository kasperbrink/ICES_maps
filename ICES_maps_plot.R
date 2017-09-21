library(tidyverse)
library(ggmap)
library(rgdal)
library(sp)
library(maptools)
library(leaflet)
library(htmlwidgets)
library(mapview)

ices <-readOGR("T:\\kasbri\\Shapefiles\\ices_rectangles\\ices_rectangles.shp")

iok <-
  list(
    a=(list(licens='1039 / 1139',art='Blåhvilling EU-farvand',name='BLH1X14', ices=c('43E0','37D6','32D4','34D4','36D6','42E0','35D5','31D4','35D4','44E0'))),
    b=(list(licens='1040 / 1140',art='Blåhvilling færøsk zone',name='BLH2X12F', ices=c('49E4','49E1','49E0','59E7','57E7','58E7'))),
    c=(list(licens='1028 / 1128',art='Brisling Skagerrak og Kattegat',name='BRS03A.',ices=c('44G0','43G0','44G1','41G1','43G1','42G1','41G0','42G0','41G2'))),
    d=(list(licens='1029 / 1129',art='Brisling Østersøen',name='BRS3BCD-C',ices=c('46H0','40G8','45G9','46G9','42G9','45H0','41G9','40G0','39G0','42G8'))),
    e=(list(licens='1027 / 1127',art='Brisling Nordsøen',name='BRSIOK',ices=c('37F7','39F6','40F5','38F4','35F3','37F3','37F4','39F5','36F3','36F4'))),
    f=(list(licens='1043 / 1143',art='Havgalt EU og Int. Zone',name='HAG678-',ices=c('25E1','24E2','25E2','26E1','40E0','46E2','23E3','36D8'))),
    g=(list(licens='1041 / 1141',art='Hestamkrel i Kanalen og vestlige farv.',name='HMK578/14',ices=c('46E2','40E0','43E0','44E0','37D9','41E0','42E0','24E2','36D8','35D7'))),
    h=(list(licens='606 / 626',art='Makrel i Nordsøen EU zone',name='MAK2A34',ices=c('47E8','46E8','46E9','47E9','45E8','48E8','49F1','48E7','48F0','46F1'))),
    i=(list(licens='608 / 628',art='Makrel i Nordsøen norsk zone',name='MAK2A4N',ices=c('52F2','57F1','51F2','56F0','55F1','56E9','58F1','56F1','48E9','51F3'))),
    j=(list(licens='629',art='Makrel i vestlige farvande',name='MAK2CX14',ices=c('46E2','38D9','47E4','39E0','35D8','44E0','47E3','48E4','48E5','43E0'))),
    k=(list(licens='601 / 621',art='Atlantoskandisk sild',name='SIL1/2',ices=c('59F0','66G2','58E8','67G2','63G0','60F0','64G0','62G0','61F1','57E7'))),
    l=(list(licens='1030 / 1130',art='Sild i vestlige Østersø',name='SIL22/24',ices=c('39G2','38G2','38G3','39G3','41G2','41G1','37G2','39G0','40G1','37G1'))),
    m=(list(licens='1031 / 1131',art='Sild i østlige Østersø',name='SIL25/32',ices=c('39G4','45H0','47G9','39G5','45G9','46H0','47H2','44G9','40G4','46G9'))),
    n=(list(licens='603 / 623',art='Sild i Skagerrak og Kattegat',name='SIL3A',ices=c('45G0','44G0','43G0','41G1','44F9','42G2','44G1','43G1','44F8','41G2'))),
    o=(list(licens='602 / 622',art='Sild i Nordsøen',name='SIL4AB',ices=c('45F0','43E9','46F0','46E9','47F0','47E9','48F0','49F2','49F0','48F1'))),
    p=(list(licens='605 / 625',art='Sild i Limfjorden',name='SIL4L',ices=c('42F8','42F9','43F8'))),
    q=(list(licens='Kun bifangst',art='Sild i vestlige farvande (bifangst)',name='SIL5B6ANB',ices=c('46E2','42E0','47E2'))),
    r=(list(licens='1042 / 1142',art='Sperling i Nordsøen, Skagerrak og Kattegat',name='SPE2A34',ices=c('46F0','45F0','49F1','49F0','46E9','47E9','45G0','47F0','44F9','45F1'))),
    s=(list(licens='1038 / 1138',art='Tobis i Nordsøen, Skagerrak og Kattegat',name='TBS2A3A4',ices=c('39F1','39F0','38F1','37F0','39F7','40F7','37F1','40F1','43F8','39F3')))
  )



col=rainbow(length(iok))

m <- get_map(c(lon = 3, lat = 62), zoom = 4, color = 'bw')


for (i in 1:length(iok)) {
  ices_s <- subset(ices, ICESNAME %in% iok[[i]]$ices)
  ices_s1 <- fortify(ices_s)
  print(
    ggmap(m) + geom_polygon(
      aes(x = long, y = lat, group = group),
      fill = col[i],
      size = .3,
      color = 'grey',
      data = ices_s1,
      alpha = .5) + 
      labs(
        title = paste0(
          'Vigtigste fangstområder (ICES rektangler) for fiskeri af \n',
          iok[[i]]$art,
          ' licens nr. ',
          iok[[i]]$licens)) + 
      labs(caption = 'Data fra fiskeristyrelsens logbogsregister') + 
      theme(
        plot.title = element_text(
          margin = margin(t = 10, b = 0)))
  )
  # dev.print(pdf,paste0(
  #             "T:\\kasbri\\Forespoergsler\\soren_palle\\kort\\",
  #             gsub('/', '_', iok[[i]]$name),
  #             "map.pdf")
  # )
  }

