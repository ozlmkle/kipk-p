# ------------------ Paketler ------------------
kütüphane (flexdashboard)
kütüphane (readr)
kütüphane (ggplot2)
kütüphane (dplyr)
kütüphane (tidyr)
kütüphane (arsa)
kütüphane (highcharter)
# ------------------ Veri ------------------
koronavirüs <- read.csv ("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", stringsAsFactors = FALSE)
korona <- koronavirüs%>% 
 pivot_wider (names_from = tür, değerler_from = vakalar)
corona $ onaylandı [is.na (corona $ onaylandı)] = 0
corona $ ölüm [is.na (corona $ ölüm)] = 0
korona $ kurtarıldı [is.na (korona $ kurtarıldı)] = 0
coronavirus_Turkey <- corona%>%
  filtre (ülke == "Türkiye")
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (confirmation_cum = cumsum (onaylandı))%>%
  mutate (confirmation_death = cumsum (ölüm))%>%
  mutate (confirm_recovered = cumsum (kurtarıldı))%>%
  mutate (onaylanmış_aktif = cumsum (onaylanmış) -cumsum (ölüm) -cumsum (geri kazanılmış))
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (death_ratio = (ölüm / onaylandı) * 100)
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (confirmation_death_ratio = (confirm_death / confirm_cum) * 100)
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (recoveryed_ratio = (kurtarıldı / onaylandı) * 100)
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (confirmation_recovered_ratio = (confirm_recovered / confirm_cum) * 100)
korona <- korona%>%
  mutate (confirmation_cum = cumsum (onaylandı))%>%
  mutate (confirmation_death = cumsum (ölüm))%>%
  mutate (confirm_recovered = cumsum (kurtarıldı))%>%
  mutate (onaylanmış_aktif = cumsum (onaylanmış) -cumsum (ölüm) -cumsum (geri kazanılmış))
koronavirüs_US <- korona%>%
  filtre (ülke == "ABD")
koronavirüs_US <- koronavirüs_US%>%
  mutate (confirm_cum = cumsum (onaylandı))
coronavirus_Germany <- korona%>%
  filtre (ülke == "Almanya")%>%
  mutate (confirm_cum = cumsum (onaylandı))
coronavirus_Japan <- corona%>%
  filtre (ülke == "Japonya")%>%
  mutate (confirm_cum = cumsum (onaylandı))
coronavirus_Brazil <- korona%>%
  filtre (ülke == "Brezilya")%>%
  mutate (confirm_cum = cumsum (onaylandı))
tophastgr <- coronavirus_Turkey%>% 
  left_join (coronavirus_US, by = "tarih")%>%
  left_join (coronavirus_Germany, by = "tarih")%>%
  left_join (coronavirus_Japan, yazan = "tarih")%>%
  left_join (coronavirus_Brazil, = "tarih" ile)
''


Dünya
================================================ =====================

{Data-width = 430} Sütunu
-------------------------------------------------- ---------------------

### Dünyada En Yüksek Toplam Vaka Sayısına Sahip 10 Ülke
`` {r}
ry <-corona%>% group_by (ülke)%>%
     özetle (onaylandı = toplam (onaylandı))%>%
     düzenlemek (onaylandı)%>%
     düzenlemek (azalan (onaylandı))
ry%>%
  düzenle (azalan (onaylandı))%>%
  kafa (10)%>% 
  hchart (tür = "bar", hcaes (x = ülke, y = onaylandı))%>%
  hc_yAxis (text = "Ülke")%>% 
  hc_xAxis (text = "Toplam Hasta Sayısı")
''

### Covid 19 Vakalarının Dünya'daki Dağılımı
`` {r}
koronavirüs%>% 
  group_by (tür, tarih)%>%
  özet (total_cases = sum (vakalar))%>%
  pivot_wider (names_from = type, values_from = total_cases)%>%
  düzenlemek (tarih)%>%
  mutate (confirm_total = cumsum (onaylandı),
                recoveryed_total = cumsum (kurtarıldı),
                death_total = cumsum (ölüm))%>%
  plot_ly (x = ~ tarih,
                  y = ~ onaylanan_toplam,
                  name = 'Toplam Vaka Sayısı', 
                  fillcolor = '# 7ec0ee',
                  type = 'scatter',
                  mode = 'yok', 
                  stackgroup = 'bir')%>%
  add_trace (y = ~ death_total, 
             name = "Toplam Vefat Sayısı",
             fillcolor = '# E41317')%>%
  add_trace (y = ~ kurtarılmış_toplam, 
            name = 'Toplam İyileşen Sayısı', 
            fillcolor = 'forestgreen')%>%
  düzen (açıklama = liste (x = 0.1, y = 0.9),
         yaxis = liste (title = "Toplam Vaka Sayısı"),
         xaxis = list (title = "Tarih"))
''



{Data-width = 430} Sütunu
-------------------------------------------------- ---------------------

### 5 Ülke'nin Karşılaştırılması
`` {r}
plot_ly (data = tophastgr)%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum,
            ad = "Brezilya", satır = liste (genişlik = 2))%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum.yy,
                    isim = "Japonya", satır = liste (genişlik = 2))%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum.xx,
            isim = "Almanya", satır = liste (genişlik = 2))%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum.y,
            ad = "ABD", satır = liste (genişlik = 2))%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum.x,
            name = "Türkiye", satır = liste (genişlik = 2))%>%
  düzen (yaxis = liste (title = "Toplam Vaka Sayısı", tür = "günlük"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Dünyada En Yüksek Toplam Vaka Sayısına Sahip 10 Ülke
`` {r}
ry%>%
  düzenle (azalan (onaylandı))%>%
  kafa (10)%>%
  plot_ly (etiketler = ~ ülke, değerler = ~ onaylandı,
          textinfo = "etiket + yüzde",
          tür = 'pasta')
''



{Data-width = 195} Sütunu
-------------------------------------------------- ---------------------

### Toplam Vaka Sayısı {.value-box}
`` {r}
kutu <- (corona $ confirm_cum [uzunluk (corona $ confirm_cum [! is.na (corona $ confirm_cum)])])
valueBox (kutu, simge = "fa-artı-kare", renk = "# cd3333")
''

### Toplam İyileşen Sayısı {.value-box}
`` {r}
kutu <- (corona $ confirm_recovered [uzunluk (corona $ confirm_recovered [! is.na (corona $ confirm_recovered)])])
valueBox (kutu, icon = "fa-heartbeat", color = "forestgreen")
''

### Toplam Vefat Sayısı {.value-box}
`` {r}
kutu <- (korona $ doğrulanmış ölüm [uzunluk (korona $ onaylı ölüm [! is.na (korona $ onaylı ölüm)])])
valueBox (kutu, icon = "fa-frown-o", renk = "siyah")
''

### Aktif Vaka Sayısı {.value-box}
`` {r}
kutu <- (corona $ confirm_active [uzunluk (corona $ confirm_active [! is.na (corona $ confirm_active)])])
valueBox (kutu, icon = "fa-hospital-o", color = "# d8bfd8")
''

Dünya +
================================================ =====================

{.Tabset} Sütunu
-------------------------------------------------- ---------------------
### Karşılaştırma
`` {r}
plot_ly (data = tophastgr)%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum,
            ad = "Brezilya", satır = liste (genişlik = 2))%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum.yy,
                    isim = "Japonya", satır = liste (genişlik = 2))%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum.xx,
            isim = "Almanya", satır = liste (genişlik = 2))%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum.y,
            ad = "ABD", satır = liste (genişlik = 2))%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum.x,
            name = "Türkiye", satır = liste (genişlik = 2))%>%
  düzen (yaxis = liste (title = "Toplam Vaka Sayısı", tür = "günlük"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''


Türkiye
================================================ =====================

{.Tabset} Sütunu
-------------------------------------------------- ---------------------
### Günlük Vaka Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ onaylandı,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  düzen (yaxis = liste (title = "Günlük Vaka Sayısı", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Toplam Vaka Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  düzen (yaxis = liste (title = "Toplam Vaka Sayısı", tür = "günlük"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Günlük Vefat Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ ölüm,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  layout (title = "Günlük Vefat Sayısı Grafiği",
         yaxis = liste (title = "Günlük Vefat Sayısı", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Toplam Vefat Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ teyit edilen_ ölüm,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  düzen (title = "Toplam Vefat Sayısı Grafiği",
         yaxis = liste (title = "Toplam Vefat Sayısı", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Günlük İyileşen Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ kurtarıldı,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  layout (title = "Günlük İyileşen Sayısı Grafiği",
         yaxis = liste (title = "Günlük İyileşen Sayısı", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Toplam İyileşen Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ doğrulanmış_başvuru,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  layout (title = "Toplam İyileşen Sayısı Grafiği",
         yaxis = list (title = "Toplam İyileşen Sayısı Grafiği", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Aktif Vaka Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ onaylandı_aktif,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  düzen (title = "Aktif Vaka Sayısı Grafiği",
         yaxis = liste (title = "Aktif Vaka Sayısı", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''


### Covid 19 Vakalarını takip Dağılımı
`` {r}
coronavirus_Turkey%>%
  plot_ly (x = ~ tarih,
          y = ~ confirm_cum,
          name = 'Toplam Vaka Sayısı', 
          fillcolor = '# 7ec0ee',
          type = 'scatter',
          mode = 'yok', 
          stackgroup = 'bir')%>%
  add_trace (y = ~ onaylanmış_ ölüm, 
            name = "Toplam Vefat Sayısı",
            fillcolor = '# E41317')%>%
  add_trace (y = ~ confirm_recovered, 
            name = 'Toplam İyileşen Sayısı', 
            fillcolor = 'forestgreen')%>%
  düzen (açıklama = liste (x = 0.1, y = 0.9),
         yaxis = liste (title = "Toplam Vaka Sayısı"),
         xaxis = list (title = "Tarih"))
''


{Data-width = 100} Sütunu
-------------------------------------------------- ---------------------
### Toplam Vaka Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ confirm_cum [uzunluk (coronavirus_Turkey $ doğrulanmış_cum)]
valueBox (kutu, simge = "fa-artı-kare", renk = "# cd3333")
''

### Bugünkü Vaka Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ onaylandı [uzunluk (coronavirus_Turkey $ onaylandı)]
valueBox (kutu, simge = "fa-artı-kare", renk = "# cd3333")
''

### Dünkü Vaka Sayısı {.value-box}
`` {r}
kutu <-coronavirus_Turkey $ onaylandı [uzunluk (coronavirus_Turkey $ onaylandı) -1]
valueBox (kutu, simge = "fa-artı-kare", renk = "# cd3333")
''


### Toplam Vefat Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ onaylı_ ölüm [uzunluk (coronavirus_Turkey $ onaylı_ ölüm)]
valueBox (kutu, icon = "fa-frown-o", renk = "siyah")
''

### Aktif Vaka Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ onaylandı_aktif [uzunluk (coronavirus_Turkey $ onaylandı_aktif)]
valueBox (kutu, icon = "fa-hospital-o", color = "# d8bfd8")
''



{Data-width = 100} Sütunu
-------------------------------------------------- ---------------------

### Toplam İyileşen Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ confirm_recovered [uzunluk (coronavirus_Turkey $ resedipcovered)]
valueBox (kutu, icon = "fa-heartbeat", color = "forestgreen")
''

### Bugünkü İyileşen Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ onaylandı [uzunluk (coronavirus_Turkey $ onaylandı)]
valueBox (kutu, icon = "fa-heartbeat", color = "forestgreen")
''

### Dünkü İyileşen Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ onaylandı [uzunluk (coronavirus_Turkey $ onaylandı) -1]
valueBox (kutu, icon = "fa-heartbeat", color = "forestgreen")
''

### Bugünkü Vefat Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ ölüm [uzunluk (coronavirus_Turkey $ ölüm)]
valueBox (kutu, icon = "fa-frown-o", renk = "siyah")
''

### Dünkü Vefat Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ ölüm [uzunluk (coronavirus_Turkey $ ölüm) -1]
valueBox (kutu, icon = "fa-frown-o", renk = "siyah")
''


Türkiye (Oransal)
================================================ =====================

{Data-width = 100} Sütunu
-------------------------------------------------- ---------------------
### Bugünkü Vaka Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ onaylandı [uzunluk (coronavirus_Turkey $ onaylandı)]
valueBox (kutu, simge = "fa-artı-kare", renk = "# cd3333")
''

### Bugünkü İyileşen Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ onaylandı [uzunluk (coronavirus_Turkey $ onaylandı)]
valueBox (kutu, icon = "fa-heartbeat", color = "forestgreen")
''

### Bugünkü Vefat Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ ölüm [uzunluk (coronavirus_Turkey $ ölüm)]
valueBox (kutu, icon = "fa-frown-o", renk = "siyah")
''


{.Tabset} Sütunu
-------------------------------------------------- ---------------------

### Günlük İyileşen Faiz Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ kurtarılmış_ratio,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  layout (title = "Günlük İyileşen Oran Grafiği",
         yaxis = list (title = "Günlük İyileşen oranları", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Toplam İyileşen Oran Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_recovered_ratio,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  düzen (title = "Toplam İyileşen Oran Grafiği",
         yaxis = list (title = "Toplam İyileşen oran", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Günlük Vefat Rate Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ ölüm_ oranı,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  layout (title = "Günlük Vefat Hız Grafiği",
         yaxis = list (title = "Günlük Vefat Oranı", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

### Toplam Vefat Hız Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ doğrulanmış ölüm_ oranı,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  düzen (title = "Toplam Vefat Hız Grafiği",
         yaxis = list (title = "Toplam Vefat Rate", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''


{Data-width = 100} Sütunu
-------------------------------------------------- ---------------------
### Toplam Vaka Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ confirm_cum [uzunluk (coronavirus_Turkey $ doğrulanmış_cum)]
valueBox (kutu, simge = "fa-artı-kare", renk = "# cd3333")
''

### Toplam İyileşen Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ confirm_recovered [uzunluk (coronavirus_Turkey $ resedipcovered)]
valueBox (kutu, icon = "fa-heartbeat", color = "forestgreen")
''

### Toplam Vefat Sayısı {.value-box}
`` {r}
kutu <- coronavirus_Turkey $ onaylı_ ölüm [uzunluk (coronavirus_Turkey $ onaylı_ ölüm)]
valueBox (kutu, icon = "fa-frown-o", renk = "siyah")
''
