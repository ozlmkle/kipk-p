# kipk-p
kütüphane (flexdashboard)
kütüphane (readr)
kütüphane (ggplot2)
kütüphane (dplyr)
kütüphane (tidyr)
kütüphane (arsa)
kütüphane (highcharter)
''

read.csv programının ile github üzerinden verimizi çekip coronavirus olarak atama atıyoruz. R'ın karakteri olan değişkenleri faktör olarak tanımlamaması için stringsAsFactors'e FALSE olarak atama yapıyoruz.
`` {r}
koronavirüs <- read.csv ("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", stringsAsFactors = FALSE)
''


pivot_wider parası ile veride para cezası üzerinden para cezası kesiyoruz. Buradaki amacımız tip değişkenini ayırmaktır. Devamında değişkenlerde Na olarak tanımlı olana 0 olarak atıyoruz.
`` {r}
korona <- koronavirüs%>% 
 pivot_wider (names_from = tür, değerler_from = vakalar)
corona $ onaylandı [is.na (corona $ onaylandı)] = 0
corona $ ölüm [is.na (corona $ ölüm)] = 0
korona $ kurtarıldı [is.na (korona $ kurtarıldı)] = 0
''

filtre fonksiyonu ile verimizden ülke olarak Türkiye'yi seçip "coronavirus_Turkey" değişkenini oluşturuyoruz.
`` {r}
coronavirus_Turkey <- corona%>%
  filtre (ülke == "Türkiye")
''

coronavirus_Turkey değişkenine mutate fonksiyonu ile yeni değişkenler ekliyoruz. Yeni eklenen değişkenler vaka sayılarının, vefat sayılarının, iyileşen ve aktif vaka sayılarının kümülatif olarak toplanması ile oluşturulmuştur.
`` {r}
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (confirmation_cum = cumsum (onaylandı))%>%
  mutate (confirmation_death = cumsum (ölüm))%>%
  mutate (confirm_recovered = cumsum (kurtarıldı))%>%
  mutate (onaylanmış_aktif = cumsum (onaylanmış) -cumsum (ölüm) -cumsum (geri kazanılmış))
''

coronavirus_Turkey değişkeni üzerinde yaptığımız değişken eklemeleri tüm ülkeleri içeren corona değişkenine de uygulanmıştır.
`` {r}
korona <- korona%>%
  mutate (confirmation_cum = cumsum (onaylandı))%>%
  mutate (confirmation_death = cumsum (ölüm))%>%
  mutate (confirm_recovered = cumsum (kurtarıldı))%>%
  mutate (onaylanmış_aktif = cumsum (onaylanmış) -cumsum (ölüm) -cumsum (geri kazanılmış))
''

Dünya

group_by ile ülkelere göre gruplandırma yapıyoruz. Sonra özetleyin bu ile birlikte toplam vaka sayılarını gruplandırıyoruz. Düzenleme görevi ile sıralama yapıyoruz. Fakat sıralama artan yönündedir. desc fonksiyonu ile sıralandı azalana doğru yapabiliriz. Elde ettiğimiz veriyi değişkenine atıyoruz. Daha sonra highcharter paketini kullanrak elde ettiğimiz "Dünyada En Yüksek Toplam Vaka Sayısına Sahip 10 Ülke" verisinin grafiğini çizdiriyoruz.

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

group_by fonksiyonu ile ülke ve tarihe göre gruplandırma yapıyoruz. Ardından özetle ile ülkelere toplam vaka sayılarını gruplandırıyoruz. pivot_wider komutunu ile de type değişkenini ayırıyoruz. düzenlemek ile tarihe göre sütunları düzenleriz. Arından mutate fonksiyonu ile toplam vaka sayısı, toplam iyileşen sayısı ve toplam vefat sayısı değişkenlerini elde ederiz. Daha sonra plotly paketi ile gerekli değerler girilerek grafiğimiz elde edilir.

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


Verimizden ülke seçimleri yapıp atama yapıyoruz. Daha sonra bu ülkelerin toplam vaka sayılarını yeni değişken olarak ekliyoruz. Seçilen ülkeler ABD, Almanya, Japonya ve Brazilya'dır.
`` {r}
koronavirüs_US <- korona%>%
  filtre (ülke == "ABD")
koronavirüs_US <- koronavirüs_US%>%
  mutate (confirm_cum = cumsum (onaylandı))
coronavirus_Germany <- korona%>%
  filtre (ülke == "Almanya")
coronavirus_Germany <- coronavirus_Germany%>%
  mutate (confirm_cum = cumsum (onaylandı))
coronavirus_Japan <- corona%>%
  filtre (ülke == "Japonya")
coronavirus_Japan <- coronavirus_Japan%>%
  mutate (confirm_cum = cumsum (onaylandı))
coronavirus_Brazil <- korona%>%
  filtre (ülke == "Brezilya")
coronavirus_Brazil <- coronavirus_Brazil%>%
  mutate (confirm_cum = cumsum (onaylandı))
''

left_join fonksiyonu ile "coronavirus_Turkey" diğer çağda değişken olarak ekliyoruz.
`` {r}
tophastgr <- coronavirus_Turkey%>% 
  left_join (coronavirus_US, by = "tarih")%>%
  left_join (coronavirus_Germany, by = "tarih")%>%
  left_join (coronavirus_Japan, yazan = "tarih")%>%
  left_join (coronavirus_Brazil, = "tarih" ile)
''

plotly paketi ile gerekli atamalar yapılarak "tophastgr" verimizin logaritmik olarak çizgi grafiğini oluşturuyoruz. Elde ettiğimiz grafik ile ülkeler arasında toplam vaka sayılarının karşılaştırmasını yapabiliriz.
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



"Dünyada En Yüksek Toplam Vaka Sayısına Sahip 10 Ülke" nin plotly paketinden yararlanarak bu kez pasta grafiğini çizdiriyoruz.
`` {r}
ry%>%
  düzenle (azalan (onaylandı))%>%
  kafa (10)%>%
  plot_ly (etiketler = ~ ülke, değerler = ~ onaylandı,
          textinfo = "etiket + yüzde",
          tür = 'pasta')
''


Türkiye

"Coronavirus_Turkey" verisi ile Türkiye için oluşturulacaktır.

Toplam Vaka Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_cum,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  düzen (yaxis = list (title = "Toplam Vaka Sayısı Grafiği", tür = "günlük"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

Toplam Vaka Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
                    y = ~ confirm_cum,
                    isim = "Türkiye", satır = liste (genişlik = 3))%>%
  düzen (title = "Toplam Vaka Sayısı Grafiği",
         yaxis = liste (title = "Toplam Vaka Sayısı", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

Günlük Vaka Sayısı Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ onaylandı,
            isim = "Türkiye", satır = liste (genişlik = 3))%>%
  düzen (yaxis = liste (title = "Günlük Vaka Sayısı", tür = "dağılım"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''

Günlük Vefat Sayısı Grafiği
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

Toplam Vefat Sayısı Grafiği
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

Günlük İyileşen Sayısı Grafiği
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

Toplam İyileşen Sayısı Grafiği
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

Aktif Vaka Sayısı Grafiği
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

Günlük Vefat Hız Grafiği - (Günlük Vefat Oranı = Günlük Vefat Sayısı / Günlük Vaka Sayısı)
`` {r}
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (death_ratio = (ölüm / onaylandı) * 100)
''

Günlük Vefat Hız Grafiği
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

Toplam Vefat Rate Grafiği - (Toplam Vefat Rate = Toplam Vefat Sayısı / Toplam Vaka Sayısı)
`` {r}
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (confirmation_death_ratio = (confirm_death / confirm_cum) * 100)
''

Toplam Vefat Hız Grafiği
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

Günlük İyileşen Faiz Grafiği - (Günlük İyileşen Faiz = Günlük İyileşen Sayısı / Günlük Vaka Sayısı)
`` {r}
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (recoveryed_ratio = (kurtarıldı / onaylandı) * 100)
''

Günlük İyileşen Oran Grafiği
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

Toplam İyileşen Oranı Grafiği - (Toplam İyileşen oranları = Toplam İyileşen Sayısı / Toplam Vaka Sayısı)
`` {r}
coronavirus_Turkey <- coronavirus_Turkey%>%
  mutate (confirmation_recovered_ratio = (confirm_recovered / confirm_cum) * 100)
''

Toplam İyileşen Hız Grafiği
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

Günlük ve Toplam İyileşen Oran Grafiği
`` {r}
plot_ly (data = coronavirus_Turkey)%>%
  add_lines (x = ~ tarih,
            y = ~ kurtarılmış_ratio,
            name = "Günlük İyileşen Oranı", line = liste (genişlik = 2))%>%
  add_lines (x = ~ tarih,
            y = ~ confirm_recovered_ratio,
            name = "Toplam İyileşen Oranı", line = liste (genişlik = 2))%>%
  layout (yaxis = list (title = "Günlük ve Toplam İyileşen Oran Grafiği", tür = "log"),
         xaxis = list (title = "Tarih"),
         açıklama = liste (x = 0,7, y = 0,3))
''



Covid 19 Vakalarının yerine Dağılımı Grafiği
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
