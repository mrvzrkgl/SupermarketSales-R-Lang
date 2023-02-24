
### Supermarket Sales (Süpermarket Satışları)


## Giriş

# Supermarket sales veri seti, bir süpermarketin 3 farklı şubesinin satış 
# bilgilerini içermektedir.
# Veri seti değişkenleri tanıtılarak sırasıyla kategorik değişken analizleri,
# satış analizleri ve müşteri davranışı analizleri yapılacaktır. 

# Veri seti 1000 gözlemden ve 17 değişkenden oluşmaktadır. Ayrıca kayıp değer içermemektedir.


# KAYNAK: <https://www.kaggle.com/datasets/aungpyaeap/supermarket-sales> 
  

  
## Veri seti değişkenleri
  
# 1. Invoice id: Fatura ID
# 2. Branch: Süpermarket şubesi (A, B, C)
# 3. City: Süpermarketin bulunduğu şehir
# 4. Customer type: Müşteri türü
#   * Member = Üye kartı olanlar
#   * Normal = Üye kartı olmayanlar
# 5. Gender: Cinsiyet
#   * Female = Kadın 
#   * Male = Erkek
# 6. Product line: Ürün grupları 
#   * Electronic accessories = Elektronik aksesuarlar 
#   * Fashion accessories = Moda aksesuarları
#   * Food and beverages = Yiyecek ve içecekler
#   * Health and beauty = Sağlık ve güzellik
#   * Home and lifestyle = Ev ve yaşam tarzı 
#   * Sports and travel = Spor ve seyahat
# 7. Unit price: Ürünlerin $ cinsinden birim fiyatı
# 8. Quantity: Müşteri tarafından satın alınan ürün sayısı
# 9. Tax: Müşteri alımları için %5 vergi ücreti
# 10. Total: Vergi dahil toplam fiyat
# 11. Date: Satın alma tarihi (Ocak 2019 - Mart 2019)
# 12. Time: Satın alma zamanı (10.00 - 21.00)
# 13. Payment: Ödeme yöntemi 
#   * Cash = Nakit
#   * Credit card = Kredi kartı
#   * Ewallet = E-cüzdan
# 14. COGS: Satılan malların maliyeti
# 15. Gross margin percentage: Brüt kar marjı yüzdesi
# 16. Gross income: Brüt gelir
# 17. Rating: Alışveriş deneyimine ilişkin müşteri derecelendirmesi (1 - 10 arası)



# Verimizi import edelim.

library(readxl)
getwd()
setwd("C:/Users/zrkgl/Desktop/project")
sales <- read_xlsx("supermarket_sales.xlsx")
View(sales)



# Time değişkeni için zaman formatını düzenleyelim.

library(tidyverse)
sales <- sales %>%
  mutate(
    Time = format(Time, "%H:%M:%S"))
View(sales)



# Kayıp gözlem kontrolü yapalım.

table(is.na(sales))


# Kaç gözlem olduğuna bakalım.

nrow(sales)


# Toplamda 1000 kişinin kaydı tutulmuştur.




## Kategorik Değişken Analizleri

# Branch , city, customer type, gender, product line, payment değişkenleri;
# kategorik değişkenlerdir.
# Kategorik değişkenleri inceleyelim.



## Şubelere Talep Sayısı

# Branch; 

table(sales$Branch)


# A şubesinden 340 , B şubesinden 332 ve C şubesinden 328 kişi alışveriş yapmıştır. 
# Şubelerden alışveriş yapan kişi sayıları birbirine oldukça yakındır. 


ggplot(sales, aes(x = Branch)) +
  geom_bar(aes(fill = Branch), stat = "count") +
  ggtitle("Şubelere Talep Sayısı") +
  xlab("Şubeler") +
  ylab("Şubelere Talep Sayısı") +
  scale_fill_manual(values = c("khaki" , "rosybrown", "lightsalmon"), name = "Şubelere Talep Sayısı") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10 , face = "bold"),
        axis.text.y = element_text(size = 10 , face = "bold"))


## Şehirlere Talep Sayısı

# City;

table(sales$City)

# A şubesi Yangon'da, B şubesi Mandalay'da ve C şubesi ise Naypyitaw'da bulunmaktadır.



ggplot(sales, aes(x = City)) +
  geom_bar(aes(fill = City), stat = "count") +
  ggtitle("Şehirlere Talep Sayısı") +  
  xlab("Şehirler") +
  ylab("Satış Sayısı") +
  scale_fill_manual(values = c("khaki" , "rosybrown", "lightsalmon"), "Şehirler") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10 , face = "bold"),
        axis.text.y = element_text(size = 10 , face = "bold"))




## Müşteri Türü

# Customer type;

table(sales$`Customer type`)

# Alışveriş yapanların 501'i üye olan, 499'u ise üye olmayan müşteridir.


ggplot(sales, aes(x = `Customer type`)) +
  geom_bar(aes(fill = `Customer type`), stat = "count") +
  ggtitle("Müşteri Türü Dağılımı") +
  xlab("Müşteri Türü") +
  ylab("Kişi Sayısı") +
  scale_fill_manual(values = c("lightblue", "lightsalmon"),name = "Müşteri Türü") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10 , face = "bold"),
        axis.text.y = element_text(size = 10 , face = "bold"))


## Cinsiyet Dağılımı

# Gender;

table(sales$Gender)


# Alışveriş yapanların 501'i kadın ve 499'u erkektir.


ggplot(sales, aes(x = Gender)) +
  geom_bar(aes(fill = Gender), stat = "count") +
  ggtitle("Cinsiyet Dağılımı") +
  xlab("Cinsiyet") +
  ylab("Kişi Sayısı") +
  scale_fill_manual(values = c("lightgreen", "lightsalmon"),name = "Gender") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text( size = 10 , face = "bold"),
        axis.text.y = element_text( size = 10 , face = "bold"))


## Ürün Kategorileri

# Product line;

table(sales$`Product line`)


# Elektronik aksesuarlar için 170, Moda aksesuarları için 178 , 
# Yiyecek ve içecekler için 174, Sağlık ve güzellik için 152, 
# Ev ve yaşam tarzı için 160, Spor ve seyahat için 166 satış gerçekleşmiştir.


sales %>%
  ggplot(aes(x=`Product line`)) +
  geom_bar(fill = c("powderblue" , "rosybrown2" , "palegreen" , "mediumorchid2" , "lightgoldenrod" , "lightsalmon")) +
  ggtitle("Ürün Kategorileri") +
  xlab("Kategoriler") +
  ylab("Satış Sayıları") +
  scale_fill_discrete(name = "Kategoriler") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45 , size = 10 , face = "bold"),
        axis.text.y = element_text(angle = 0 , size = 10 , face = "bold"))


## Ödeme Yöntemleri

# Payment;

table(sales$Payment)


# 344 nakit ödeme, 311 kredi kartı ödemesi ve 345 e-cüzdan ödemesi alınmıştır.

ggplot(sales, aes(x = Payment)) +
  geom_bar(stat = "count", fill = c("khaki" , "rosybrown", "lightsalmon")) +
  ggtitle("Ödeme Yöntemi Dağılımı") +
  xlab("Ödeme Yöntemi") +
  ylab("Kişi Sayısı") +
  scale_fill_discrete(name = "Ödeme Yöntemi") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10 , face = "bold"),
        axis.text.y = element_text(size = 10 , face = "bold"))



## Satış Analizleri


## Ürün Satış Analizi

Şubelere göre en çok satılan ürün kategorilerini belirleyelim


# Verileri Branch ve Product line değişkenlerine göre gruplandıralım
grouped_data <- sales %>%
  group_by(Branch, `Product line`) %>%
  summarize(quantity = sum(Quantity)) %>%
  arrange(desc(quantity))

# Her Branch için en çok satılan ürün gruplarını belirleyelim
top_product_lines <- grouped_data %>%
  group_by(Branch) %>%
  top_n(1, quantity)

top_product_lines


# A şubesi için 371 ürün ile Home and lifestyle grubu
# B	şubesi için 322 ürün ile Sports and travel grubu
# C	şubesi için 369 ürün ile Food and beverages	grubu en çok satılan kategoridir.


# Her şube için en çok satılan ürün kategorilerinin satışlarını görselleştirelim.

top_product_lines$Branch <- factor(top_product_lines$Branch, 
                                  levels = c("A", "B", "C"),
                                  labels = c("Home and lifestyle", "Sports and travel", "Food and beverages"))

ggplot(top_product_lines, aes(x = reorder(Branch, -quantity), y = quantity, fill = `Product line`)) +
  geom_bar(stat = "identity") +
  labs(x = "Şubeler", y = "Satış Miktarı", fill = "Branch") +
  ggtitle("Şubelere Göre En Çok Satan Ürün Kategorileri") +
  scale_fill_manual(values = c("powderblue" , "rosybrown2" ,"lightsalmon")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_text(angle = 0, hjust = 1, size = 10, face = "bold"))



## Satış Miktarı Analizi

# Her bir şubedeki ürün satış miktarlarını karşılaştıralım.


# Her şube için toplam satın alınan ürün sayısını hesaplayalım
branch_sales <- aggregate(Quantity ~ Branch, data = sales, sum)
branch_sales


# Yeniden sıralayalım
branch_sales <- branch_sales[order(-branch_sales$Quantity), ]

# Yüzdeleri bulalım
branch_sales$Percentage <- round(100 * branch_sales$Quantity / sum(branch_sales$Quantity), 2)

# Pasta grafiği çizelim
library(ggplot2)
ggplot(branch_sales, aes(x = 1, y = Quantity, fill = Branch)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(x = "", y = "", fill = "Branch") +
  ggtitle("Şubelerin Satış Yüzdeleri") +
  scale_fill_manual(values = c("powderblue" , "rosybrown2" ,"lightsalmon")) +
  theme_void() +
  geom_text(aes(label = paste0(Branch, "\n", Percentage, "%")), position = position_stack(vjust = 0.5))




## Şubelere Göre Zamana Bağlı Satış Analizi

# Date değişkeninin tarih formatında olduğundan emin olalım
class(sales$Date)

library(lubridate)
sales$Date <- ymd(sales$Date)
class(sales$Date)


# Şubelerin satışlarını aylara göre gruplandıralım
sales %>%
  group_by(month = format(Date, "%Y-%m"), Branch) %>%
  summarise(mean = mean(Total))


# Görselleştirme yapalım
ggplot(data = sales, aes(x = as.Date(Date, format = "%Y-%m-%d"), y = Total, fill = Branch)) +
  geom_area(position = "stack") +
  ggtitle("Şubelere Göre Zamana Bağlı Satış") +
  xlab("Aylar") +
  ylab("Satışlar") +
  scale_fill_manual(values = c("sandybrown", "saddlebrown", "papayawhip")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))


## Müşteri Davranış Analizleri

## Müşteri Davranışları ve Satış Analizi

# Müşterilerin satın alma alışkanlıklarını ve satışlar üzerindeki etkilerini inceleyelim.


# Müşteri başına satış grafiği;

sales %>%
  group_by(`Invoice ID`, Date) %>%
  summarise(total_sale = sum(Total),
            total_items = n()) %>%
  ggplot(aes(x = Date, y = total_sale)) +
  geom_line(color = "dodgerblue4") +
  ggtitle("Müşteri Başına Tarihsel Satış Tutarı") +
  xlab("Tarih") +
  ylab("Satış Tutarı") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))



# Satın alınan ürün grafiği;


sales %>%
  group_by('Invoice ID', `Product line`) %>%
  summarise(total_sale = sum(Total),
            total_items = n()) %>%
  ggplot(aes(x = `Product line`, y = total_sale, fill = `Product line`)) +
  geom_bar(stat = "identity") +
  ggtitle("Satın Alınan Ürün Kategorileri") +
  xlab("Ürün Kategorisi") +
  ylab("Satış Tutarı") +
  scale_fill_discrete(name = "Ürün Kategorisi") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 50 , size = 10 , face = "bold"),
        axis.text.y = element_text(angle = 0 , size = 10 , face = "bold"))



## Güne Bağlı Satış Analizi


# Hangi günlerde daha fazla satış yapıldığını inceleyelim.

# Tarih sütununu tarih formatına dönüştürelim
sales$Date <- as.Date(sales$Date, "%m/%d/%Y")

# Tarih sütunundan gün adlarını alıp "Day" isimli yeni bir sütuna ekleyelim
sales$Day <- weekdays(sales$Date)

# Toplam satışları gün adına göre toplayalım
sales_by_day <- sales %>%
  group_by(Day) %>%
  summarise(total_sales = sum(Total))

# Çizgi grafiği oluşturalım
ggplot(sales_by_day, aes(x = Day, y = total_sales, group = 1)) +
  geom_line(color = "orange", size = 1) +
  labs(title = "Günlere Göre Satış Miktarı", x = "Günler", y = "Satış Miktarları") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0 , size = 10 , face = "bold"),
        axis.text.y = element_text(angle = 0 , size = 10 , face = "bold"))


# Grafikten hareketle en çok satışın Cumartesi, en az satışın ise Pazartesi
# günlerinin gerçekleştiğini söyleyebiliriz.




## Saate Bağlı Satış Analizi


# Hangi saatlerde daha fazla satış yapıldığını inceleyelim.



# Satın alma saatlerini "hour" adında yeni bir sütuna ekleyelim
sales$hour <- format(as.POSIXct(sales$Time, format = "%H:%M:%S"), format = "%H")

# Satın alma saatlerine göre toplam satışları hesaplayalım
sales_by_hour <- sales %>%
  group_by(hour) %>%
  summarise(total_sales = sum(Total))

# Çizgi grafiği oluşturalım
ggplot(sales_by_hour, aes(x = hour, y = total_sales, group = 1)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Saatlere Göre Satış Miktarı", x = "Saatler", y = "Satış Miktarı") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0 , size = 10 , face = "bold"),
        axis.text.y = element_text(angle = 0 , size = 10 , face = "bold"))



# Grafikten hareketle en çok satışın saat 19:00'da, en az satışın ise saat 20:00'da yapıldığını söyleyebiliriz.





