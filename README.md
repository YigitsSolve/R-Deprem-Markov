Rastgele Deprem Analizi Simülasyonu
Bu R kodu, rastgele deprem verileri oluşturur ve bu veriler üzerinde bir analiz gerçekleştirir. Kod, belirli bir zaman aralığında belirli bir adımda deprem oluşumlarını izler ve bu depremlerin bölgesel dağılımını analiz eder. Sonuç olarak, bir deprem analizi matrisi oluşturur ve bu matrisi ısı haritası olarak görselleştirir.

Nasıl Kullanılır?
1. Kodun Çalıştırılması: R programını çalıştırın ve kodu bir R dosyasına kopyalayın veya bir R ortamında doğrudan çalıştırın.
2. Gerekli Kütüphaneler: Kod, tidyverse kütüphanesini kullanır. Bu yüzden kodu çalıştırmadan önce bu kütüphaneyi yüklemek gerekebilir. Eğer bu kütüphaneyi yüklememişseniz, aşağıdaki komutu kullanarak yükleyebilirsiniz.
install.packages("tidyverse")
library(tidyverse)
3. Kodun Anlaşılması: Kodun yürütülmesi sonucunda oluşturulan deprem analizi matrisi, depremlerin belirli bölgeler arasındaki ilişkilerini gösterir. Isı haritası, bu ilişkileri görselleştirmek için kullanılır.

Parametreler

StepSize: Bu değişken, analizin hangi aralıklarla yapıldığını belirler. Örneğin, StepSize 0.1 ise, her 0.1 birimde bir analiz yapılır. StepSize En yüksek entropi olarak Seçilmeli
