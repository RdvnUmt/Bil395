Bu proje, Lex ve Yacc kullanarak basit bir hesap makinesi uygulamasıdır. Temel aritmetik işlemleri (+, -, *, /) ve parantezleri destekler. Ayrıca ondalıklı sayıları ve üs alma işlemini de içerir.

Standart operatör önceliği uygulandı (^, sonra *, /, sonra +, -).

1. Lex Dosyası Hazırlama:
   - Sayılar operatörler ve parantezler için pattern tanımlandı
   - Parser için token üretimi ayarlandı

2. Yacc Dosyası Hazırlama:
   - Aritmetik ifadeler için gramer kuralları tanımlandı
   - İfadeleri değerlendirmek kurallar eklendi
   - Operatör öncelik kuralları eklendi
   - Sıfıra bölme hatası için kontrol eklendi

Test Cases:
3 + 5
5 - 2
7 - 9
10 * 4
(1 + 2) * 4
10 / (2 + 3)
(3 + 5) * (2 - 1) / 4
8 / 0

3.5 + 2.5
1.5 * 2.5
2^3
2**3
2.5^2

Nasıl derlenir ve çalıştırılır?

lex calculator.l        # lex.yy.c dosyasını üretir
yacc -d calculator.y    # Yacc dosyasını işleyerek y.tab.c ve y.tab.h dosyalarını üretir
gcc lex.yy.c y.tab.c -o calculator -lm #  C kodlarını derleyip çalıştırılabilir dosya oluşturur. Buradaki lm önemlidir ve pow() fonksiyonunun çalışmasını sağlar.
./calculator            # Programı çalıştır
