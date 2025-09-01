Ovaj repozitorij sadrži implementaciju diplomskog rada 
- Implementacija u **R/Shiny**  
- Kod se nalazi u folderu [`kvaliteta_zraka/`](./kvaliteta_zraka)  
- Funkcionalnosti:
  - Dohvat real-time AQI podataka putem WAQI API-ja
  - Vizualizacije povijesnih podataka (mjesečni i godišnji trendovi, sezonski prikaz, korelacije)
  - Interaktivni prikaz na karti
  - Predviđanja pomoću ARIMA/ETS/SNaive i Random Forest modela

## Podaci
- `gradovi_drzave.xlsx` – povezivanje gradova i država
- `podaci.xlsx`, `podaciv1.xlsx`, `podaciv2.xlsx` – povijesni skupovi s mjesečnim agregatima
