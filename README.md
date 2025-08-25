# Diplomski rad – Analiza kvalitete zraka (R vs. Python)

Ovaj repozitorij sadrži implementaciju diplomskog rada na temu **"Razvoj i usporedba web aplikacija za obradu podataka u programskim jezicima R i Python"**.

## Struktura repozitorija

### Branch `main`
- Implementacija u **R/Shiny**  
- Kod se nalazi u folderu [`kvaliteta_zraka/`](./kvaliteta_zraka)  
- Funkcionalnosti:
  - Dohvat real-time AQI podataka putem WAQI API-ja
  - Vizualizacije povijesnih podataka (mjesečni i godišnji trendovi, sezonski prikaz, korelacije)
  - Interaktivni prikaz na karti
  - Predviđanja pomoću ARIMA/ETS/SNaive i Random Forest modela

### Branch `python`
- Implementacija u **Python/Dash**  
- Python kod se nalazi u folderu [`python_app/`](./python_app)  
- Nasljeđuje sve datoteke iz `main` grane (R kod ostaje prisutan), ali dodaje i Dash aplikaciju.  
- Funkcionalnosti (u izradi):
  - Replika glavnih funkcionalnosti iz R verzije
  - Vizualizacije pomoću `plotly` i `dash_table`
  - Predviđanja koristeći `statsmodels` i `scikit-learn`

## Podaci
- `gradovi_drzave.xlsx` – povezivanje gradova i država
- `podaci.xlsx`, `podaciv1.xlsx`, `podaciv2.xlsx` – povijesni skupovi s mjesečnim agregatima
