# Sources used by the automatic downloader

## GPR
Official Caldara-Iacoviello monthly data:
https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls

The official website states that GPRT is the Geopolitical Threats component
and GPRA is the Geopolitical Acts component. Monthly data are updated
regularly.

## Global real economic activity
FRED series IGREA, published by the Federal Reserve Bank of Dallas.
This is the updated Kilian global real economic activity index.

## Oil price
FRED DCOILWTICO (WTI). The script aggregates daily WTI to monthly and
deflates it by US CPI (CPIAUCSL) to create a real oil-price proxy.

## VIX
FRED VIXCLS.

## Country macro variables
World Bank WDI:
NY.GDP.MKTP.KD.ZG = GDP growth
FP.CPI.TOTL.ZG = inflation

## Stock indices
Yahoo Finance via quantmod. Tickers are explicitly stored in
data/raw/country_tickers.csv and failures are reported.

## NOT AUTOMATED YET
Global oil production, policy rates, exchange rates, and the structural
oil shocks SUP/DEM/PRE require source/definition choices that must match
the thesis. They are left NA rather than invented.
