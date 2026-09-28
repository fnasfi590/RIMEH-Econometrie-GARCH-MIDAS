$url="https://www.stat-search.boj.or.jp/ssi/mtshtml/fm02_m_1_en.html"
try {
    $r=Invoke-WebRequest -Uri $url -UseBasicParsing
    Write-Host "HTTP:" $r.StatusCode
    $r.Content | Select-String -Pattern "2000/|2001/|2002/|2003/|2004/|2005/|2006/|2013/|2014/|2015/|2016/" | Select-Object -First 40
}
catch {
    Write-Host "ERREUR:" $_
}
