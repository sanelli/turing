param([string[]]$Languages = $("csharp"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet run --project ./Turing -c:Release --no-build toml "../samples/Substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
}

if ($Success) {
    Write-Host "`n`nRun successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`n`nRun failed" -ForegroundColor:Red
    exit 1
}