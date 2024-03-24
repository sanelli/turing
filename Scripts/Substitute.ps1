param([string[]]$Languages = $("csharp","python"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet run --project ./Turing -c:Release --no-build toml "../samples/Substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("python" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Python ==="
    Push-Location ./python
    python3 ./turing.py toml "../samples/Substitute.tom" "abba"
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ($Success) {
    Write-Host "`nRun successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`nRun failed" -ForegroundColor:Red
    exit 1
}