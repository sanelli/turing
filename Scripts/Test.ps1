param([string[]]$Languages = $("csharp", "python"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet test ./Turing.Tests -c:Release
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ("python" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== Python ==="
    Push-Location ./python
    python3 test.py
    $Success = $Success -and $?
    Pop-Location
    Write-Host ""
}

if ($Success) {
    Write-Host "`nTest successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`nTest failed" -ForegroundColor:Red
    exit 1
}