param([string[]]$Languages = $("csharp"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet test ./Turing.Tests -c:Release
    $Success = $Success -and $?
    Pop-Location
}

if ($Success) {
    Write-Host "`n`nTest successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`n`nTest failed" -ForegroundColor:Red
    exit 1
}