param([string[]]$Languages = $("csharp"))

$Success = $true

if ("csharp" -in $Languages) {
    Write-Host -ForegroundColor:"Yellow" "=== C# ==="
    Push-Location ./csharp
    dotnet build ./Turing -c:Release
    $Success = $Success -and $?
    dotnet build ./Turing.Tests -c:Release
    $Success = $Success -and $?
    Pop-Location
}

if ($Success) {
    Write-Host "`n`nBuild successful!" -ForegroundColor:Green
    exit 0
}
else {
    Write-Host "`n`nBuild failed" -ForegroundColor:Red
    exit 1
}