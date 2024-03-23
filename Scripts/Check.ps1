param([string[]]$Languages = $("csharp"))

function Invoke-Checks() {
    $Success = $true

    if ("csharp" -in $Languages) {
        Write-Host -ForegroundColor:"Yellow" "=== C# ==="
        $Exists = Confirm-CommandExists "dotnet"
        $CorrectVersion = Confirm-DotnetVersion "8"
        $LocalSuccess = $Exists -and $CorrectVersion
        $Success = $Success -and $LocalSuccess
        Write-Succes $LocalSuccess "C#"
        if (-not $LocalSuccess)
        {
            Write-Host -ForegroundColor:"Red" "Visit https://dotnet.microsoft.com/en-us/download for installation instructions"
        }
    }

    if ($Success) {
        Write-Host "`n`nAll compilers available!" -ForegroundColor:Green
        exit 0
    }
    else {
        Write-Host "`n`nNot all compilers are available" -ForegroundColor:Red
        exit 1
    }
}

function Write-Succes([bool]$Success, [string]$Language) {
    if ($Success) {
        Write-Host "The required command(s) exist(s) for $Language" -ForegroundColor:Green
    }
    else {
        Write-Host "The required command(s) does/do NOT exist for $Language" -ForegroundColor:Red
    }
}

function Confirm-CommandExists($command) {
    Get-Command $command 2>&1 >$null
    if (-not $?) {
        Write-Host "Cannot find executable '$command'" -ForegroundColor:Red
        return $false
    }

    return $true
}

function Confirm-DotnetVersion($expectedVersion) {

    $sdks = dotnet --list-sdks

    foreach ($sdk in $sdks) {
        $major = $sdk.Split(".")[0]
        if ($major -eq $expectedVersion) {
            return $true
        }
    }
  
    Write-Host "Require dotnet version $expectedVersion but version $version found" -ForegroundColor:Red
    return $false;
}

Invoke-Checks