$ErrorActionPreference = 'Stop'

# Найдём путь к скрипту
$scriptName = Split-Path -Leaf $MyInvocation.MyCommand.Path
$scriptPath = Get-ChildItem -Recurse -Filter $scriptName -File -ErrorAction SilentlyContinue |
  Select-Object -First 1

if (-not $scriptPath) {
    Write-Error "Error: script '$scriptName' not found in current directory tree."
    exit 1
}

$sourceDir = Resolve-Path -Path (Join-Path $scriptPath.Directory.FullName "..")
$emacsConfigPath = Join-Path $env:APPDATA ".emacs.d"
$userConfigPath = Join-Path $emacsConfigPath "user-config"
$sourceTemplates = Join-Path $sourceDir "templates"
$sourcePackage = Join-Path $sourceDir "package"

# Создаём директории
New-Item -ItemType Directory -Force -Path $userConfigPath | Out-Null
New-Item -ItemType Directory -Force -Path $emacsConfigPath | Out-Null

# Синхронизация каталога package
Robocopy $sourcePackage $userConfigPath /MIR /NFL /NDL /NJH /NJS /NP | Out-Null

# Копирование шаблонов с отбросом .template
$templateSuffix = ".template"
Get-ChildItem "$sourceTemplates\*$templateSuffix" -File | ForEach-Object {
    $destName = $_.BaseName
    $destPath = Join-Path $emacsConfigPath $destName
    Copy-Item $_.FullName $destPath -Force
    Write-Host "Copied: $($_.Name) → $destName"
}

Write-Host "Host config updated successfully."
