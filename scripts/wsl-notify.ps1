# $Data is supplied as base64-encoded UTF-8 JSON by mu4e-wsl-notify.el.
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$null = [Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime]
$null = [Windows.Data.Xml.Dom.XmlDocument, Windows.Data.Xml.Dom.XmlDocument, ContentType = WindowsRuntime]

# Reuse WSLg's registered Emacs application identity when available.
$app = Get-StartApps | Where-Object { $_.Name -eq "Emacs ($($Data.distro))" } | Select-Object -First 1
$appId = if ($app) { $app.AppID } else {
    '{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}\WindowsPowerShell\v1.0\powershell.exe'
}

$xml = New-Object Windows.Data.Xml.Dom.XmlDocument
$xml.LoadXml('<toast><visual><binding template="ToastGeneric"><text/><text/></binding></visual><audio src="ms-winsoundevent:Notification.Mail"/></toast>')
$text = $xml.GetElementsByTagName('text')
$null = $text.Item(0).AppendChild($xml.CreateTextNode([string]$Data.title))
$null = $text.Item(1).AppendChild($xml.CreateTextNode([string]$Data.body))

$toast = [Windows.UI.Notifications.ToastNotification]::new($xml)
$toast.Tag = 'mu4e-mail'
$toast.Group = 'mu4e'
$notifier = [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier([string]$appId)
$notifier.Show($toast)
Write-Output "Toast submitted: $appId"
