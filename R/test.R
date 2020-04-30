# Below are mock secret and key, they will not work. They are just an example of what they
#  would look like. Replace with your.
library(httr)
switchdrive <- oauth_app("SwitchDrive", key = "jana.jarecki@unibas.ch", secret = "ODPNF-YHJSH-FQJVW-QRJPB")
endp <- oauth_endpoint(authorize = NULL, access = "https://drive.switch.ch")

oauth2.0_token(endpoint = endp, app = switchdrive)

https://drive.switch.ch/index.php/f/2266937603

# 1. Find OAuth settings for reddit:
#    https://github.com/reddit/reddit/wiki/OAuth2
switchdrive <- oauth_endpoint(
  authorize = "https://www.reddit.com/api/v1/authorize",
  access = "https://www.reddit.com/api/v1/access_token"
)

# 2. Register an application at https://www.reddit.com/prefs/apps
#    Make sure to register http://localhost:1410/ as the "redirect uri".
#    (the trailing slash is important!)
app <- oauth_app(name = "switchdrive", key = "jana.jarecki@unibas.ch", secret = "ODPNF-YHJSH-FQJVW-QRJPB")

# 3. Get OAuth credentials
token <- oauth2.0_token(switchdrive, app,
  scope = c("read", "modposts"),
  use_basic_auth = TRUE
)