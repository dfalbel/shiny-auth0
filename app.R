library(shiny)
library(httr)

# OAuth setup --------------------------------------------------------

# Most OAuth applications require that you redirect to a fixed and known
# set of URLs. Many only allow you to redirect to a single URL: if this
# is the case for, you'll need to create an app for testing with a localhost
# url, and an app for your deployed app.

if (interactive()) {
    # testing url
    options(shiny.port = 8100)
    APP_URL <- "http://localhost:8100/"
} else {
    # deployed URL
    APP_URL <- "https://servername/path-to-app"
}

# Note that secret is not really secret, and it's fine to include inline
app <- oauth_app(
    "shiny-auth0",
    key = "{your Client ID}",
    secret = "{your Client Secret}",
    redirect_uri = APP_URL
)

# Here I'm using a canned endpoint, but you can create with oauth_endpoint()
api <- oauth_endpoint(
    base_url = "https://dfalbel.auth0.com/",
    request = "oauth/token", 
    authorize = "authorize",
    access = "oauth/token"
)

# Always request the minimal scope needed. For github, an empty scope
# gives read-only access to public info
scope <- "openid profile"

# Shiny -------------------------------------------------------------------

has_auth_code <- function(params) {
    # params is a list object containing the parsed URL parameters. Return TRUE if
    # based on these parameters, it looks like auth codes are present that we can
    # use to get an access token. If not, it means we need to go through the OAuth
    # flow.
    return(!is.null(params$code))
}

ui <- fluidPage(
    # Your regular UI goes here, for when everything is properly auth'd
    verbatimTextOutput("code")
)

# A little-known feature of Shiny is that the UI can be a function, not just
# objects. You can use this to dynamically render the UI based on the request.
# We're going to pass this uiFunc, not ui, to shinyApp(). If you're using
# ui.R/server.R style files, that's fine too--just make this function the last
# expression in your ui.R file.
uiFunc <- function(req) {
    if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
        url <- oauth2.0_authorize_url(api, app, scope = scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
    } else {
        ui
    }
}

server <- function(input, output, session) {
    params <- parseQueryString(isolate(session$clientData$url_search))
    if (!has_auth_code(params)) {
        return()
    }
    
    # Manually create a token
    token <- oauth2.0_token(
        app = app,
        endpoint = api,
        credentials = oauth2.0_access_token(api, app, params$code),
        cache = FALSE,
        user_params = list(grant_type = "authorization_code")
    )
    
    resp <- GET("https://dfalbel.auth0.com/userinfo", config(token = token))
    # TODO: check for success/failure here
    
    print(content(resp, "parsed"))
    
    output$code <- renderPrint(content(resp, "text"))
}

# Note that we're using uiFunc, not ui!
shinyApp(uiFunc, server)