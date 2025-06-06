require(keyring)
login_to_wikibase <-function(instance, username) {
  get_csrf(username,
           password=key_get(instance, keyring = "reprexbase"),
           wikibase_api_url = sprintf(
             "https://reprexbase.eu/%s/api.php", instance)
           )

}


