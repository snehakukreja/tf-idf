
require(devtools)
install_github('joyofdata/RTwitterAPI')
require(RTwitterAPI)

arams <- c(
  'oath_consumer_key' = '',
  'oath_nonce' = NA,
  'oath_signature_method' = 'HMAC-SHA1',
  'oath_timestamp' = NA,
  'oath_token' = ''
)