export FLORA_DB_HOST="localhost"
export FLORA_DB_PORT="5432"
export FLORA_DB_USER="postgres"
export FLORA_DB_PASSWORD="postgres"
export FLORA_DB_DATABASE="flora_dev"
export FLORA_DB_POOL_CONNECTIONS="10"
export FLORA_DB_SUB_POOLS="10"
export FLORA_DB_TIMEOUT="10"

export FLORA_PG_URI="postgresql://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${DB_PORT}/${DB_DATABASE}"
export FLORA_PG_CONNSTRING="host=${DB_HOST} dbname=${DB_DATABASE} user=${DB_USER} password=${DB_PASSWORD}"

export FLORA_HTTP_PORT=8083
export FLORA_ENVIRONMENT="local"

# Compatibility mode for Hackage.
# This includes:
#
#   * Accept multiple packages with the same name but different case
#   * Accept multiple users with the same name but different case
export FLORA_COMPATIBILITY_MODE=true

# Set these variables in `environment.local.sh`, which is not tracked by git.
#export SENTRY_DSN="" # Set this variable to connecto to your Sentry instance
#export FLORA_PROMETHEUS_ENABLED="true" # Set this variable to true or false to enable Prometheus metrics export
