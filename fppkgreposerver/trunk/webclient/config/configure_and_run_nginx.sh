#!/bin/bash
envsubst '${HOST}${Buildagent_304_HOST}${Buildagent_320_HOST}${Buildagent_trunk_HOST}${Buildagent_fixes32_HOST}  ${Buildmanager_HOST}${Fppkgrepository_HOST}${Identityserver_HOST}${Repository_HOST}${Packagemanager_HOST}' < /etc/nginx/conf.d/default.conf.template > /etc/nginx/conf.d/default.conf
if [ $redirect_https == "no" ]
then
  sed -i '57,59d' /etc/nginx/conf.d/default.conf
fi

envsubst '${IdentityServerUrl}${WebClientUrl}${BuildAgentUrl}${PackageManagerUrl}${CategoryUrl}${BuildManagerUrl}${RepositoryUrl}${FppkgRepositoryUrl}' < /usr/share/nginx/html/assets/auth.clientConfiguration.json.template > /usr/share/nginx/html/assets/auth.clientConfiguration.json
envsubst '${IdentityServerUrl}${WebClientUrl}${BuildAgentUrl}${PackageManagerUrl}${CategoryUrl}${BuildManagerUrl}${RepositoryUrl}${FppkgRepositoryUrl}' < /usr/share/nginx/html/assets/config.json.template > /usr/share/nginx/html/assets/config.json

nginx -g 'daemon off;'