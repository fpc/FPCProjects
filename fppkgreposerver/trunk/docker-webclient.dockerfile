FROM nginx
ARG nginxconffile=nginx-default.conf

COPY webclient/dist /usr/share/nginx/html
COPY webclient/config/nginx-default.conf.template /etc/nginx/conf.d/default.conf.template

COPY readme.html /usr/share/nginx/html
COPY buildagent/readme.html /usr/share/nginx/html/buildagent_readme.html
COPY identityserver/readme.html /usr/share/nginx/html/identityserver_readme.html
COPY webclient/readme.html /usr/share/nginx/html/webclient_readme.html

CMD ["/bin/bash", "-c", "envsubst '${HOST}${Buildagent_304_HOST}${Buildagent_trunk_HOST}${Buildagent_fixes32_HOST}${Buildmanager_HOST}${Fppkgrepository_HOST}${Identityserver_HOST}${Repository_HOST}${Packagemanager_HOST}' < /etc/nginx/conf.d/default.conf.template > /etc/nginx/conf.d/default.conf && nginx -g 'daemon off;'"]