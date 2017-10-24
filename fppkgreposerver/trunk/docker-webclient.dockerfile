FROM nginx
ARG nginxconffile=nginx-default.conf

COPY webclient/dist /usr/share/nginx/html
COPY webclient/config/$nginxconffile /etc/nginx/conf.d/default.conf

COPY readme.html /usr/share/nginx/html
COPY buildagent/readme.html /usr/share/nginx/html/buildagent_readme.html
COPY identityserver/readme.html /usr/share/nginx/html/identityserver_readme.html
COPY webclient/readme.html /usr/share/nginx/html/webclient_readme.html


