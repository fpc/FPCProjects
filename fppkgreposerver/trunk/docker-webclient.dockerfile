FROM nginx
ARG nginxconffile=nginx-default.conf

COPY webclient/dist /usr/share/nginx/html
COPY webclient/config/$nginxconffile /etc/nginx/conf.d/default.conf
