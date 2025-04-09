FROM nginx:1.14-alpine
MAINTAINER folib.com
ENV agent.service_name=dp-ui
ENV TZ=Asia/Shanghai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
WORKDIR /
ADD dist.tar /
RUN chmod -R 777 dist/
ADD devops-ui.conf  /etc/nginx/conf.d/
EXPOSE 9529
