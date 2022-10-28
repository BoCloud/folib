FROM openjdk:11-jre
MAINTAINER folib.com
ENV TZ=Asia/Shanghai
# 项目构建文件结构 环境变量
ENV RUN_WORKDIR=/opt/folib
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN mkdir ${RUN_WORKDIR}
COPY folib-1.0-SNAPSHOT ${RUN_WORKDIR}/folib-1.0-SNAPSHOT
COPY folib-vault  ${RUN_WORKDIR}/folib-vault
RUN chmod -R 777 ${RUN_WORKDIR}
WORKDIR  ${RUN_WORKDIR}
VOLUME  ${RUN_WORKDIR}/folib-vault/
EXPOSE 38080
EXPOSE 7010
EXPOSE 7011
EXPOSE 49142
EXPOSE 8182
ENTRYPOINT  ${RUN_WORKDIR}/folib-1.0-SNAPSHOT/bin/folib console