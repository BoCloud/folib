//package com.veadan.folib.services;
//
//import com.alibaba.fastjson.JSONArray;
//import com.alibaba.fastjson.JSONObject;
//import com.github.dockerjava.api.DockerClient;
//import com.github.dockerjava.api.model.AuthConfig;
//import com.github.dockerjava.api.model.PushResponseItem;
//import com.github.dockerjava.core.command.PushImageResultCallback;
//import com.veadan.folib.scanner.common.exception.BusinessException;
//import lombok.extern.slf4j.Slf4j;
//import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
//import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
//import org.apache.commons.lang3.exception.ExceptionUtils;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.core.annotation.Order;
//import org.springframework.stereotype.Service;
//
//import javax.annotation.PostConstruct;
//import java.io.ByteArrayOutputStream;
//import java.io.InputStream;
//
//@Order(0)
//@Slf4j
//@Service
//public class DockerServiceImpl {
//
//    /**
//     * 宿主机开放docker客户端端口
//     */
//    @Value("${docker.host}")
//    private String host;
//    /**
//     * 仓库地址
//     */
//    @Value("${docker.respositryUrl}")
//    private String respositry;
//    /**
//     * 仓库用户名
//     */
//    @Value("${docker.userName}")
//    private String userName;
//
//    /**
//     * 仓库密码
//     */
//    @Value("${docker.passWord}")
//    private String passWord;
//
//    /**
//     * 仓库权限设定
//     */
//    private  AuthConfig authConfig ;
//
//
//    private DockerClient dockerClient;
//
//
//    @PostConstruct
//    public void  initDockerClient() throws Exception{
//        log.info("==========================初始化dockers客户端=====================================");
//        log.info(this.host);
//        log.info(this.respositry);
//        log.info(this.userName);
//        log.info(this.passWord);
//        try {
////            this.dockerClient = DockerClientBuilder.getInstance(this.host).build();
////            this.authConfig = new AuthConfig()
////                    .withUsername(this.userName)
////                    .withPassword(this.passWord)
////                    .withRegistryAddress(this.respositry);
//        } catch (Exception e) {
//            log.error(ExceptionUtils.getStackTrace(e));
//            throw new Exception("=====>>>>>doccker 服务器连接异常，请检查服务器以及客户端端口是否打开");
//        }
//
//    }
//
//
//
//    /**
//     * 推送镜像回调
//     */
//    private   PushImageResultCallback push = new PushImageResultCallback() {
//        @Override
//        public void onNext(PushResponseItem item) {
//            log.info("=====>>>>>推送镜像中" + JSONObject.toJSONString(item));
//            super.onNext(item);
//        }
//
//        @Override
//        public void onComplete() {
//            log.info("=====>>>>>推送镜像完成=====================");
//            super.onComplete();
//        }
//    };
//
//
//    /**
//     *
//     * @param inputStream
//     * @return
//     * @throws Exception
//     */
//    private  String getLoadTags(InputStream inputStream) throws Exception{
//        try {
//            String imgageTags="";
//            TarArchiveInputStream tin = new TarArchiveInputStream(inputStream);
//            TarArchiveEntry entry = tin.getNextTarEntry();
//            String json = null;
//            while (entry != null) {
//                // 只读取manifest.json
//                if (entry.getName().equals("manifest.json")) {
//                    ByteArrayOutputStream result = new ByteArrayOutputStream();
//                    int count;
//                    byte data[] = new byte[1024];
//                    while ((count = tin.read(data, 0, 1024)) != -1) {
//                        result.write(data, 0, count);
//                    }
//                    json = result.toString();
//                    result.close();
//                    break;
//                }
//                entry = tin.getNextTarEntry();
//            }
//            if (json == null) {
//                throw new BusinessException("错误镜像");
//            }
//            String jsonImage = JSONArray.parseArray(json).getJSONObject(0).getString("RepoTags");
//            imgageTags = JSONArray.parseArray(jsonImage).getString(0);
//
//            return imgageTags;
//        }catch (Exception e){
//            log.error(ExceptionUtils.getStackTrace(e));
//            throw new Exception();
//        }finally {
//            inputStream.close();
//        }
//
//
//    }
//
//
//    /**
//     *
//     * @param inputStream 文件流
//     * @param storageId 存储空间
//     * @param respository 仓库名称
//     * @throws Exception
//     */
//    public  void pushMirrorToLocalRes(InputStream inputStream,InputStream io, String storageId, String respository, Integer version) throws Exception {
//        String imageTags="";
//        String storagePath="";
//        try {
//            imageTags = getLoadTags(io);
//            dockerClient.loadImageCmd(inputStream).exec();
//            String tag = version+"";
//            storagePath =this.respositry + "/" + storageId + "/"   + respository + "/"   +respository;
//            dockerClient.tagImageCmd(imageTags,storagePath, tag).exec();
//            dockerClient.pushImageCmd(storagePath + ":" + tag).withAuthConfig(authConfig).exec(push).awaitSuccess();
//        }catch (Exception e){
//            log.error(ExceptionUtils.getStackTrace(e));
//            throw  new Exception("=====>>>>>制品同步失败"+e.getMessage());
//        }
//
//    }
//
//
//}