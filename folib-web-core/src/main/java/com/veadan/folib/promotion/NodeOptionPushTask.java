package com.veadan.folib.promotion;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.storage.repository.Repository;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpEntity;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.HttpMultipartMode;
import org.apache.http.entity.mime.MultipartEntityBuilder;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.concurrent.Callable;

@Data
@Slf4j
public class NodeOptionPushTask implements Callable<String> {

    /**
     * 源仓库
     */
    private Repository srcRepository;

    /**
     * 目标路径
     */
    private String URl;

    /**
     * 源路径下文件
     */
    private File file;

    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    private PromotionUtil promotionUtil;

    private SecurityComponent securityComponent;

    public NodeOptionPushTask() {
    }

    public NodeOptionPushTask(Repository srcRepository, String URl, File file) {
        this.srcRepository = srcRepository;
        this.URl = URl;
        this.file = file;
        this.clientPool = SpringUtil.getBean(ProxyRepositoryConnectionPoolConfigurationService.class);
        this.promotionUtil = SpringUtil.getBean(PromotionUtil.class);
        this.securityComponent = SpringUtil.getBean(SecurityComponent.class);
    }

    @Override
    public String call() {
        String rs = "";

        Client client = null;
        Response response = null;
        try (InputStream inputStream = Files.newInputStream(file.toPath())) {
            client = clientPool.getRestClient();
            WebTarget target = client.target(getURl());
            MultipartEntityBuilder multipartEntityBuilder = MultipartEntityBuilder.create()
                    .setMode(HttpMultipartMode.BROWSER_COMPATIBLE);

            multipartEntityBuilder.setCharset(Charset.forName("UTF-8"));
            multipartEntityBuilder.addTextBody("repository", "[]");
            //如果以inputStream形式上传文件，接收文件上传的接口可能会限制content-type为二进制：ContentType.DEFAULT_BINARY，否则为ContentType.MULTIPART_FORM_DATA
            multipartEntityBuilder.addBinaryBody("file", inputStream, ContentType.DEFAULT_BINARY, file.getName());
            HttpEntity httpEntity = multipartEntityBuilder.build();
            Invocation.Builder builder = target.request();
            securityComponent.securityTokenHeader(builder);
            response = builder.put(Entity.entity(httpEntity, MediaType.APPLICATION_JSON));
            if (response.getStatus() > 210) {
                log.error("Push artifact error {}", getURl());
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }

        } catch (Exception e) {
            log.error("NodeOptionPushTask error {}: ", e.getMessage());
            rs = e.getMessage();
        } finally {
            if (null != response) {
                response.close();
            }
            if (null != client) {
                client.close();
            }
        }
        return rs;
    }

}
