package com.veadan.folib.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.io.IOException;
import java.net.URI;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.util.HashMap;
import java.util.Map;

import static com.veadan.folib.cloud.storage.s3fs.S3Factory.*;

@Configuration
public class S3CoreConfig {

    @Value("${s3fs.access.key}")
    private String accessKey;

    @Value("${s3fs.secret.key}")
    private String secretKey;


    @Bean
    public FileSystem s3FileSystem()
            throws IOException
    {

        Map<String, String> env = new HashMap<>();

//        env.put("access-key","ERevhJMgrz0JUKD8");
//        env.put("secret-key","UGo8B5wcjFasU3xKxanxxYKEyVNGxyfH");
        env.put(ACCESS_KEY, accessKey);
        env.put(SECRET_KEY, secretKey);
        env.put(REGION,"bo-nj-cn");

        return FileSystems.newFileSystem(URI.create("s3://127.0.0.1:9000/"),
                env,
                Thread.currentThread().getContextClassLoader());
    }

}
