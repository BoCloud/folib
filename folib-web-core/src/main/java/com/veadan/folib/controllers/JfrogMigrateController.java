package com.veadan.folib.controllers;

import com.veadan.folib.forms.JfrogMigrateForm;
import com.veadan.folib.services.JfrogMigrateService;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.client.HttpResponseException;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author huayanjun
 * @since 2024-10-22 14:31
 */
@Slf4j
@RestController
@RequestMapping("/api/migrate/jfrog")
public class JfrogMigrateController {

    @Resource
    private JfrogMigrateService jfrogMigrateService;
    private final static String JFROG_PREFIX = "/artifactory";


    @PostMapping("")
    public ResponseEntity<String> migrate(@Validated @RequestBody JfrogMigrateForm form) {
        try (Artifactory artifactory = ArtifactoryClientBuilder.create().setUrl(form.getUrl() + JFROG_PREFIX).setUsername(form.getUsername()).setPassword(form.getPassword()).build()) {
            artifactory.security().groupNames();
            jfrogMigrateService.migrate(form);
            return ResponseEntity.ok("the data is syncing");
        } catch (Exception e) {
            String msg = e.getMessage();
            if (e instanceof HttpResponseException) {
                msg = "与jfrog通讯异常，请检查网络ip和端口或用户名及密码是否正确";
            }
            throw new RuntimeException(msg);
        }
    }


    @PostMapping("/import")
    public ResponseEntity<String> changeRepositoryType(@RequestParam("file") MultipartFile file) {
        jfrogMigrateService.changeRepositoryType(file);
        return ResponseEntity.ok("the repository is changed");
    }


    @GetMapping("/download")
    public void download(HttpServletResponse response) {
        try (InputStream inputStream = this.getClass().getResourceAsStream("/template/syncRepositoryType.xlsx");
             OutputStream outputStream = response.getOutputStream()) {
            response.setContentType("application/octet-stream");
            response.setHeader("Content-Disposition", "attachment; filename=\"syncRepositoryType.xlsx\"");
            // 将 InputStream 内容写入到 Response 的 OutputStream
            byte[] buffer = new byte[1024];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
            outputStream.flush();
        } catch (Exception e) {
            log.error("download failed",e);
            throw new RuntimeException("File download failed.");

        }
    }


}
