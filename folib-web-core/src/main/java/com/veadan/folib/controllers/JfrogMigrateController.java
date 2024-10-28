package com.veadan.folib.controllers;

import com.veadan.folib.forms.JfrogMigrateForm;
import com.veadan.folib.services.JfrogMigrateService;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.client.HttpResponseException;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

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
            jfrogMigrateService.migrate(artifactory, form);
            return ResponseEntity.ok("the data is syncing");
        } catch (Exception e) {
            String msg = e.getMessage();
            if (e instanceof HttpResponseException) {
                msg = "与jfrog通讯异常，请检查网络ip和端口或用户名及密码是否正确";
            }
            throw new RuntimeException(msg);
        }
    }


}
