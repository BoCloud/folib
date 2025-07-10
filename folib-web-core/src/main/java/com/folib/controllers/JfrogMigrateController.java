/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.controllers;

import com.folib.domain.migrate.AddRepositoryForm;
import com.folib.domain.migrate.ArtifactMigrateInfo;
import com.folib.entity.Dict;
import com.folib.entity.MigrateInfo;
import com.folib.forms.JfrogMigrateForm;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.services.JfrogMigrateService;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.client.HttpResponseException;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;
import java.util.Map;

/**
 * @author veadan
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

    @GetMapping("/task")
    public ResponseEntity<List<Dict>> getMigrateTask() {
        List<Dict> migrateTask = jfrogMigrateService.getMigrateTask();
        return ResponseEntity.ok(migrateTask);

    }

    @PutMapping("/task/{id}")
    public ResponseEntity<String> updateTask(@PathVariable Long id, @RequestBody ArtifactMigrateInfo info) {
        jfrogMigrateService.updateTask(id, info);
        return ResponseEntity.ok("update success");
    }

    @PostMapping("/task")
    public ResponseEntity<String> addTask(@RequestBody ArtifactMigrateInfo info) {
        jfrogMigrateService.addTask(info);
        return ResponseEntity.ok("update success");
    }

    @DeleteMapping("/task/{id}")
    public ResponseEntity<String> deleteTask(@PathVariable Long id) {
        jfrogMigrateService.deleteTask(id);
        return ResponseEntity.ok("update success");
    }

    @PostMapping("/repository/restart")
    public ResponseEntity<String> restartRepo(@RequestBody AddRepositoryForm form) {
        jfrogMigrateService.restartRepo(form.getMigrateId());
        return ResponseEntity.ok("update success");
    }


    @GetMapping("/repository")
    public TableResultResponse<MigrateInfo> getRepositoryByMigrateId(@RequestParam(name = "page", defaultValue = "1") Integer page,
                                                                     @RequestParam(name = "limit", defaultValue = "10") Integer limit,
                                                                     String migrateId, String status,String repoName) {
        return jfrogMigrateService.getRepositoryByMigrateId(page, limit, migrateId, status,repoName);
    }

    // 添加迁移仓库
    @PostMapping("/repository")
    public ResponseEntity<String> addRepos(@RequestBody AddRepositoryForm form) {
        jfrogMigrateService.addSyncRepository(form);
        return ResponseEntity.ok("Success");
    }

    @PutMapping("/repository/layout")
    public ResponseEntity<String> addRepos(@RequestBody MigrateInfo info) {
        jfrogMigrateService.changeLayout(info);
        return ResponseEntity.ok("Success");
    }


    // 将待迁移仓库迁移至迁移中
    @PostMapping("/start")
    public ResponseEntity<String> startSync(@RequestBody AddRepositoryForm form) {
        jfrogMigrateService.startMigrate(form.getMigrateId(), form.getStoreAndRepos());
        return ResponseEntity.ok("Success");
    }

    @PostMapping("/repository/pause")
    public ResponseEntity<String> pauseMigrate(@RequestBody AddRepositoryForm form) {
        jfrogMigrateService.pauseMigrate(form.getMigrateId(), form.getStoreAndRepos());
        return ResponseEntity.ok("Success");
    }

    @PostMapping("/repository/failed")
    public ResponseEntity<String> setFailed(@RequestBody List<Long> ids) {
        jfrogMigrateService.setFailed(ids);
        return ResponseEntity.ok("Success");
    }


    @PostMapping("/repository/progress")
    public ResponseEntity<Map<String, String>> getCountByRepo(@RequestBody AddRepositoryForm form) {
        Map<String, String> cnt = jfrogMigrateService.getFinishedCount(form.getMigrateId(), form.getStoreAndRepos());
        return ResponseEntity.ok(cnt);
    }

    @PostMapping("/index/progress")
    public ResponseEntity<Map<String, Long>> getIndexCountByRepo(@RequestBody AddRepositoryForm form) {
        Map<String, Long> cnt = jfrogMigrateService.getIndexCount(form.getMigrateId(), form.getStoreAndRepos());
        return ResponseEntity.ok(cnt);
    }

    @PostMapping("/repository/continue")
    public ResponseEntity<String> repoContinue(@RequestBody AddRepositoryForm form) {
        jfrogMigrateService.repoContinue(form.getMigrateId(), form.getStoreAndRepos());
        return ResponseEntity.ok("continue");
    }

    @PostMapping("/repository/finish")
    public ResponseEntity<String> repoFinish(@RequestBody AddRepositoryForm form) {
        jfrogMigrateService.repoFinish(form.getMigrateId(), form.getStoreAndRepos());
        return ResponseEntity.ok("finished");
    }

    @PostMapping("/repository/all")
    public ResponseEntity<List<String>> getAllRepo(@RequestBody AddRepositoryForm form) {
        List<String> allRepo = jfrogMigrateService.getAllRepo(form.getMigrateId());
        return ResponseEntity.ok(allRepo);
    }


}
