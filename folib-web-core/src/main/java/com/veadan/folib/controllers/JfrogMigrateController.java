package com.veadan.folib.controllers;

import com.veadan.folib.components.jfrogArtifactSync.ArtifactDownloader;
import com.veadan.folib.components.jfrogArtifactSync.JfrogPropertySyncer;
import com.veadan.folib.domain.migrate.AddRepositoryForm;
import com.veadan.folib.domain.migrate.ArtifactMigrateInfo;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.MigrateInfo;
import com.veadan.folib.forms.JfrogMigrateForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.JfrogMigrateService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.client.HttpResponseException;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StopWatch;
import org.springframework.validation.annotation.Validated;
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
    public ResponseEntity<String> AddTask(@RequestBody ArtifactMigrateInfo info) {
        jfrogMigrateService.addTask(info);
        return ResponseEntity.ok("update success");
    }

    @GetMapping("/repository")
    public TableResultResponse<MigrateInfo> getRepositoryByMigrateId(@RequestParam(name = "page", defaultValue = "1") Integer page,
                                                                     @RequestParam(name = "limit", defaultValue = "10") Integer limit,
                                                                     String migrateId, String status) {
        return jfrogMigrateService.getRepositoryByMigrateId(page, limit, migrateId, status);


    }

    // 添加迁移仓库
    @PostMapping("/repository")
    public ResponseEntity<String> addRepos(@RequestBody AddRepositoryForm form) {
        jfrogMigrateService.addSyncRepository(form);
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


    @PostMapping("/repository/progress")
    public ResponseEntity<Map<String, Long>> getCountByRepo(@RequestBody AddRepositoryForm form) {
        Map<String, Long> cnt = jfrogMigrateService.getFinishedCount(form.getMigrateId(),form.getStoreAndRepos());
        return ResponseEntity.ok(cnt);
    }

    @PostMapping("/repository/continue")
    public ResponseEntity<String> repoContinue(@RequestBody AddRepositoryForm form) {
        jfrogMigrateService.repoContinue(form.getMigrateId(),form.getStoreAndRepos());
        return ResponseEntity.ok("continue");
    }

    @PostMapping("/repository/finish")
    public ResponseEntity<String> repoFinish(@RequestBody AddRepositoryForm form) {
        jfrogMigrateService.repoFinish(form.getMigrateId(),form.getStoreAndRepos());
        return ResponseEntity.ok("finished");
    }

    @GetMapping("/test")
    public Object downLoadTest(String name,String path) {
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();
//        ArtifactDownloader artifactDownload = new ArtifactDownloader("http://10.10.33.149:8082/artifactory/", "admin", "folib@v587", "/", 5);
//        List<String> allArtifacts = artifactDownload.getAllArtifacts(name);
//        log.info(allArtifacts.size() + "");
//        stopWatch.stop();
//        return String.valueOf(stopWatch.getTotalTimeSeconds());
        JfrogPropertySyncer jfrogPropertySyncer=new JfrogPropertySyncer("http://10.10.33.149:8082/artifactory/", "admin", "folib@v587");
        return jfrogPropertySyncer.getPropertiesByKeyAndPath(name,path);


    }

}
