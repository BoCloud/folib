package com.folib.controllers.layout.debian;

import com.folib.domain.debian.DebianParserVO;
import com.folib.domain.debian.DebianUploadBO;
import com.folib.services.DebianService;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author veadan
 * @since 2024-09-06 10:36
 */


@RestController
@RequestMapping("api/debian")
public class DebianStorageController{

    @Resource
    private DebianService debianService;

    @PostMapping("parseArtifact")
    public ResponseEntity<DebianParserVO> parseArtifact(String storageId, String repositoryId, MultipartFile file) {
        return ResponseEntity.ok(debianService.parseArtifact(storageId, repositoryId, file));
    }

    @PostMapping("upload")
    public ResponseEntity<String> upload(@Validated @RequestBody DebianUploadBO uploadBO) {
        return ResponseEntity.ok(debianService.upload(uploadBO));
    }
    @PostMapping("batchUpload")
    public ResponseEntity<String> batchUpload(List<MultipartFile> files, String storageId, String repositoryId, String distribution,String component) {
        debianService.batchUpload(files,storageId,repositoryId,distribution,component);
        return ResponseEntity.ok("accepted");
    }






}
