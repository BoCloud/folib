package com.veadan.folib.services;

import com.veadan.folib.domain.debian.DebianParserVO;
import com.veadan.folib.domain.debian.DebianUploadBO;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * @author veadan
 * @since 2024-09-06 11:20
 */
public interface DebianService {

     DebianParserVO parseArtifact(String storageId, String repositoryId, MultipartFile file);

     String upload(DebianUploadBO uploadBO);

     void batchUpload(List<MultipartFile> files, String storageId, String repositoryId, String distribution, String component);
}
