package com.folib.services;

import com.folib.domain.debian.DebianParserVO;
import com.folib.domain.debian.DebianUploadBO;
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
