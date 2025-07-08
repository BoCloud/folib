package com.folib.forms;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;

/**
 * @author veadan
 * @date 2022-11-18
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UploadArtifactFrom {
    private MultipartFile[] files;
    private String storageId;
    private String repostoryId;
    private String filePathMap;
    private String fileMetaDataMap;
    private File file;
}
