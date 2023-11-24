package com.veadan.folib.ws.server.timer;

import cn.hutool.core.io.FileUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.io.File;
import java.util.Date;
import java.util.function.Consumer;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/23 21:51
 * @since x.x.x
 */
@Slf4j
@Component
public class ArtifactSliceCleanTimer {

    /**
     * 切片文件缓存时间
     */
    public static final Long SLICE_FILE_CACHE_TIME = 12 * 60 * 60 * 1000L;
    
    @Value("${folib.temp}")
    private String tempPath;
    
    @Scheduled(cron = "0 0 0 * * ?")
    public void clean() {
        log.info("==================================清理制品缓存切片文件-BEGIN==================================");
        final String allArtifactFileSliceRootFolderPathStr = String.format("%s/artifactSlice/", StringUtils.chomp(tempPath, "/"));
        final StringBuilder fileCleanLog = new StringBuilder();
        final File file = new File(allArtifactFileSliceRootFolderPathStr);
        this.recursionFiles(file, (f) -> {
            final Date lastModifiedTime = FileUtil.lastModifiedTime(f);
            final long time = System.currentTimeMillis() - lastModifiedTime.getTime();
            if (time >= SLICE_FILE_CACHE_TIME) {
                try {
                    if (FileUtil.del(f)) {
                        fileCleanLog.append("清理文件成功（").append(f.getPath()).append(")\n");
                    } else {
                        fileCleanLog.append("清理文件失败（").append(f.getPath()).append(")\n");
                    }
                } catch (Exception e) {
                    fileCleanLog.append("清理文件发生错误（").append(f.getPath()).append(")，错误原因（").append(e.getMessage()).append(")\n");
                }
            }
        });

        log.info("清理文件结果：\n{}", fileCleanLog);
        log.info("==================================清理制品缓存切片文件-END==================================");
    }

    private File recursionFiles(File file, Consumer<File> handlerFile) {
        if (null != file && file.isDirectory()) {
            final File[] files = file.listFiles();
            if (null != files) {
                for (File f : files) {
                    recursionFiles(f, handlerFile);
                }
            }
        }
        
        if (null != file && file.isFile()) {
            handlerFile.accept(file); 
        }
        
        return file;
    }
    
}
